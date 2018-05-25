LOCAL INCLUDE 'WETHR.INC'
C                                       Local include for WETHR
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXPTS, NOPTS
      PARAMETER (MAXPTS = 100000)
      PARAMETER (NOPTS = 16)
C                                       Input parameters
      REAL      XSIN, XDISIN, XNVER, XTIME(8), XANT(50), XSUBA, PIXR(2),
     *   XNCOU, XXINC, XSMOTH(3), DWT, APARM(10), XFGVER, XFGOUT,
     *   BPARM(10), DETIME, FACTOR, XLABEL, XDOTV, XGRCH
      HOLLERITH XNAMEI(3), XCLAIN(2), XOPTY
      CHARACTER NAMEIN*12, CLAIN*6, OPTYPE*4
C                                       Program info
C
      REAL      TSTART, TSTOP, XYSCL(2), XYOFF(2), YYMX(NOPTS),
     *   YYMN(NOPTS), XMX, XMN, XSTART, XSTOP, CHOUT(4),
     *   XXMIN(MAXANT+1), XXMAX(MAXANT+1), YYMIN(NOPTS,MAXANT+1),
     *   YYMAX(NOPTS, MAXANT+1), UBUFF(16384), XPTS(MAXPTS),
     *   YPTS(NOPTS,MAXPTS)
      INTEGER   SEQIN, DISKIN, CNOIN, IVER, ANTS(50), NCOUNT, ICODE,
     *   NPARMS, NANTSL, NPLOTS, XINC, GRCHN, TVCHN, TVCORN(4), ITPLOT,
     *   ITVER, LABEL, MUMANT, NUMPTS(MAXANT+1), NUMTOT, UBUFSZ, NPR,
     *   PRLIST(10), ISMOTH, ISUBA
      LOGICAL   DOAWNT, DOTV, DOLINE
C                                       WX info
      INCLUDE 'INCS:DWXV.INC'
      INTEGER   WXBUFF(512), WXKOLS(MAXWXC), WXNUMV(MAXWXC), IWXRNO,
     *   TABVER, NWXINR, WXRECI(MXSPWX)
      REAL      WXRECR(MXSPWX)
      DOUBLE PRECISION WXRECD(MXSPWX/2)
      CHARACTER OBSCOD*8, OBSDAT*8
      EQUIVALENCE (WXRECR, WXRECI, WXRECD)
C                                       Constants
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNVER, XTIME, XANT,
     *   XSUBA, PIXR, XNCOU, XXINC, XSMOTH, XOPTY, DWT, APARM, XFGVER,
     *   XFGOUT, BPARM, DETIME, FACTOR, XLABEL, XDOTV, XGRCH,
     *   SEQIN, DISKIN, CNOIN, IVER, ANTS, NCOUNT, ICODE, NPARMS, GRCHN,
     *   TVCHN, TVCORN, ITPLOT, ITVER, DOTV, LABEL, CHOUT
      COMMON /VGNCOM/ UBUFF, XPTS, YPTS, TSTART, TSTOP, XYSCL, XYOFF,
     *   XMX, XMN, XSTART, XSTOP, NANTSL, NPLOTS, DOAWNT, XINC, MUMANT,
     *   NUMPTS, XXMIN, XXMAX, YYMIN, YYMAX, YYMX, YYMN , NUMTOT,
     *   UBUFSZ, NPR, PRLIST, ISMOTH, ISUBA, DOLINE
      COMMON /VGNCHR/ NAMEIN, CLAIN, OPTYPE, OBSCOD, OBSDAT
      COMMON /WCDATA/ WXBUFF, WXRECR, WXKOLS, WXNUMV, IWXRNO,TABVER,
     *   NWXINR
C                                                          End WETHR
LOCAL END
      PROGRAM WETHR
C-----------------------------------------------------------------------
C! Plots data from a WX
C# UV Plot EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2002-2004, 2007-2008, 2010-2012, 2014-2017
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
C   WETHR plots WX extension files. A 'PL' extension file is made
C   which can be displayed in the usual ways .
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      INVERS.....Version number of table to plot, 0=>highest no.
C      TIMERANG...Time range of the data to be plotted. In order:
C                 Start day, hour, min. sec,
C                 end day, hour, min. sec. Days relative to ref.
C                 date.
C      ANTENNAS...A list of the antennas to be plotted. All 0 => all.
C                 If any number is negative then all antennas listed
C                 are NOT to be plotted and all others are.
C      PIXRANGE...Limit the plot to values between PIXR(1) and
C                 PIXR(2).  The plots will not exceed the min/max in
C                 the actual gains.  Basically, if PIXR(1) < PIXR(2),
C                 all plots will be on the same scale and be limited
C                 to max (datamin, PIXR(1)) through min (datamax,
C                 PIXR(2)).  If PIXR(1) >= PIXR(2), each plot will be
C                 self-scaled individually.
C      NCOUNT.....Number of antennas to plot per page (try 5).
C      OPTYPE.....Data to be plotted:'TEMP', 'PRES', 'DEWP', 'WVEL',
C                 'WDIR', 'H2OC', 'ELEC', 'RHUM', 'AHUM'
C      XTYPE......Variable data to be plotted against,
C                 1 = IAT time; 2 = elevation; 3 = HA, 4 = LST
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
      INCLUDE 'WETHR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGN /'WETHR '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      UBUFSZ = 16384 * 2
      CALL WETIN (PRGN, IRET)
C                                       Fetch data, determine scaling
      IF (IRET.EQ.0) CALL WETMAX (IRET)
C                                       Smooth and re-scale data
      IF ((IRET.EQ.0) .AND. (ISMOTH.GT.0)) CALL WETSMO (IRET)
C                                       Do plots
      IF (IRET.EQ.0) THEN
         IF (OPTYPE.EQ.'FLAG') THEN
            CALL WETFLG (IRET)
         ELSE
            CALL WETLOT (IRET)
            END IF
         END IF
      IRET = MAX (0, IRET)
C                                       Close down
      CALL DIE (IRET, WXBUFF)
C
 999  STOP
      END
      SUBROUTINE WETIN (PRGN, IERR)
C-----------------------------------------------------------------------
C   Gets the inputs parameters for WETHR.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IERR    I    Error code: 0 => ok
C      ICODE   I    1='TEMP', 2='PRES', 3='DEWP', 4='WVEL', 5='WDIR',
C                   6='WGUS', 7='PREC', 8='H2OC', 9='IONC',
c                   10='RHUM', 11='CH2O', 12='KZOP', 13='QZOP',
C                   14='WCOS', 15='WSIN', 16='DDEP'
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'WETHR.INC'
C
      CHARACTER STAT*4, PRGN*6, CODE(NOPTS)*4, TYPTMP*2
      INTEGER   IRET, BUFF(256), I, J, K, JERR, BUFFER(512), IROUND,
     *   LTYPE
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA CODE /'TEMP', 'PRES', 'DEWP', 'WVEL', 'WDIR', 'WGUS', 'PREC',
     *   'H2OC', 'IONC', 'RHUM', 'CH2O', 'KZOP', 'QZOP', 'WCOS', 'WSIN',
     *   'DDEP'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NPARMS = 103
C                                        Get input parameters.
      CALL SETUP (PRGN, NPARMS, XNAMEI, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRET = 8
         RQUICK = .FALSE.
         GO TO 990
         END IF
C                                       Decode inputs.
C                                       characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XOPTY, OPTYPE)
      CALL FILL (MAXANT+1, 0, NUMPTS)
      NUMTOT = 0
C                                       Integers
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      IVER = IROUND (XNVER)
      NCOUNT = IROUND (XNCOU)
      IF (NCOUNT.LE.0) NCOUNT = 5
      XNCOU = NCOUNT
      XINC = IROUND (XXINC)
      IF (XINC.LE.0) XINC = 1
      XXINC = XINC
      ISUBA = IROUND (XSUBA)
      DOLINE = FACTOR.LT.0.0
      FACTOR = ABS (FACTOR)
      IF ((.NOT.DOLINE) .AND. (FACTOR.LT.0.1)) FACTOR = 1.0
      IF (FACTOR.GT.10.0) FACTOR = 1.0
C                                       plot types
      IF ((OPTYPE.EQ.'MULT') .OR. (OPTYPE.EQ.'FLAG')) THEN
         NPR = 11
         DO 25 I = 1,10
            PRLIST(I) = IROUND (APARM(I))
            IF (PRLIST(I).LE.0) NPR = MIN (NPR, I)
 25         CONTINUE
         NPR = NPR - 1
         IF (NPR.LT.1) THEN
            NPR = 1
            PRLIST(1) = 1
            APARM(1) = 1
            END IF
      ELSE
         NPR = 1
         ICODE = 1
         DO 30 I = 1,NOPTS
            IF (OPTYPE.EQ.CODE(I)) ICODE = I
 30         CONTINUE
         CALL CHR2H (4, CODE(ICODE), 1, XOPTY)
         PRLIST(1) = ICODE
         END IF
C                                       Time range
      TSTART = XTIME(1) + (XTIME(2) / 24.0) + (XTIME(3) / (24.0*60.0)) +
     *   (XTIME(4) / (24.0*3600.0))
      TSTOP = XTIME(5) + (XTIME(6) / 24.0) + (XTIME(7) / (24.0*60.0)) +
     *   (XTIME(8) / (24.0*3600.0))
      IF (TSTART.GE.TSTOP) THEN
         TSTART = 0.0
         TSTOP = 999.0
         END IF
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      LABEL = IROUND (XLABEL)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (LTYPE.GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.LT.0) THEN
         LABEL = (LABEL/100)*100 - LTYPE
      ELSE
         LABEL = (LABEL/100)*100 + LTYPE
         END IF
      XLABEL = LABEL
C                                       smoothing
      ISMOTH = IROUND (XSMOTH(1))
      IF (XSMOTH(2).LE.0.0) ISMOTH = 0
      IF (ISMOTH.GT.4) ISMOTH = ISMOTH - 4
      IF ((ISMOTH.LE.0) .OR. (ISMOTH.GT.4)) ISMOTH = 0
      IF ((ISMOTH.GT.0) .AND. (XSMOTH(3).LT.XSMOTH(2))) THEN
         XSMOTH(3) = XSMOTH(2)
         IF (ISMOTH.EQ.2) XSMOTH(3) = XSMOTH(2) * 3.0
         IF (ISMOTH.EQ.4) XSMOTH(3) = XSMOTH(2) * 4.0
         END IF
C                                       Find input catalog
      CNOIN = 1
      TYPTMP = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, TYPTMP,
     *   NLUSER, STAT, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      'UV', NLUSER
         GO TO 990
         END IF
C                                       Save name class etc.
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      XDISIN = DISKIN
      XSIN = SEQIN
C                                       Read catalog header
      STAT = 'WRIT'
      IF ((DOTV) .AND. (OPTYPE.NE.'FLAG')) STAT = 'READ'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STAT, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CNOIN
      FVOL(NCFILE) = DISKIN
      FRW(NCFILE) = 1
      IF (DOTV) FRW(NCFILE) = 0
      XDISIN = DISKIN
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
      SEQIN = CATBLK(KIIMS)
      XSIN = SEQIN
C                                       Check antennas desired.
      NANTSL = 0
      DOAWNT = T
      DO 70 J = 1,50
         ANTS(J) = IROUND (XANT(J))
         IF (ANTS(J).LT.0) DOAWNT = F
C                                       Make positive
         ANTS(J) = ABS (ANTS(J))
         IF (NANTSL.LT.1) GO TO 60
            DO 50 K = 1,NANTSL
               IF (ANTS(J).EQ.ANTS(K)) ANTS(J) = 0
 50            CONTINUE
C                                       Check for multiple entries
 60      IF (ABS (ANTS(J)).GE.1) NANTSL = J
 70      CONTINUE
C                                       Make sure not too many
      IF (NANTSL.GT.MAXANT) NANTSL = MAXANT
C                                       Get antenna names
      CALL GETANT (DISKIN, CNOIN, 1, CATBLK, BUFFER, JERR)
      MUMANT = NSTNS + 1
      IF (MUMANT.LE.1) THEN
         MUMANT = MAXANT
         TIMLAB = 'IAT'
         END IF
C                                       Open table to check
      CALL WETOPN (IERR)
      IF (IERR.NE.0) GO TO 999
      XNVER = IVER
      I = MUMANT
      IF ((TSTART.GT.0.0) .AND. (TSTOP.LT.999.0)) THEN
         CALL RFILL (I, TSTART, XXMIN)
         CALL RFILL (I, TSTOP, XXMAX)
      ELSE
         CALL RFILL (I, 1.E5, XXMIN)
         CALL RFILL (I, -1.E5, XXMAX)
         END IF
      I = MUMANT * NOPTS
      CALL RFILL (I, 1.E8, YYMIN)
      CALL RFILL (I, -1.E8, YYMAX)
      CALL RFILL (NOPTS, -1.E8, YYMX)
      CALL RFILL (NOPTS, 1.E8, YYMN)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR;',I7,'GETTING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' TYPE=',A2,' USER=',I4)
 1040 FORMAT ('ERROR',I3,' COPYING CATALOG HEADER')
      END
      SUBROUTINE WETOPN (IERR)
C-----------------------------------------------------------------------
C   Routine to open WX table and get necessary information
C   Input from Common:
C      DISKIN   I     Disk number
C      CNOIN    I     Catalog slot number
C      CATBLK   I(*)  Catalog header
C   Output:
C      IERR     I     Error code, 0=OK else failed.
C   Output in common:
C      ICLRNO       I    Current cal record number
C      NCLINR       I    Number of gain records in file.
C      NUMANT       I    Number of antennas
C      ITVER        I    Version number opened.
C      KOLS         I(*) Column pointers
C      KOLTYP       I(*) Column data types
C      KOLDIM       I(*) Column dimension
C-----------------------------------------------------------------------
      INTEGER   IERR
      INCLUDE 'WETHR.INC'
C
      INTEGER   NKEY, NREC, NCOL, WXLUN, KOLS(2), KEY(2,2), JERR,
     *   KEYSUB(2,2)
      REAL      FKEY(2,2)
      CHARACTER TYPE*2, KEYS(2)*8
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA TYPE /'WX'/
      DATA KEYS /'TIME','ANTENNA'/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Open table
      WXLUN = 28
      NKEY = 0
      NREC = 0
      NCOL = 0
      IWXRNO = 1
      CALL WXINI ('READ', WXBUFF, DISKIN, CNOIN, IVER, CATBLK, WXLUN,
     *   IWXRNO, WXKOLS, WXNUMV, OBSCOD, OBSDAT, TABVER, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1100) IERR, TYPE, IVER
         GO TO 980
         END IF
      ITVER = IVER
C                                       Get number of scans
      NWXINR = WXBUFF(5)
C                                       Check if empty
      IF (NWXINR.LE.0) THEN
         IERR = 6
         MSGTXT = 'ERROR: SELECTED TABLE IS EMPTY'
         GO TO 980
         END IF
C                                       Check sort order
      CALL FNDCOL (2, KEYS, 7, .TRUE., WXBUFF, KOLS, JERR)
      IF ((JERR.EQ.0) .AND. ((WXBUFF(43).NE.KOLS(2)) .OR.
     *   (WXBUFF(44).NE.KOLS(1)))) THEN
C                                       Close table
         CALL TABIO ('CLOS', 0, IWXRNO, WXBUFF, WXBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         KEY(1,1) = KOLS(2)
         KEY(2,1) = 0
         KEY(1,2) = KOLS(1)
         KEY(2,2) = 0
C                                       Sort
         CALL TABSRT (DISKIN, CNOIN, TYPE, IVER, IVER, KEY, KEYSUB,
     *      FKEY, WXBUFF, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Reopen table
         IWXRNO = 1
         CALL WXINI ('READ', WXBUFF, DISKIN, CNOIN, IVER, CATBLK, WXLUN,
     *      IWXRNO, WXKOLS, WXNUMV, OBSCOD, OBSDAT, TABVER, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1100) IERR, TYPE, IVER
            GO TO 980
            END IF
         END IF
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR ',I3,' OPENING ',A,' TABLE NO. ',I3)
      END
      SUBROUTINE WETMAX (IERR)
C-----------------------------------------------------------------------
C   WETMAX reads the WX table to find the max and min values.
C   Input/Output in common:
C      TSTART   R      Start time of plot
C      TSTOP    R      Stop time of plot
C   Output:
C      IERR     I      Error code, 0=OK else failed
C   Outputs in common:
C      XPTS     R(*)   X values to be plotted
C      YPTS     R(*)   Y values to be plotted
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      LOGICAL   NODATA, TOMANY, OKAY, DOUBLE
      INTEGER   I, NP, IANT, IPR, JSUB
      REAL      TB, TE, TMAX, TMIN, GTIME
      INCLUDE 'WETHR.INC'
      REAL      VALUE(NOPTS)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      DOUBLE = WXKOLS(2)-WXKOLS(1).EQ.2
      NODATA = .TRUE.
      TOMANY = .FALSE.
      IF ((TSTART.GT.0.0) .AND. (TSTOP.LT.999.0)) THEN
         TB = TSTART
         TE = TSTOP
      ELSE
         TB = 1.0E5
         TE = -1.0E5
         END IF
      XMX = TE
      XMN = TB
C                                       Loop thru data
      NP = 1
      JSUB = 0
      DO 100 IWXRNO = 1,NWXINR,XINC
         CALL TABIO ('READ', 0, IWXRNO, WXRECR, WXBUFF, IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Record within specified
C                                       time range ?
         IF (DOUBLE) THEN
            GTIME = WXRECD(WXKOLS(WXDTIM))
         ELSE
            GTIME = WXRECR(WXKOLS(WXDTIM))
            END IF
         IF ((GTIME.LT.TSTART) .OR. (GTIME.GT.TSTOP)) GO TO 100
C                                       subarray
         IF (WXKOLS(WXSUBA).GT.0) THEN
            JSUB = WXRECI(WXKOLS(WXSUBA))
            IF ((ISUBA.NE.JSUB) .AND. (ISUBA.NE.0) .AND. (JSUB.NE.0))
     *         GO TO 100
            END IF
C                                       Antenna?
         IANT = WXRECI(WXKOLS(WXIANT))
         IF ((IANT.GT.0) .AND. (NANTSL.GT.0)) THEN
            DO 50 I = 1,NANTSL
               IF ((IANT.EQ.ANTS(I)).AND.DOAWNT) GO TO 60
               IF ((IANT.EQ.ANTS(I)).AND.(.NOT.DOAWNT)) GO TO 100
 50            CONTINUE
            IF (DOAWNT) GO TO 100
            END IF
C                                      Get start, stop times
 60      TB = MIN (TB, GTIME)
         TE = MAX (TE, GTIME)
         IANT = MAX (1, IANT+1)
C                                       Get value
         CALL WETDAT (VALUE, OKAY)
C                                       Max. - Min
         IF (OKAY) THEN
            NODATA = .FALSE.
C                                       Put in array
            IF (NUMTOT.GE.MAXPTS) THEN
               TOMANY = .TRUE.
            ELSE
               NUMTOT = NUMTOT + 1
               NUMPTS(IANT) = NUMPTS(IANT) + 1
               XPTS(NUMTOT) = GTIME
               CALL RCOPY (NPR, VALUE, YPTS(1,NUMTOT))
               XMX = MAX (XMX, GTIME)
               XMN = MIN (XMN, GTIME)
               XXMAX(IANT) = MAX (XXMAX(IANT), GTIME)
               XXMIN(IANT) = MIN (XXMIN(IANT), GTIME)
C                                       If not a summary plot
               DO 70 IPR = 1,NPR
                  IF (VALUE(IPR).NE.FBLANK) THEN
                     YYMIN(IPR,IANT) = MIN (VALUE(IPR), YYMIN(IPR,IANT))
                     YYMAX(IPR,IANT) = MAX (VALUE(IPR), YYMAX(IPR,IANT))
                     YYMX(IPR) = MAX (YYMX(IPR), VALUE(IPR))
                     YYMN(IPR) = MIN (YYMN(IPR), VALUE(IPR))
                     END IF
 70               CONTINUE
               END IF
            END IF
 100     CONTINUE
      CALL TABIO ('CLOS', 0, IWXRNO, WXBUFF, WXBUFF, IERR)
C                                       reset max min on fixed scale
      IF ((PIXR(1).LT.PIXR(2)) .AND. (NPR.EQ.1)) THEN
         YYMX(1) = PIXR(2)
         YYMN(1) = PIXR(1)
         DO 120 IANT = 1,MUMANT
            IF (YYMAX(1,IANT).GE.YYMIN(1,IANT)) THEN
               YYMAX(1,IANT) = PIXR(2)
               YYMIN(1,IANT) = PIXR(1)
               END IF
 120        CONTINUE
         END IF
C                                       Set actual X range
      XSTART = TB
      XSTOP = TE
C                                       Check for no data
      IF (NODATA) THEN
         IERR = 6
         MSGTXT = 'NO DATA SELECTED'
         GO TO 990
         END IF
C                                       check and set scaling
      IF ((TSTART.GT.0.0) .AND. (TSTOP.LT.999.0)) THEN
         TMAX = (XSTOP + 0.03 * (XSTOP - XSTART)) * 360.0
         TMIN = (XSTART- 0.03 * (XSTOP - XSTART)) * 360.0
      ELSE
         TMAX = (XSTOP + 0.1 * (XSTOP - XSTART)) * 360.0
         TMIN = (XSTART- 0.1 * (XSTOP - XSTART)) * 360.0
         END IF
C                                       If start time is stop time,
      IF (ABS (TMAX-TMIN) .LT. 0.01) THEN
         TMIN = MAX( TMIN-0.005, 0.0)
         TMAX = TMIN + 0.01
         END IF
      XYOFF(1) = TMIN
      XYSCL(1) = 1000.0 / (TMAX - TMIN)
C                                       Send back time range
      TSTART = TB
      TSTOP = TE
      XTIME(1) = TSTART
      XTIME(2) = 0.0
      XTIME(3) = 0.0
      XTIME(4) = 0.0
      XTIME(5) = TSTOP
      XTIME(6) = 0.0
      XTIME(7) = 0.0
      XTIME(8) = 0.0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('WETMAX: ERROR =',I3,' FROM TABIO')
      END
      SUBROUTINE WETSMO (IRET)
C-----------------------------------------------------------------------
C   Smooths the data in time and recomputes the max/min
C   In/Out in common:
C      YYMIN    R(*,*)   Min of Y value by type and antenna
C      YYMAX    R(*,*)   Max of Y value by type and antenna
C      YYMN     R(*)     Min of Y value by type only
C      YYMX     R(*)     Max of Y value by type only
C      YPTS     R(*,*)   Y data by type antenna
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'WETHR.INC'
C
      INTEGER   IANT, ITYP, IP, MP
C-----------------------------------------------------------------------
      IRET = 0
      CALL RFILL (NOPTS, -1.E8, YYMX)
      CALL RFILL (NOPTS, 1.E8, YYMN)
C                                       loop over data parts
      MP = 0
      DO 100 IANT = 1,MUMANT
         IP = MP + 1
         MP = IP + NUMPTS(IANT) - 1
         IF (MP.GE.IP) THEN
            DO 95 ITYP = 1,NPR
               IF (YYMAX(ITYP,IANT).GT.YYMIN(ITYP,IANT)) CALL SMOWX
     *            (ITYP, IP, MP, IANT)
 95            CONTINUE
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SMOWX (ITYP, IP, MP, IANT)
C-----------------------------------------------------------------------
C   Smooths a portion of data
C   Inputs:
C      ITYP   I   Type of data - seq #
C      IP     I   Start array index
C      MP     I   End array index
C      IANT   I   Antenna number
C-----------------------------------------------------------------------
      INTEGER   ITYP, IP, MP, IANT
C
      INCLUDE 'WETHR.INC'
C
      INTEGER   I, J
      REAL      X, V, W, WT, GF, XRAD, XW, A, TT(MAXPTS), SC, SS
      LOGICAL   DOVECT
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      YYMAX(ITYP,IANT) = -1.E8
      YYMIN(ITYP,IANT) = 1.E8
      XRAD = XSMOTH(3) / 2 / (60.0 * 24.0)
      XW = XSMOTH(2) / 2 / (60.0 * 24.0)
      IF (ISMOTH.EQ.2) GF = -LOG(2.0) / (XW**2)
      IF (ISMOTH.EQ.4) GF =  PI / XW
      DOVECT = PRLIST(ITYP).EQ.5
C                                       Outer loop of points
      DO 100 I = IP,MP
         V = 0.0
         W = 0.0
         SC = 0.0
         SS = 0.0
         DO 90 J = IP,MP
            X = ABS (XPTS(I) - XPTS(J))
            IF ((X.LT.XRAD) .AND. (YPTS(ITYP,I).NE.FBLANK)) THEN
C                                       Hanning
               IF (ISMOTH.EQ.1) THEN
                  WT = 1.0  - X/XW
                  WT = MAX (0.0, WT)
C                                       Gaussian
               ELSE IF (ISMOTH.EQ.2) THEN
                  WT = EXP (GF * X * X)
C                                       boxcar
               ELSE IF (ISMOTH.EQ.3) THEN
                  WT = 1.0
C                                       sinc function
               ELSE IF (ISMOTH.EQ.4) THEN
                  A = X * GF
                  IF (A.EQ.0.0) THEN
                     WT = 1.0
                  ELSE
                     WT = SIN (A) / A
                     END IF
                  END IF
               W = W + WT
               IF (DOVECT) THEN
                  SC = SC + WT * COS (DG2RAD * YPTS(ITYP,J))
                  SS = SS + WT * SIN (DG2RAD * YPTS(ITYP,J))
               ELSE
                  V = V + YPTS(ITYP,J) * WT
                  END IF
               END IF
 90         CONTINUE
         IF (W.GT.0.0) THEN
            IF (DOVECT) THEN
               IF ((SC.EQ.0.0) .AND. (SS.EQ.0.0)) THEN
                  TT(I) = FBLANK
               ELSE
                  TT(I) = RAD2DG * ATAN2 (SS, SC)
                  IF (TT(I).LT.0.0) TT(I) = TT(I) + 360.0
                  END IF
            ELSE
               TT(I) = V / W
               END IF
         ELSE
            TT(I) = FBLANK
            END IF
 100     CONTINUE
C                                       move back and max/min
      DO 120 I = IP,MP
         YPTS(ITYP,I) = TT(I)
         IF (TT(I).NE.FBLANK) THEN
            YYMIN(ITYP,IANT) = MIN (TT(I), YYMIN(ITYP,IANT))
            YYMAX(ITYP,IANT) = MAX (TT(I), YYMAX(ITYP,IANT))
            YYMX(ITYP) = MAX (YYMX(ITYP), TT(I))
            YYMN(ITYP) = MIN (YYMN(ITYP), TT(I))
            END IF
 120     CONTINUE
C                                       reset max min on fixed scale
      IF ((PIXR(1).LT.PIXR(2)) .AND. (NPR.EQ.1)) THEN
         YYMX(1) = PIXR(2)
         YYMN(1) = PIXR(1)
         IF (YYMAX(1,IANT).GE.YYMIN(1,IANT)) THEN
            YYMAX(1,IANT) = PIXR(2)
            YYMIN(1,IANT) = PIXR(1)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE GETSCL (LANT, LPLT, DOIT)
C-----------------------------------------------------------------------
C   GETSCL converts a number of max/min's to a scale
C   Inputs:
C      LANT    I   Antenna number
C      LPLT    I   Plot number
C   Output:
C      DOIT    L      There were valid values
C   Output in common
C      XYSCL   R(2)   Scaling - only 2nd one changed
C      XYOFF   R(2)   Offset  - only second one changed
C-----------------------------------------------------------------------
      INTEGER   LANT, LPLT
      LOGICAL   DOIT
C
      INCLUDE 'WETHR.INC'
      REAL      YMX, YMN, TMAX, TMIN, TDIF, TOLER(NOPTS), SIZEY
C                                       Minimum value range for each
C                                       ICODE
C                  T    P  Dewp  Vel  Dir  Gus  Pre  H2O  Elec  Hum
      DATA TOLER /0.1, 0.1, 0.1,  1.,  1.,  1., 0.1, 0.1,  1.0,  1.,
C        CH2O,   opac    opac Wcos Wsin dDEP
     *    0.1, 0.0001, 0.0001, 1.0, 1.0, 0.1/
C-----------------------------------------------------------------------
      DOIT = .FALSE.
      YMX = -1.E8
      YMN = -YMX
      IF (YYMAX(LPLT,LANT).GT.YYMIN(LPLT,LANT)) THEN
         DOIT = .TRUE.
         YMX = MAX (YMX, YYMAX(LPLT,LANT))
         YMN = MIN (YMN, YYMIN(LPLT,LANT))
         END IF
      SIZEY = 1000.0 / NCOUNT
      TMAX = YMX + 0.1 * (YMX - YMN)
      TMIN = YMN - 0.1 * (YMX - YMN)
      IF (ABS (TMAX-TMIN) .LT. TOLER(ICODE)) THEN
         TMAX = TMAX + TOLER(ICODE)
         TMIN = TMIN - TOLER(ICODE)
         END IF
      TDIF = TMAX - TMIN
      IF (ABS (TDIF).LE.1.0E-25) TDIF = 1.0E-25
      XYOFF(2) = TMIN
      XYSCL(2) = 1000.0 / TDIF / NCOUNT
C
 999  RETURN
      END
      SUBROUTINE WETDAT (VALUE, OKAY)
C-----------------------------------------------------------------------
C   Routine to get the specified value from a WX entry
C   Input from common:
C      WXREC    R(*)  Table record
C   Also uses pointers etc. set in WETOPN
C   Output:
C      VALUE    R(*)   Table value, magic value blanked (amp on ICODE 6)
C      OKAY     L      Some values are good
C-----------------------------------------------------------------------
      REAL     VALUE(10)
      LOGICAL  OKAY
C
      INTEGER  IPR, MJD
      REAL     PT, PD, PP, T, P, DUMMY
      DOUBLE PRECISION TIME, JD
      CHARACTER DATOBS*8
      INCLUDE 'WETHR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL H2CHR (8, 1, CATH(KHDOB), DATOBS)
      CALL JULDAY (DATOBS, JD)
      MJD = (JD - 2400000.5D0) + 0.00001D0
C                                       In case the data is bad
      OKAY = .FALSE.
      DO 100 IPR = 1,NPR
         VALUE(IPR) = FBLANK
         ICODE = PRLIST(IPR)
C                                       Select data type
C                                       Temperature
         IF (ICODE.EQ.1) THEN
            VALUE(IPR) = WXRECR(WXKOLS(WXRTMP))
C                                       Pressure
         ELSE IF (ICODE.EQ.2) THEN
            VALUE(IPR) = WXRECR(WXKOLS(WXRPRS))
C                                       Dew point
         ELSE IF (ICODE.EQ.3) THEN
            VALUE(IPR) = WXRECR(WXKOLS(WXRDWP))
C                                       Wind velocity
         ELSE IF (ICODE.EQ.4) THEN
            VALUE(IPR) = WXRECR(WXKOLS(WXRVEL))
C                                       Wind direction
         ELSE IF (ICODE.EQ.5) THEN
            VALUE(IPR) = WXRECR(WXKOLS(WXRDIR))
C                                       Wind gust
         ELSE IF (ICODE.EQ.6) THEN
            VALUE(IPR) = WXRECR(WXKOLS(WXRGUS))
C                                       Precipitation
         ELSE IF (ICODE.EQ.7) THEN
            VALUE(IPR) = WXRECR(WXKOLS(WXRPRE))
C                                       H2O content
         ELSE IF (ICODE.EQ.8) THEN
            VALUE(IPR) = WXRECR(WXKOLS(WXRH2O))
C                                       Ion column
         ELSE IF (ICODE.EQ.9) THEN
            VALUE(IPR) = WXRECR(WXKOLS(WXRION))
C                                       Relative humidity
         ELSE IF (ICODE.EQ.10) THEN
            P = WXRECR(WXKOLS(WXRPRS))
            T = WXRECR(WXKOLS(WXRTMP))
            IF ((P.NE.FBLANK) .AND. (T.NE.FBLANK)) THEN
               CALL SATPRS (T, P, PT)
               T = WXRECR(WXKOLS(WXRDWP))
               IF (T.NE.FBLANK) THEN
                  CALL SATPRS (T, P, PD)
                  VALUE(IPR) = 100.0 * PD / PT
                  END IF
               END IF
C                                       Use opacity routine
         ELSE IF ((ICODE.GE.11) .AND. (ICODE.LE.13)) THEN
            PD = WXRECR(WXKOLS(WXRDWP))
            PT = WXRECR(WXKOLS(WXRTMP))
            PP = WXRECR(WXKOLS(WXRPRS))
            IF (WXNUMV(WXDTIM).GT.0) THEN
               TIME = WXRECD(WXKOLS(WXDTIM))
            ELSE
               TIME = WXRECR(WXKOLS(WXDTIM))
               END IF
            IF ((PT.NE.FBLANK) .AND. (PD.NE.FBLANK) .AND.
     *         (PP.NE.FBLANK)) THEN
C                                       Computed H2O
               IF (ICODE.EQ.11) CALL OPACTY (10, PT, PD, PP, DWT, MJD,
     *            TIME, VALUE(IPR), DUMMY)
C                                       K band zenith opacity
               IF (ICODE.EQ.12) CALL OPACTY (8, PT, PD, PP, DWT, MJD,
     *            TIME, DUMMY, VALUE(IPR))
C                                       Q band zenith opacity
               IF (ICODE.EQ.13) CALL OPACTY (10, PT, PD, PP, DWT, MJD,
     *            TIME, DUMMY, VALUE(IPR))
               END IF
C                                       Wind velocity from North
         ELSE IF (ICODE.EQ.14) THEN
            PT = WXRECR(WXKOLS(WXRVEL))
            PD = WXRECR(WXKOLS(WXRDIR))
            IF ((PT.NE.FBLANK) .AND. (PD.NE.FBLANK)) VALUE(IPR) = PT *
     *         COS (DG2RAD * PD)
C                                       Wind velocity from North
         ELSE IF (ICODE.EQ.15) THEN
            PT = WXRECR(WXKOLS(WXRVEL))
            PD = WXRECR(WXKOLS(WXRDIR))
            IF ((PT.NE.FBLANK) .AND. (PD.NE.FBLANK)) VALUE(IPR) = PT *
     *         SIN (DG2RAD * PD)
C                                       Dew point depression
         ELSE IF (ICODE.EQ.16) THEN
            PT = WXRECR(WXKOLS(WXRTMP))
            PD = WXRECR(WXKOLS(WXRDWP))
            IF ((PT.NE.FBLANK) .AND. (PD.NE.FBLANK)) VALUE(IPR) =
     *         PT - PD
            END IF
         IF (VALUE(IPR).NE.FBLANK) OKAY = .TRUE.
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE WETLOT (IRET)
C-----------------------------------------------------------------------
C   WETLOT plots the data thru calls to PLTSN.
C   Input in common:
C      XPTS     R(*)   Data x values
C      YPTS     R(*)   Data y values
C   Output:
C      IRET     I      Return code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IPLOT, JPLT, IANT, IPLT, NPLT, IP, NP
      LOGICAL   DOIT
      INCLUDE 'WETHR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IPLOT = 0
C                                       count the plots
      NPLOTS = 0
      DO 35 IANT = 1,MUMANT
         DO 30 IPLT = 1,NPR
            ICODE = PRLIST(IPLT)
            CALL GETSCL (IANT, IPLT, DOIT)
            IF (DOIT) NPLOTS = NPLOTS + 1
 30         CONTINUE
 35      CONTINUE
      IF (NPLOTS.LE.0) GO TO 999
C                                       Now plot
      NPLT = 0
      IP = 1
      NCOUNT = MIN (NCOUNT, NPLOTS)
      DO 100 IANT = 1,MUMANT
         NP = NUMPTS(IANT)
         DO 90 IPLT = 1,NPR
            ICODE = PRLIST(IPLT)
            CALL GETSCL (IANT, IPLT, DOIT)
            IF (DOIT) THEN
               NPLT = NPLT + 1
               JPLT = NPLT
               IPLOT = MOD (NPLT-1, NCOUNT) + 1
               IF (NPLT.EQ.NPLOTS) IPLOT = -IPLOT
               CALL PLTWX (IPLOT, IANT, IPLT, IP, NP, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 90         CONTINUE
         IP = IP + NP
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PLTWX (IPLOT, ANTNO, IPLT, IP, NP, IRET)
C-----------------------------------------------------------------------
C   PLTWX actually plots data.
C   Input:
C      IPLOT    I      Plot number on current page. If neg. then this is
C                      last plot.
C      ANTNO    I      Antenna number
C      IPLT     I      Plot type sequence number
C      IP       I      Start subscript
C      NP       I      Number of points
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C                       -1 => user request termination
C                        1 => failed to add to catalog
C                        2 => failed to create
C                        3 => graph file write error
C                        4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IPLOT, ANTNO, IPLT, IP, NP, IRET
C
      INCLUDE 'WETHR.INC'
C
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, AUNITS(NOPTS)*8,
     *   CHTYPE(NOPTS)*16, CHTMP*18, XUNITS(5)*20
      INTEGER   BUFFER(256), VER, IERR, ITYPE, IPSIZE, LUNPL, LTYPE,
     *   FINDPL, DEPTH(5), INCHAR, INP, IT(3), ID(3), IAXLAB, IAPLOT,
     *   I, NGOOD, NNOFIT, JCODE, NN, NNM
      REAL      BLC(2), TRC(2), XYRATO, DX, DY, TR, VALUE, TI, XY(2),
     *   XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2), SIZE, XMULT(2),
     *   YPT, DBY, YPT2
      LOGICAL   T, F, GOOD, CATUP, INSIDE, BLNKD
      SAVE LTYPE, BUFFER, XYRATO
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA LUNPL /26/
      DATA DEPTH /5*1/
      DATA T, F /.TRUE.,.FALSE./
      DATA AUNITS /'Centigra', 'Millibar', 'Centigra', 'm/s','Degrees',
     *   'm/s', 'cm', 2*'/m/m', 'Per cent', 'mm H2O', 2*'Opacity',
     *   2*'m/s','Centigra'/
      DATA CHTYPE /'Temperature', 'Pressure', 'Dew point',
     *   'Wind velocity', 'Wind direction', 'Wind gusts',
     *   'Precipitation', 'H2O content', 'Electron content',
     *   'Rel humidity', 'Computed H2O', 'K-band opacity',
     *   'Q-band opacity', 'North wind', 'East wind',
     *   'DewPt depression'/
      DATA XUNITS /'IAT (hr)', 'Elevation (degrees)',
     *   'Hour Angle (hr)', 'LST (hr)', 'Parallactic angle' /
C-----------------------------------------------------------------------
C                                       Time system from AN table
      XUNITS(1)(1:3) = TIMLAB(1:3)
      INSIDE = (NPR.GT.1) .AND. (NCOUNT.GT.1)
      NGOOD = 0
      NNOFIT = 0
      IRET = 3
      CATUP = T
C
      JCODE = ICODE
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      PLTINC = 1000. / NCOUNT
C                                       Set window for current plot.
      XBLC(1) = BLC(1)
      XBLC(2) = 1000.0 - ABS (IPLOT) * PLTINC
      XTRC(1) = TRC(1)
      XTRC(2) = XBLC(2) + PLTINC - 1.0
      TLC(1) = XBLC(1)
      TLC(2) = XTRC(2)
C                                       Offsets for current plot.
      YYOFF(1) = XBLC(1)
      YYOFF(2) = XBLC(2)
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 7
      AXTYP(LOCNUM) = 0
      TR = 1.2 * (YYMX(IPLT) - YYMN(IPLT))
      IF (TR.LE.0.0) TR = 1.0
      TI = TR
      CALL METSCL (LABEL, TR, CPREF(2,LOCNUM), GOOD)
      XMULT(2) = TR / TI
      CPREF(1,LOCNUM) = ' '
      XMULT(1) = 1.0
      DO 50 I = 1,2
         SIZE = 1000.0
         IF (I.EQ.2) SIZE = PLTINC
         TR = SIZE / XYSCL(I)
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
 50      CONTINUE
      CTYP(1,LOCNUM) = XUNITS(1)
      CTYP(2,LOCNUM) = AUNITS(JCODE)
C                                       Create plot file
      IF (ABS (IPLOT).EQ.1) THEN
C                                       Update catalog header.
         VER = 0
         IRET = 1
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', DISKIN, CNOIN, CATBLK, BUFFER, CATUP,
     *         'WRIT', VER, IERR)
            IF (IERR.NE.0) THEN
               NCFILE = NCFILE - 1
               GO TO 999
               END IF
            END IF
         CALL ZPHFIL ('PL', DISKIN, CNOIN, VER, PFILE, IERR)
         IF (IERR.NE.0) GO TO 960
         IPSIZE = 0
         ITYPE = 46
         CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, NPARMS,
     *      XNAMEI, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER, LUNPL,
     *      FINDPL, IERR)
         IRET = 2
         IF (IERR.NE.0) GO TO 960
C                                       Number of characters on each
C                                       side of the plot
         CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
         CALL CHNTIC (XBLC, XTRC, INP)
         INP = MAX (INP, 5)
         LTYPE = MOD (ABS (LABEL), 100)
         IF (LTYPE.EQ.2) CHOUT(1) = 2.5
         IF (LTYPE.GT.2) CHOUT(1) = INP + 4
         IF (LTYPE.GT.1) CHOUT(2) = 2.0
         IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
         IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = 2.333
         IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *      CHOUT(4) = CHOUT(4) + 1.333
C                                       Init for line drawing.
         IF (DOTV) THEN
            XYRATO = WINDTV(4) - WINDTV(2) + 1
            XYRATO = (WINDTV(3) - WINDTV(1) + 1) / XYRATO
         ELSE
            XYRATO = 1.0
            END IF
         CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, BUFFER, IERR)
         IRET = 3
         IF (IERR.NE.0) GO TO 970
         IF (.NOT.DOTV) THEN
            WRITE (MSGTXT,1000) VER
            CALL MSGWRT (2)
            END IF
         END IF
      IRET = 3
      CATUP = T
C                                       Draw border
      CALL GLTYPE (1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XBLC(1), XBLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XTRC(1), XBLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XTRC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Top labels: type & name
      IF ((ABS(IPLOT).EQ.1) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         DX = 0.0
         DY = 0.50
C                                       The second line of the header
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         INCHAR = 16
         INP = 1
         IF (INSIDE) THEN
            TEXT = 'Multiple'
         ELSE
            TEXT = CHTYPE(JCODE)
            END IF
         CALL CHTRIM (TEXT, INCHAR, TEXT, INP)
         INP = INP + 1
         TEXT(INP:INP+16) = ' vs '// TIMLAB(1:3) // ' time for '
         INP = INP + 17
C                                       File name
         CALL H2CHR (18, KHIMNO, CATH(KHIMN), CHTMP)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTMP(13:18))
         CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT(INP:), INCHAR)
         CALL REFRMT (TEXT, ' ', INCHAR)
         WRITE (TEXT(INCHAR+1:),1010) 'WX', IVER
         CALL REFRMT (TEXT, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
         IF ((LABEL.GT.0) .AND. (LTYPE.GT.1)) THEN
            DY = 0.5 + 1.333
C                                       the first line of the header
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (TEXT,1030) VER, ADATE, ATIME
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         END IF
C                                       station ID
      CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX =  1.5
      DY = -1.8
      IF (ANTNO.EQ.1) THEN
         TEXT = 'All antennas'
      ELSE
         WRITE (TEXT,1040) ANTNO-1
         INP = 4
         TEXT(INP+1:) = STNNAM(ANTNO-1)
         END IF
      CALL CHTRIM (TEXT, 132, TEXT, INCHAR)
      IF (INSIDE) TEXT(INCHAR+2:) = CHTYPE(JCODE)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Set up location common
C                                       Blank bottom label.
      IF ((IPLOT.GE.0) .AND. (ABS (IPLOT).NE.NCOUNT)) THEN
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         END IF
C                                       Only label Y axis once.
      IAXLAB = NCOUNT / 2 + 1
      IAPLOT = ABS (IPLOT)
      IF ((.NOT.INSIDE) .AND. (IAPLOT.NE.IAXLAB) .AND. ((IPLOT.GE.0)
     *   .OR. (IAPLOT.GT.IAXLAB))) CPREF(2,LOCNUM) = '-1'
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Size of symbol.
      DBY = 0.5 * FACTOR
C                                       Loop
      NNM = IP + NP - 1
      DY = 5.0 * FACTOR * XYRATO
      DX = 5.0 * FACTOR
      CALL GLTYPE (4, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DO 100 NN = IP,NNM
C                                       Scale X
         XY(1) = XPTS(NN) * 360.0
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) THEN
            NNOFIT = NNOFIT + 1
            GO TO 100
            END IF
C                                       loop for points
         VALUE = YPTS(IPLT,NN)
         IF (VALUE.NE.FBLANK) THEN
            XY(2) = VALUE
            XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
            IF ((XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
               NNOFIT = NNOFIT + 1
            ELSE
               NGOOD = NGOOD + 1
C                                       Mark point
               YPT = MIN (XY(2) + DY, XTRC(2))
               YPT2 = MAX (XY(2) - DY, XBLC(2))
               CALL GPOS (XY(1)+DX, XY(2), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               CALL GVEC (XY(1)-DX, XY(2), BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               CALL GPOS (XY(1), YPT, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               CALL GVEC (XY(1), YPT2, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
            END IF
 100     CONTINUE
      IF (DOLINE) THEN
         CALL GLTYPE (2, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         BLNKD = .TRUE.
         DO 200 NN = IP,NNM
C                                       Scale X
            XY(1) = XPTS(NN) * 360.0
            XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
            IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) THEN
               BLNKD = .TRUE.
               GO TO 200
               END IF
C                                       loop for points
            VALUE = YPTS(IPLT,NN)
            IF (VALUE.EQ.FBLANK) THEN
               BLNKD = .TRUE.
            ELSE
               XY(2) = VALUE
               XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
               IF ((XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
                  BLNKD = .TRUE.
C                                       Mark point
               ELSE IF (BLNKD) THEN
                  CALL GPOS (XY(1), XY(2), BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  BLNKD = .FALSE.
               ELSE
                  CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  END IF
               END IF
 200        CONTINUE
         END IF
C                                       Done: finish plot
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1202) NNOFIT
         CALL MSGWRT (2)
         END IF
      IF ((IPLOT.GT.0) .AND. (ABS(IPLOT).LT.NCOUNT)) GO TO 210
         GPHPAG = IPLOT.GT.0
         CALL GFINIS (BUFFER, IERR)
         IF (IERR.GT.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
            IERR = 0
            END IF
 210  IF (IERR.GT.0) GO TO 975
         IRET = MIN (IERR, 0)
         GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1202) NNOFIT
         CALL MSGWRT (2)
         END IF
      GPHPAG = IPLOT.GT.0
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, BUFFER, IERR)
            IERR = 0
            END IF
         GO TO 999
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Plot file version',I4,'  created.')
 1010 FORMAT ('__(',A2,I4,')')
 1030 FORMAT ('Plot file version',I4,'__created ',A, A)
 1040 FORMAT (I3)
 1200 FORMAT ('PLTWX:',I9,' points plotted')
 1202 FORMAT ('PLTWX:',I9,' points did not fit')
 1960 FORMAT ('PLTWX: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLTWX: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
      SUBROUTINE WETFLG (IRET)
C-----------------------------------------------------------------------
C   WETFLG copies the FG table to a new one and appends any flags due to
C   weather conditions.
C   Output:
C      IRET   I   > 0 something bad
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'WETHR.INC'
C
      INTEGER   INVER, OVER, I, IROUND, ILUN, OLUN, IANT, IPLT, ITRIM,
     *   NF, IP, NP, MP, J, IDUM
      REAL      VALUE
      LOGICAL   DOIT
      CHARACTER CHTYPE(NOPTS)*16, CTEMP*12
      INCLUDE 'INCS:PUVD.INC'
C                                       FG table parameters
      INTEGER   FLANT(2), IFS(2), CHANS(2), IFGRNO, FGKOLS(MAXFGC),
     *   FGNUMV(MAXFGC), SOURID, SUBA, FREQID
      LOGICAL   PFLAGS(4)
      REAL      TIMER(2)
      CHARACTER  REASON*24
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA ILUN, OLUN /28, 29/
      DATA PFLAGS /4*.TRUE./
      DATA CHTYPE /'Temperature', 'Pressure', 'Dew point',
     *   'Wind velocity', 'Wind direction', 'Wind gusts',
     *   'Precipitation', 'H2O content', 'Electron content',
     *   'Rel humidity', 'Computed H2O', 'K-band opacity',
     *   'Q-band opacity', 'North wind', 'East wind',
     *   'DewPt depression'/
C-----------------------------------------------------------------------
      IF (DETIME.LE.0.0) THEN
         DETIME = (XPTS(2) - XPTS(1)) / 2.0
      ELSE
         DETIME = DETIME / (60.0 * 24.0)
         END IF
      CALL FNDEXT ('FG', CATBLK, I)
      INVER = IROUND (XFGVER)
      IF ((INVER.EQ.0) .OR. (INVER.GT.I)) INVER = I
      OVER = IROUND (XFGOUT)
      IF ((OVER.LE.0) .OR. (OVER.GT.I)) OVER = I + 1
      IF (OVER.LE.I) INVER = -ABS(INVER)
C                                       copy old file
      IF (INVER.GT.0) THEN
         WRITE (MSGTXT,1000) INVER, OVER
         CALL MSGWRT (3)
         CALL TABCOP ('FG', INVER, OVER, ILUN, OLUN, DISKIN, DISKIN,
     *      CNOIN, CNOIN, CATBLK, UBUFF, WXBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) 'COPY', IRET
            GO TO 990
            END IF
         END IF
C                                       init FG parms
      SUBA = ISUBA
      FREQID = 0
      SOURID = 0
      CHANS(1) = 0
      CHANS(2) = 0
      IFS(1) = 0
      IFS(2) = 0
      FLANT(2) = 0
      CALL FLGINI ('WRIT', WXBUFF, DISKIN, CNOIN, OVER, CATBLK, OLUN,
     *   IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'OPEN', IRET
         GO TO 990
         END IF
C                                       Loop over parameter types
      DO 100 IPLT = 1,NPR
         ICODE = PRLIST(IPLT)
         I = ITRIM (CHTYPE(ICODE))
         I = MIN (I, 12)
         WRITE (REASON,1020) CHTYPE(ICODE)(:I), BPARM(IPLT)
         NF = 0
         MP = 0
         DO 90 IANT = 1,MUMANT
            NP = MP + 1
            MP = MP + NUMPTS(IANT)
            CALL GETSCL (IANT, IPLT, DOIT)
            IF (DOIT) THEN
               FLANT(1) = IANT - 1
C                                       find first bad point
 20            DO 30 IP = NP,MP
                  VALUE = YPTS(IPLT,IP)
                  IF (VALUE.NE.FBLANK) THEN
                     IF (ICODE.LT.14) THEN
                        IF (VALUE.GT.BPARM(IPLT)) GO TO 40
                     ELSE
                        IF (ABS(VALUE).GT.BPARM(IPLT)) GO TO 40
                        END IF
                     END IF
 30               CONTINUE
               GO TO 90
C                                       find end bad point
 40            DO 50 J = IP+1,MP
                  VALUE = YPTS(IPLT,J)
                  IF (VALUE.NE.FBLANK) THEN
                     IF (ICODE.LT.14) THEN
                        IF (VALUE.LE.BPARM(IPLT)) GO TO 60
                     ELSE
                        IF (ABS(VALUE).GT.BPARM(IPLT)) GO TO 60
                        END IF
                     END IF
 50               CONTINUE
               J = MP+1
C                                       Got a range
 60            J = J - 1
               TIMER(1) = XPTS(IP) - DETIME
               TIMER(2) = XPTS(J) + DETIME
               NP = J + 1
               NF = NF + 1
C                                       flag command
               CALL TABFLG ('WRIT', WXBUFF, IFGRNO, FGKOLS, FGNUMV,
     *            SOURID, SUBA, FREQID, FLANT, TIMER, IFS, CHANS,
     *            PFLAGS, REASON, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) 'WRITE', IRET
                  GO TO 990
                  END IF
C                                       loop for more
               GO TO 20
               END IF
 90         CONTINUE
         WRITE (MSGTXT,1090) NF, REASON
         CALL MSGWRT (4)
 100     CONTINUE
C                                       close file
      CALL TABFLG ('CLOS', WXBUFF, IFGRNO, FGKOLS, FGNUMV, SOURID, SUBA,
     *   FREQID, FLANT, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
C                                       Clear write status
      CALL CATDIR ('CSTA', DISKIN, CNOIN, CTEMP, CTEMP, IDUM, 'UV',
     *   IDUM, 'CLWR', WXBUFF, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Copying FG version',I3,' to',I3)
 1010 FORMAT ('WETFLG: ERROR ON ',A,' =',I5)
 1020 FORMAT (A,' >',1PE10.3)
 1090 FORMAT ('Wrote',I4,' flag commands for ',A)
      END
