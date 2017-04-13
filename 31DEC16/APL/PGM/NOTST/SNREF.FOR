LOCAL INCLUDE 'SNREF.INC'
C                                       Local include for SNREF
      INCLUDE 'INCS:PUVD.INC'
C                                       Input parameters
      REAL      XSIN, XDISIN, XNVER, XBAND, XFREQ, XFQID, XSUBA, KEEP
      HOLLERITH XNAMEI(3), XCLAIN(2), XTYPE
      CHARACTER NAMEIN*12, CLAIN*6, TYPE*2
C                                       Program info
      REAL      TSTART, TSTOP, GMMOD, SELBAN
      INTEGER   SEQIN, DISKIN, CNOIN, IVER, BIF, EIF, NCOUNT, ICODE,
     *   NPARMS, NID, SID(500), SUMSTK, FRQSEL, IANT, SUBARR,
     *   MUMPOL, MUMIF, MUMANT, NUMPTS(MAXANT), NANREC(MAXANT),
     *   FANREC(MAXANT), NUMREF(MAXANT)
      DOUBLE PRECISION FOFF(MAXIF), SELFRQ, GNRECD(XCLRSZ/2)
C                                       SN/CL table info
      INTEGER CLBUFF(512), NCLINR, NUMANT, NUMPOL, NUMIF, ICLRNO,
     *   KOLS(40), KOLTYP(40), KOLDIM(40), ICLUN, GNRECI(XCLRSZ),
     *   TIMKOL, INTKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL, IFRKOL,
     *   GEOKOL, DOPKOL, ATMKOL, DATKOL,
     *   MB1KOL, RE1KOL, IM1KOL, DL1KOL, RA1KOL, WT1KOL, RF1KOL, TS1KOL,
     *   TA1KOL, CK1KOL, DC1KOL, DS1KOL, DD1KOL,
     *   MB2KOL, RE2KOL, IM2KOL, DL2KOL, RA2KOL, WT2KOL, RF2KOL, TS2KOL,
     *   TA2KOL, CK2KOL, DC2KOL, DS2KOL, DD2KOL,
     *   MBKOL(4), REKOL(4), IMKOL(4), DLKOL(4), RAKOL(4), WTKOL(4),
     *   RFKOL(4), TSKOL(4), TAKOL(4), CKKOL(4), DCKOL(4), DSKOL(4),
     *   DDKOL(4), STKOL(4),
     *   DOPLKL, DOP3KL, CLTIME, CABKOL, ST1KOL, ST2KOL
      REAL GNREC(XCLRSZ)
C                                       Constants
      DOUBLE PRECISION SIDER, CLIGHT
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XTYPE, XNVER,
     *   XBAND, XFREQ, XFQID, XSUBA, KEEP
      COMMON /VPARM/ SEQIN, DISKIN, CNOIN, IVER, BIF, EIF, NCOUNT,
     *   ICODE, NPARMS, IANT
      COMMON /VGNCOM/ SELFRQ, TSTART, TSTOP, SELBAN, GMMOD,
     *   NID, SID, SUMSTK, FRQSEL, SUBARR, MUMPOL, MUMIF,
     *   MUMANT, NUMPTS,  NANREC, FANREC, NUMREF
      COMMON /VGNCHR/ NAMEIN, CLAIN, TYPE
      COMMON /TABCOM/ GNREC, CLBUFF, NCLINR, NUMANT, NUMPOL, NUMIF,
     *   ICLRNO, KOLS, KOLTYP, KOLDIM, ICLUN,
     *   MBKOL, REKOL, IMKOL, DLKOL, RAKOL, WTKOL, RFKOL, TSKOL,
     *   TAKOL, CKKOL, DCKOL, DSKOL, DDKOL, STKOL,
     *   DOPLKL, DOP3KL, CLTIME
      COMMON /CONST/ SIDER, CLIGHT, FOFF
      EQUIVALENCE (GNREC, GNRECD, GNRECI)
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), INTKOL),
     *   (KOLS(3), SOUKOL), (KOLS(4), ANTKOL), (KOLS(5), SUBKOL),
     *   (KOLS(6), FRQKOL), (KOLS(7), IFRKOL),
     *   (KOLS(8), GEOKOL), (KOLS(9), DOPKOL), (KOLS(10), ATMKOL),
     *   (KOLS(11), DATKOL)
      EQUIVALENCE (KOLS(12), MB1KOL),
     *   (KOLS(13), RE1KOL), (KOLS(14), IM1KOL),
     *   (KOLS(15), RA1KOL), (KOLS(16), DL1KOL), (KOLS(17), WT1KOL),
     *   (KOLS(18), RF1KOL), (KOLS(19), TS1KOL), (KOLS(20), TA1KOL),
     *   (KOLS(21), CK1KOL), (KOLS(22), DC1KOL),
     *   (KOLS(23), DS1KOL), (KOLS(24), DD1KOL)
      EQUIVALENCE (KOLS(25), MB2KOL),
     *   (KOLS(26), RE2KOL), (KOLS(27), IM2KOL),
     *   (KOLS(28), RA2KOL), (KOLS(29), DL2KOL), (KOLS(30), WT2KOL),
     *   (KOLS(31), RF2KOL), (KOLS(32), TS2KOL), (KOLS(33), TA2KOL),
     *   (KOLS(34), CK2KOL), (KOLS(35), DC2KOL),
     *   (KOLS(36), DS2KOL), (KOLS(37), DD2KOL),
     *   (KOLS(38), CABKOL), (KOLS(39), ST1KOL), (KOLS(40), ST2KOL)
C                                                          End SNREF
LOCAL END
      PROGRAM SNREF
C-----------------------------------------------------------------------
C! Determines reference antenna with min R-L variability
C# UV EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2012, 2015
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
C   SNREF plots SN or CL extension files. A 'PL' extension file is made
C   which can be displayed in the usual ways .
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      INEXT......'SN','TY','PC' or 'CL' table to be plotted
C      INVERS.....Version number of table to plot, 0=>highest no.
C-----------------------------------------------------------------------
C
      CHARACTER PRGN*6
      REAL      SNVALS(2)
      LONGINT   PSNVAL
      INTEGER   IRET, MVAL, NWORDS, NROWS
      INCLUDE 'SNREF.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGN /'SNREF '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL SNPIN (PRGN, NROWS, IRET)
      MUMANT = MAX (1, MUMANT)
      MVAL = 1 + MUMPOL*MUMIF
      MVAL = MAX (MVAL, 5)
      NWORDS = (MVAL * NROWS - 1) / 1024  + 21
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SNVALS, PSNVAL, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         END IF
C                                       read data to figure out
C                                       distribution
      NWORDS = NWORDS * 1024
      IF (IRET.EQ.0) CALL SNPCNT (NWORDS, IRET)
C                                       Fetch data, determine values
      IF (IRET.EQ.0) CALL SNPREF (MUMANT, SNVALS(1+PSNVAL), IRET)
      IF (IRET.LT.0) IRET = 0
C                                       Close down
      CALL DIE (IRET, CLBUFF)
C
 999  STOP
      END
      SUBROUTINE SNPIN (PRGN, NROWS, IERR)
C-----------------------------------------------------------------------
C   Gets the inputs parameters for SNREF.
C   Inputs:
C      PRGN    C*6  Program name
C   Output in common:
C      SUMSTK  I    Selected Stokes 0=both, 1=R, 2=L, 4=difference
C   Output:
C      IERR    I    Error code: 0 => ok
C      ICODE   I    1='PHAS', 2='AMP ', 3='DELA', 4='RATE', 5='TSYS',
C                   6='SUM ', 7='DOPL', 8='SNR', 9='MDEL', 10='TANT',
C                   11='ATM', 12='GEO', 13='CCAL', 14='DDLY'
C                   15='REAL', 16='IMAG', 17='IFR', 18='PDIF',
C                   19='PSUM', 20=PGN ', 21='PON ', 22='POFF', 23-'PSYS'
C-----------------------------------------------------------------------
      INTEGER   NROWS, IERR
      CHARACTER PRGN*6
C
      INTEGER   NCODE, NTPLT
C
      PARAMETER (NCODE=23, NTPLT=5)
C
      CHARACTER STAT*4, TYPTMP*2
      INTEGER   IRET, BUFF(256), IROUND, LUN, FRQTMP, VER, NIF, JERR,
     *   BUFFER(512)
      LOGICAL   MATCH
      INCLUDE 'SNREF.INC'
      INTEGER   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      NPARMS = 14
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
      CALL H2CHR (2, 1, XTYPE, TYPE)
      IF (TYPE.EQ.' ') TYPE ='SN'
      CALL FILL (MAXANT, 0, NUMPTS)
C                                       Integers
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      IVER = IROUND (XNVER)
      SUMSTK = 3
      ICODE = 1
C                                       we are looking at R-L
      MUMPOL = 1
C                                       Time range
      TSTART = 0.0
      TSTOP = 999.0
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
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CNOIN
      FVOL(NCFILE) = DISKIN
      FRW(NCFILE) = 1
      XDISIN = DISKIN
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
      SEQIN = CATBLK(KIIMS)
      XSIN = SEQIN
C                                       Subarray
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.GE.0) THEN
         LUN = 25
         CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *      FRQSEL, IERR)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1070)
            IERR = 1
            GO TO 990
            END IF
         IF (IERR.GT.0) GO TO 999
         END IF
      BIF = 1
      EIF = 1
      IF (JLOCIF.GE.0) EIF = CATBLK(KINAX+JLOCIF)
C                                       get frequencies
      VER = 1
      LUN = 25
      IF (FRQSEL.LE.0) FRQTMP = 1
      IF (FRQSEL.GT.0) FRQTMP = FRQSEL
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, VER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQTMP, JERR)
C                                       Get antenna names
      CALL GETANT (DISKIN, CNOIN, MAX (1, SUBARR), CATBLK, BUFFER, JERR)
      MUMANT = NSTNS
C                                       Open table to check
C                                       Open SN, CL, TY or PC table
      IF ((TYPE.EQ.'SN') .OR. (TYPE.EQ.'CL')) THEN
         CALL SNPOPN (NROWS, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Invalid table type
      ELSE
         IERR = 5
         MSGTXT = 'INVALID TABLE TYPE =' // TYPE
         GO TO 990
         END IF
      XNVER = IVER
      MUMIF = EIF - BIF + 1
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
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE SNPOPN (NROWS, IERR)
C-----------------------------------------------------------------------
C   Routine to open SN, CL table and get necessary information
C   Input from Common:
C      TYPE     C*2  'SN', 'CL'
C      DISKIN   I     Disk number
C      CNOIN    I     Catalog slot number
C      CATBLK   I(*)  Catalog header
C      SUMSTK   I     Stokes type requested 0=both, 1=R, 2=L,
C                     3=difference, 4=ratio
C   Output:
C      IERR     I     Error code, 0=OK else failed.
C   Output in common:
C      ICLRNO       I    Current cal record number
C      NCLINR       I    Number of gain records in file.
C      NUMANT       I    Number of antennas
C      NUMPOL       I    Number of polarizations
C      NUMIF        I    Number of IFs.
C      KOLS         I(*) Column pointers
C      KOLTYP       I(*) Column data types
C      KOLDIM       I(*) Column dimension
C-----------------------------------------------------------------------
      INTEGER   NROWS, IERR
      INCLUDE 'SNREF.INC'
C
      CHARACTER KEYW(4)*8, COLHD1(11)*24, COLHD2(13)*24, COLHD3(13)*24,
     *   COLTAB(40)*24, COLHED(37)*24, KEYSN(4)*8
      INTEGER   NKEY, NREC, NCOL, DATP(128,2), IPOINT, KEYTYP(4),
     *   KLOCS(4), KEYVAL(6), I, KP, MSGSAV
      LOGICAL   T
      REAL      KEYVR(6)
      DOUBLE PRECISION KEYVAD
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (KEYVAL, KEYVR)
      EQUIVALENCE (COLHED(1), COLHD1), (COLHED(12), COLHD2),
     *   (COLHED(25), COLHD3)
      DATA COLHD1 /'TIME                    ',
     *   'TIME INTERVAL           ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'I.FAR.ROT               ',
     *   'GEODELAY                ', 'DOPPOFF                 ',
     *   'ATMOS                   ', 'DATMOS                  '/
      DATA COLHD2 /'MBDELAY1      ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'RATE 1                  ', 'DELAY 1                 ',
     *   'WEIGHT 1                ', 'REFANT 1                ',
     *   'TSYS 1                  ', 'TANT 1                  ',
     *   'CLOCK 1                 ', 'DCLOCK 1                ',
     *   'DISP 1                  ', 'DDISP 1                 '/
      DATA COLHD3 /'MBDELAY2      ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'RATE 2                  ', 'DELAY 2                 ',
     *   'WEIGHT 2                ', 'REFANT 2                ',
     *   'TSYS 2                  ', 'TANT 2                  ',
     *   'CLOCK 2                 ', 'DCLOCK 2                ',
     *   'DISP 2                  ', 'DDISP 2                 '/
      DATA KEYSN /'NO_ANT  ', 'NO_POL  ', 'NO_IF   ','MGMOD   '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Open table
      ICLUN = 28
      NKEY = 0
      NREC = 0
      NCOL = 0
      ICLRNO = 1
      CALL TABINI ('READ', TYPE, DISKIN, CNOIN, IVER, CATBLK, ICLUN,
     *   NKEY, NREC, NCOL, DATP, CLBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1100) IERR, TYPE, IVER
         GO TO 980
         END IF
C                                       Get number of scans
      NCLINR = CLBUFF(5)
      NROWS = NCLINR
C                                       Check if empty
      IF (NCLINR.LE.0) THEN
         IERR = 6
         MSGTXT = 'ERROR: SELECTED TABLE IS EMPTY'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Get column pointers
      NKEY = 40
      DO 10 I = 1,NKEY
         COLTAB(I) = COLHED(I)
 10      CONTINUE
      CALL FNDCOL (NKEY, COLTAB, 24, T, CLBUFF, KOLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
      IERR = 0
C                                       Time column logical number
      CLTIME = KOLS(1)
C                                       Convert to pointers, types
      DO 20 I = 1,NKEY
         KP = KOLS(I)
         IF (KP.GT.0) THEN
            KOLS(I) = DATP(KP,1)
            KOLTYP(I) = MOD (DATP(KP,2), 10)
            KOLDIM(I) = DATP(KP,2) / 10
         ELSE
            KOLS(I) = -1
            KOLTYP(I) = -1
            KOLDIM(I) = 0
            END IF
 20      CONTINUE
C                                       Table keywords
      NKEY = 4
      DO 30 I = 1,NKEY
         KEYW(I) = KEYSN(I)
 30      CONTINUE
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TABKEY ('READ', KEYW, NKEY, CLBUFF, KLOCS, KEYVAL, KEYTYP,
     *   IERR)
      MSGSUP = MSGSAV
      IF ((IERR.GE.1) .AND. (IERR.LE.20)) GO TO 999
      IERR = 0
C                                       No. antennas.
      NUMANT = NSTNS
      IPOINT = KLOCS(1)
      IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. poln.
      NUMPOL = 1
      IPOINT = KLOCS(2)
      IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF
      NUMIF = 1
      IPOINT = KLOCS(3)
      IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       Mean gain modulus
      GMMOD = 1.0
      IPOINT = KLOCS(4)
      IF (IPOINT.GT.0) THEN
         IF (KEYTYP(4).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVR(IPOINT), KEYVAD)
         ELSE
            KEYVAD = KEYVR(IPOINT)
            END IF
         IF (KEYVAD.GT.0.0) GMMOD = 1.0 / KEYVAD
         END IF
C                                       Set pointers
      DOPKOL = DOPKOL + BIF - 1
      DOP3KL = DOPKOL + EIF - 1
      DOPLKL = DOPKOL
C                                       1st poln
      MBKOL(1) = MB1KOL
      REKOL(1) = RE1KOL + BIF - 1
      IMKOL(1) = IM1KOL + BIF - 1
      DLKOL(1) = DL1KOL + BIF - 1
      RAKOL(1) = RA1KOL + BIF - 1
      WTKOL(1) = WT1KOL + BIF - 1
      RFKOL(1) = RF1KOL + BIF - 1
      TSKOL(1) = TS1KOL + BIF - 1
      TAKOL(1) = TA1KOL + BIF - 1
      CKKOL(1) = CK1KOL
      DCKOL(1) = DC1KOL
      DSKOL(1) = DS1KOL
      DDKOL(1) = DD1KOL
      STKOL(1) = ST1KOL + BIF - 1
C                                       2nd Poln
      MBKOL(2) = MB2KOL
      REKOL(2) = RE2KOL + BIF - 1
      IMKOL(2) = IM2KOL + BIF - 1
      DLKOL(2) = DL2KOL + BIF - 1
      RAKOL(2) = RA2KOL + BIF - 1
      WTKOL(2) = WT2KOL + BIF - 1
      RFKOL(2) = RF2KOL + BIF - 1
      TSKOL(2) = TS2KOL + BIF - 1
      TAKOL(2) = TA2KOL + BIF - 1
      CKKOL(2) = CK2KOL
      DCKOL(2) = DC2KOL
      DSKOL(2) = DS2KOL
      DDKOL(2) = DD2KOL
      STKOL(2) = ST2KOL + BIF - 1
C                                       Phase, amplitude, summary
      IF ((REKOL(1).LT.0) .AND. (IMKOL(1).LT.0)) GO TO 500
      IF ((REKOL(2).LT.0) .OR. (IMKOL(2).LT.0)) GO TO 500
      GO TO 999
C                                       Requested data not in table
 500  WRITE(MSGTXT,1500) 'PHAS', TYPE
      IERR = 10
      GO TO 980
C                                       Error
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR ',I3,' OPENING ',A,' TABLE NO. ',I3)
 1500 FORMAT (' REQUESTED DATA ',A,' NOT IN ',A,' TABLE ')
      END
      SUBROUTINE SNPCNT (NWORDS, IERR)
C-----------------------------------------------------------------------
C   SNPCNT reads the SN or CL table to find the number of samples for
C   each antenna
C   Input:
C      NWORDS   I      Number words in work space
C   Input/Output in common:
C      TSTART   R      Start time of plot
C      TSTOP    R      Stop time of plot
C   Output:
C      IERR     I      Error code, 0=OK else failed
C   Outputs in common:
C-----------------------------------------------------------------------
      INTEGER   NWORDS, IERR
C
      LOGICAL   NODATA, OKAY
      INTEGER   I, NP, IIS, IIF, IP1
      REAL      TB, TE, GTIME
      INCLUDE 'SNREF.INC'
      REAL      VALUE(2*MAXIF), VALUE2(2*MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL FILL (MAXANT, 0, NANREC)
      CALL FILL (MAXANT, 0, NUMREF)
      NODATA = .TRUE.
      TB = 1.0E5
      TE = -1.0E5
C                                       Loop thru data
      NP = MUMPOL * MUMIF
      DO 100 ICLRNO = 1,NCLINR
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Record within specified
C                                       time range ?
         IF (KOLTYP(CLTIME).EQ.1) THEN
            GTIME = GNRECD(TIMKOL)
         ELSE
            GTIME = GNREC(TIMKOL)
            END IF
         IF ((GTIME.LT.TSTART) .OR. (GTIME.GT.TSTOP)) GO TO 100
C                                       Freq id
         IF ((GNRECI(FRQKOL).GT.0) .AND. (GNRECI(FRQKOL).NE.FRQSEL)
     *      .AND. (FRQSEL.GT.0)) GO TO 100
C                                       Subarray
         IF ((GNRECI(SUBKOL).GT.0) .AND. (SUBARR.GT.0) .AND.
     *      (GNRECI(SUBKOL).NE.SUBARR)) GO TO 100
C                                      Get start, stop times
         TB = MIN (TB, GTIME)
         TE = MAX (TE, GTIME)
C                                       Get value
         CALL SNPDAT (VALUE, VALUE2, OKAY)
C                                       Max. - Min
         IF (OKAY) THEN
            IANT = GNRECI(ANTKOL)
            NODATA = .FALSE.
            NANREC(IANT) = NANREC(IANT) + 1
            DO 70 IIS = 1,MUMPOL
               IP1 = RFKOL(IIS) - 1
               DO 60 IIF = BIF,EIF
                  IP1 = IP1 + 1
                  IANT = GNRECI(IP1)
                  NUMREF(IANT) = NUMREF(IANT) + 1
 60               CONTINUE
 70            CONTINUE
            END IF
 100     CONTINUE
      FANREC(1) = 1
      DO 120 I = 2,MAXANT
         FANREC(I) = FANREC(I-1) + NANREC(I-1)
 120     CONTINUE
      IF (NWORDS.LT.FANREC(MAXANT)+NANREC(MAXANT)) THEN
         MSGTXT = 'MEMORY TOO SMALL'
         IERR = 10
         END IF
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SNPMAX: ERROR =',I3,' FROM TABIO')
      END
      SUBROUTINE SNPDAT (VALUE, VALUE2, OKAY)
C-----------------------------------------------------------------------
C   Routine to get the specified value from a SN/CL table entry
C   Input from common:
C      GNREC    R(*)  Table record
C      ICODE    I     Plot code
C      SUMSTK   I     Selected Stokes 0=both, 1=R, 2=L, 3=Difference
C   Also uses pointers etc. set in SNPOPN
C   Output:
C      VALUE    R(*)   Table value, magic value blanked (amp on ICODE 6)
C      OKAY     L      Some values are good
C-----------------------------------------------------------------------
      REAL      VALUE(*), VALUE2(*)
      LOGICAL   OKAY
C
      INTEGER   IIS, IIF, IP1, IP2, LP, JP1, JP2
      REAL      AMP1, AMP2
      INCLUDE 'SNREF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       In case the data is bad
      LP = MUMPOL * MUMIF
      CALL RFILL (LP, FBLANK, VALUE)
      CALL RFILL (LP, FBLANK, VALUE2)
C                                       Select data type
C                                       Phase (deg)
      DO 110 IIS = 1,MUMPOL
         LP = IIS - MUMPOL
         IP1 = REKOL(IIS) - 1
         JP1 = IMKOL(IIS) - 1
         IP2 = REKOL(2) - 1
         JP2 = IMKOL(2) - 1
         DO 105 IIF = 1,MUMIF
            IP1 = IP1 + 1
            JP1 = JP1 + 1
            IP2 =  IP2 + 1
            JP2 = JP2 + 1
            LP = LP + MUMPOL
            IF ((GNREC(IP1).NE.FBLANK) .AND. (GNREC(JP1).NE.FBLANK)
     *         .AND. (GNREC(IP2).NE.FBLANK) .AND.
     *         (GNREC(JP2).NE.FBLANK)) THEN
               AMP1 = SQRT (GNREC(IP1)**2 + GNREC(JP1)**2)
               AMP2 = SQRT (GNREC(IP1)**2 + GNREC(JP1)**2)
               IF ((AMP1.GT.0.0) .OR. (AMP2.GT.0.0)) THEN
                  VALUE(LP) = GNREC(IP1)/AMP1 - GNREC(IP2)/AMP2
                  VALUE2(LP) = GNREC(JP1)/AMP1 - GNREC(JP2)/AMP2
                  END IF
               END IF
 105        CONTINUE
 110     CONTINUE
C
      OKAY = .TRUE.
      LP = MUMPOL * MUMIF
      DO 910 IIS = 1,LP
         IF (VALUE(IIS).NE.FBLANK) GO TO 999
 910     CONTINUE
      OKAY = .FALSE.
C
 999  RETURN
      END
      SUBROUTINE SNPREF (NA, WORK, IRET)
C-----------------------------------------------------------------------
C   SNPREF loops over antenna - making a copy of the SN or CL table,
C   re-referencing it to the antenna, and the getting the R-L values
C   and computing the rms and extrema for each choice and then
C   deleting the temporary file.  Finally, it takes the best choice
C   and makes a new SN or CL table for it referenced to that choice.
C   Inputs:
C      NA       I      Number antennas
C   Outputs:
C      SNVALS   R(*)   Data work area (*)
C      IRET     I      Error code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   NA, IRET
      REAL      WORK(*)
C
      INCLUDE 'SNREF.INC'
      INTEGER   LA, OVER, NVER, BUFF1(256), BUFF2(256), LUN1, LUN2, IA,
     *   LKEY, LKOLS(18), IPNT, NKEY, I, NREC, NCOL, IERR, NUMROW, LOOP,
     *   JKOLS(18), IIF, ANTMIN(MAXIF), OVRMIN, IP, NUMP, NUMI, NUMA,
     *   CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMN, OVERAN(MAXANT),
     *   CATSAV(256)
      REAL      ANTRMS(MAXANT,MAXIF), ANTMAX(MAXANT,MAXIF), SMOTIM(3),
     *   MINRMS(MAXIF), OVRMS, SUM, RANOD(25), DECNOD(25)
      LOGICAL   ISAPPL
      CHARACTER KEYS(18)*24, PHNAME*48
      LOGICAL   OKAY
      DOUBLE PRECISION FREQIF, VSUM(2,MAXANT,MAXIF),
     *   VSUMS(2,MAXANT,MAXIF), VCOUNT(2,MAXANT,MAXIF)
      REAL      VALUE(2*MAXIF), VALUE2(2*MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUN1, LUN2 /47,48/
      DATA KEYS /'ANTENNA NO.             ',
     *   'REFANT 1                ', 'SUBARRAY                ',
     *   'WEIGHT 1                ', 'TIME                    ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'DELAY 1                 ', 'RATE 1                  ',
     *   'ANTENNA NO.             ',
     *   'REFANT 2                ', 'SUBARRAY                ',
     *   'WEIGHT 2                ', 'TIME                    ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'DELAY 2                 ', 'RATE 2                  '/
C-----------------------------------------------------------------------
C                                       close the table
      CALL TABIO ('CLOS', 0, ICLRNO, GNREC, CLBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING OPEN INPUT TABLE'
         GO TO 990
         END IF
      CALL COPY (256, CATBLK, CATSAV)
C                                       zero save arrays
      I = MAXANT * MAXIF
      CALL RFILL (I, 0.0, ANTRMS)
      CALL RFILL (I, 0.0, ANTMAX)
C                                       version numbers
      CALL FNDEXT (TYPE, CATBLK, NVER)
      IF ((IVER.LE.0) .OR. (IVER.GT.NVER)) IVER = NVER
      OVER = NVER + 1
C                                       Smoothing times
      SMOTIM(1) = 1.0E-6
      SMOTIM(2) = 1.0E-6
      SMOTIM(3) = 1.0E-6
C                                       loop over antenna
      DO 100 LA = 1,NA
C                                       skip if no records
         IF (NANREC(LA).LE.0) GO TO 100
C                                       copy the table to sc vers
         MSGSUP = 31999
         OVERAN(LA) = OVER
         CALL TABCOP (TYPE, IVER, OVER, LUN1, LUN2, DISKIN, DISKIN,
     *      CNOIN, CNOIN, CATBLK, BUFF1, BUFF2, IRET)
         MSGSUP = 0
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, LA, 'COPYING TABLE FOR WORK'
            GO TO 990
            END IF
         IF (KEEP.GT.0.0) THEN
            WRITE (MSGTXT,1005) TYPE, OVER, LA
            CALL MSGWRT (3)
            END IF
C                                       open the file
         ICLUN = 28
         NKEY = 0
         NREC = 0
         NCOL = 0
         ICLRNO = 1
         IF (TYPE.EQ.'SN') THEN
            CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, OVER, CATBLK,
     *         ICLUN, ICLRNO, CLKOLS, CLNUMV, NUMA, NUMP, NUMI, NUMN,
     *         GMMOD, RANOD, DECNOD, ISAPPL, IRET)
         ELSE
            CALL CALINI ('WRIT', CLBUFF, DISKIN, CNOIN, OVER, CATBLK,
     *         ICLUN, ICLRNO, CLKOLS, CLNUMV, NUMA, NUMP, NUMI, NUMN,
     *         GMMOD, IRET)
            END IF
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET, LA, 'OPEN SC TABLE'
            GO TO 990
            END IF
         NUMROW = CLBUFF(5)
         NKEY = 9
         LKEY = 24
         CALL FNDCOL (NKEY, KEYS, LKEY, .TRUE., CLBUFF, LKOLS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, LA, 'FIND SC TABLE KEYS'
            GO TO 990
            END IF
         CALL FNDCOL (NKEY, KEYS(10), LKEY, .TRUE., CLBUFF, LKOLS(10),
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, LA, 'FIND SC TABLE KEYS'
            GO TO 990
            END IF
         DO 10 LOOP = 1,18
            IPNT = LKOLS(LOOP)
            LKOLS(LOOP) = CLKOLS(IPNT)
 10         CONTINUE
C                                       re-reference
         DO 30 IA = 1,NA
            IF (NUMREF(IA).GT.0) THEN
               CALL COPY (18, LKOLS, JKOLS)
               DO 20 IIF = BIF,EIF
                  FREQIF = FREQ + FOFF(IIF)
                  CALL CALREF (IA, LA, SUBARR, JKOLS(1), FREQIF, SMOTIM,
     *               NUMROW, CLBUFF, WORK(1), WORK(1+NUMROW),
     *               WORK(1+2*NUMROW), WORK(1+3*NUMROW),
     *               WORK(1+4*NUMROW),IERR)
                  CALL CALREF (IA, LA, SUBARR, JKOLS(10), FREQIF,
     *               SMOTIM, NUMROW, CLBUFF, WORK(1), WORK(1+NUMROW),
     *               WORK(1+2*NUMROW), WORK(1+3*NUMROW),
     *               WORK(1+4*NUMROW), IRET)
                  IF ((IERR.EQ.1) .OR. (IRET.EQ.1)) GO TO 90
                  IRET = MAX (IERR, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1010) IRET, LA,
     *                  'REREFERENCING TO ANTENNA'
                     GO TO 990
                     END IF
C                                       Update column pointers for IF
                  JKOLS(2) = JKOLS(2) + 1
                  JKOLS(4) = JKOLS(4) + 1
                  JKOLS(6) = JKOLS(6) + 1
                  JKOLS(7) = JKOLS(7) + 1
                  JKOLS(8) = JKOLS(8) + 1
                  JKOLS(9) = JKOLS(9) + 1
                  JKOLS(11) = JKOLS(11) + 1
                  JKOLS(13) = JKOLS(13) + 1
                  JKOLS(15) = JKOLS(15) + 1
                  JKOLS(16) = JKOLS(16) + 1
                  JKOLS(17) = JKOLS(17) + 1
                  JKOLS(18) = JKOLS(18) + 1
 20               CONTINUE
               END IF
 30         CONTINUE
C                                       read through table get rms
         I = 2 * MAXANT * MAXIF
         CALL DFILL (I, 0.0D0, VSUM)
         CALL DFILL (I, 0.0D0, VSUMS)
         CALL DFILL (I, 0.0D0, VCOUNT)
         DO 50 ICLRNO = 1,NUMROW
            CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IRET)
            IF (IRET.LT.0) GO TO 50
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1010) IRET, LA, 'READ SC TABLE'
               GO TO 990
               END IF
C                                       Freq id
            IF ((GNRECI(FRQKOL).GT.0) .AND. (GNRECI(FRQKOL).NE.FRQSEL)
     *         .AND. (FRQSEL.GT.0)) GO TO 50
C                                       Subarray
            IF ((GNRECI(SUBKOL).GT.0) .AND. (SUBARR.GT.0) .AND.
     *         (GNRECI(SUBKOL).NE.SUBARR)) GO TO 50
            IANT = GNRECI(ANTKOL)
C                                       Get value
            CALL SNPDAT (VALUE, VALUE2, OKAY)
C                                       Max. - Min
            IF (OKAY) THEN
               IP = 0
               DO 40 IIF = 1,MUMIF
                  IP = IP + 1
                  IF (VALUE(IP).NE.FBLANK) THEN
                     VSUM(1,IANT,IIF) = VSUM(1,IANT,IIF) + VALUE(IP)
                     VSUMS(1,IANT,IIF) = VSUMS(1,IANT,IIF) + VALUE(IP)
     *                  * VALUE(IP)
                     VCOUNT(1,IANT,IIF) = VCOUNT(1,IANT,IIF) + 1.0D0
                     END IF
                  IF (VALUE2(IP).NE.FBLANK) THEN
                     VSUM(2,IANT,IIF) = VSUM(2,IANT,IIF) + VALUE2(IP)
                     VSUMS(2,IANT,IIF) = VSUMS(2,IANT,IIF) + VALUE2(IP)
     *                  * VALUE2(IP)
                     VCOUNT(2,IANT,IIF) = VCOUNT(2,IANT,IIF) + 1.0D0
                     END IF
 40               CONTINUE
               END IF
 50         CONTINUE
C                                       rms
         DO 70 IIF = BIF,EIF
            IP = 0
            DO 60 IA = 1,NA
               IF ((VCOUNT(1,IA,IIF).GT.0.0D0) .AND.
     *            (VCOUNT(2,IA,IIF).GT.0.0D0)) THEN
                  VSUM(1,IA,IIF) = VSUM(1,IA,IIF) / VCOUNT(1,IA,IIF)
                  VSUMS(1,IA,IIF) = VSUMS(1,IA,IIF) / VCOUNT(1,IA,IIF)
     *               - VSUM(1,IA,IIF) * VSUM(1,IA,IIF)
                  VSUM(2,IA,IIF) = VSUM(2,IA,IIF) / VCOUNT(2,IA,IIF)
                  VSUMS(2,IA,IIF) = VSUMS(2,IA,IIF) / VCOUNT(2,IA,IIF)
     *               - VSUM(2,IA,IIF) * VSUM(2,IA,IIF)
                  IP = IP + 1
                  ANTRMS(LA,IIF) = ANTRMS(LA,IIF) + SQRT (MAX (0.0D0,
     *               VSUMS(1,IA,IIF) + VSUMS(2,IA,IIF)))
                  END IF
 60            CONTINUE
            IF (IP.GT.0) ANTRMS(LA,IIF) = ANTRMS(LA,IIF) / IP
 70         CONTINUE
C                                       close and delete
 90      CALL TABIO ('CLOS', 0, ICLRNO, GNREC, CLBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, LA, 'CLOS SC TABLE'
            GO TO 990
            END IF
         IF (KEEP.GT.0.0) THEN
            OVER = OVER + 1
         ELSE
            CALL DELEXT (TYPE, DISKIN, CNOIN, 'WRWR', BUFF1, BUFF2,
     *         OVER, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, LA, 'DELETE SC TABLE'
               GO TO 990
               END IF
            CALL COPY (256, CATSAV, CATBLK)
            CALL ZPHFIL (TYPE, DISKIN, CNOIN, OVER, PHNAME, IERR)
            CALL ZDESTR (DISKIN, PHNAME, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, LA, 'DESTROYING TEMP TABLE'
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       now which is best?
      CALL RFILL (MAXIF, 1.E10, MINRMS)
      CALL FILL (MAXIF, 0, ANTMIN)
      OVRMIN = 0
      OVRMS = 1.E10
      DO 120 LA = 1,NA
         IP = 0
         SUM = 0.0
         DO 110 IIF = BIF,EIF
            IF (ANTRMS(LA,IIF).GT.0.0) THEN
               IP = IP + 1
               SUM = SUM + ANTRMS(LA,IIF)*ANTRMS(LA,IIF)
               IF (ANTRMS(LA,IIF).LT.MINRMS(IIF)) THEN
                  MINRMS(IIF) = ANTRMS(LA,IIF)
                  ANTMIN(IIF) = LA
                  END IF
               END IF
 110        CONTINUE
         IF (IP.GT.0) THEN
            SUM = SQRT (SUM / IP)
            IF (SUM.LT.OVRMS) THEN
               OVRMS = SUM
               OVRMIN = LA
               END IF
            END IF
 120     CONTINUE
      WRITE (MSGTXT,1120) OVRMIN, OVRMS
      CALL MSGWRT (5)
      DO 130 IIF = BIF,EIF
         WRITE (MSGTXT,1125) IIF, ANTMIN(IIF), MINRMS(IIF)
         CALL MSGWRT (5)
 130     CONTINUE
      IIF = BIF
 140  LOOP = MIN (EIF, BIF+7)
      MSGTXT = '*****  R-L rmses by reference antenna and IF *****'
      CALL MSGWRT (4)
      WRITE (MSGTXT,1140) (I, I = IIF,LOOP)
      CALL MSGWRT (4)
      DO 150 LA = 1,NA
         IF (NANREC(LA).GT.0) THEN
            WRITE (MSGTXT,1145) LA, (ANTRMS(LA,I), I = IIF,LOOP)
            CALL MSGWRT (4)
            END IF
 150     CONTINUE
      IIF = LOOP + 1
      IF (IIF.LE.EIF) GO TO 140
C                                       get best SN/CL table
      LA = OVRMIN
      IF (KEEP.LE.0.0) THEN
         OVERAN(LA) = OVER
         CALL TABCOP (TYPE, IVER, OVER, LUN1, LUN2, DISKIN, DISKIN,
     *      CNOIN, CNOIN, CATBLK, BUFF1, BUFF2, IRET)
         MSGSUP = 0
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, LA, 'COPYING TABLE FOR WORK'
            GO TO 990
            END IF
C                                       open the file
         ICLUN = 28
         NKEY = 0
         NREC = 0
         NCOL = 0
         ICLRNO = 1
         IF (TYPE.EQ.'SN') THEN
            CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, OVER, CATBLK,
     *         ICLUN, ICLRNO, CLKOLS, CLNUMV, NUMA, NUMP, NUMI, NUMN,
     *         GMMOD, RANOD, DECNOD, ISAPPL, IRET)
         ELSE
            CALL CALINI ('WRIT', CLBUFF, DISKIN, CNOIN, OVER, CATBLK,
     *         ICLUN, ICLRNO, CLKOLS, CLNUMV, NUMA, NUMP, NUMI, NUMN,
     *         GMMOD, IRET)
            END IF
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET, LA, 'OPEN SC TABLE'
            GO TO 990
            END IF
         NUMROW = CLBUFF(5)
         NKEY = 9
         LKEY = 24
         CALL FNDCOL (NKEY, KEYS, LKEY, .TRUE., CLBUFF, LKOLS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, LA, 'FIND SC TABLE KEYS'
            GO TO 990
            END IF
         CALL FNDCOL (NKEY, KEYS(10), LKEY, .TRUE., CLBUFF, LKOLS(10),
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, LA, 'FIND SC TABLE KEYS'
            GO TO 990
            END IF
         DO 210 LOOP = 1,18
            IPNT = LKOLS(LOOP)
            LKOLS(LOOP) = CLKOLS(IPNT)
 210        CONTINUE
C                                       re-reference
         DO 230 IA = 1,NA
            IF (NUMREF(IA).GT.0) THEN
               CALL COPY (18, LKOLS, JKOLS)
               DO 220 IIF = BIF,EIF
                  FREQIF = FREQ + FOFF(IIF)
                  CALL CALREF (IA, LA, SUBARR, JKOLS(1), FREQIF, SMOTIM,
     *               NUMROW, CLBUFF, WORK(1), WORK(1+NUMROW),
     *               WORK(1+2*NUMROW), WORK(1+3*NUMROW),
     *               WORK(1+4*NUMROW), IERR)
                  CALL CALREF (IA, LA, SUBARR, JKOLS(10), FREQIF,
     *               SMOTIM, NUMROW, CLBUFF, WORK(1), WORK(1+NUMROW),
     *               WORK(1+2*NUMROW), WORK(1+3*NUMROW),
     *               WORK(1+4*NUMROW), IRET)
                  IRET = MAX (IERR, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1010) IRET, LA,
     *                  'REREFERENCING TO ANTENNA'
                     GO TO 990
                     END IF
C                                       Update column pointers for IF
                  JKOLS(2) = JKOLS(2) + 1
                  JKOLS(4) = JKOLS(4) + 1
                  JKOLS(6) = JKOLS(6) + 1
                  JKOLS(7) = JKOLS(7) + 1
                  JKOLS(8) = JKOLS(8) + 1
                  JKOLS(9) = JKOLS(9) + 1
                  JKOLS(11) = JKOLS(11) + 1
                  JKOLS(13) = JKOLS(13) + 1
                  JKOLS(15) = JKOLS(15) + 1
                  JKOLS(16) = JKOLS(16) + 1
                  JKOLS(17) = JKOLS(17) + 1
                  JKOLS(18) = JKOLS(18) + 1
 220              CONTINUE
               END IF
 230        CONTINUE
         END IF
      WRITE (MSGTXT,1150) TYPE, OVERAN(LA), LA
      CALL MSGWRT (5)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNPREF: ERROR =',I3,' ON ',A)
 1005 FORMAT (A,' version ',I4,' will be referenced to antenna',I3)
 1010 FORMAT ('SNPREF: ERROR =',I3,' ANTENNA ',I3,' ON ',A)
 1120 FORMAT ('Best antenna is',I3,' with',1PE12.4,' rms')
 1125 FORMAT ('IF',I3,' best antenna is',I3,' with',1PE12.4,' rms')
 1140 FORMAT ('ANT\\IF',I4,7I7)
 1145 FORMAT (I3,8F7.4)
 1150 FORMAT (A,' version',I4,' is referenced to best antenna',I3)
      END
