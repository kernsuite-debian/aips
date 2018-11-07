LOCAL INCLUDE 'PCCOR.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER MXANS, MXANSF, MXTIME, MXSIFT
      PARAMETER (MXANS = MAXANT*2)
      PARAMETER (MXANSF = MXANS*MAXIF)
      PARAMETER (MXTIME = 750)
      PARAMETER (MXSIFT = MAXIF*MXTIME)
C                                       XCALS was (4,1) but in POPSDAT
C                                       it's got 30 elements in the
C                                       second dimension.  PPM 19961001
      HOLLERITH  XNAMEI(3), XCLAIN(2), XCALS(4,30)
      REAL XSIN, XDISIN, TIMER(8), XSNVER, XPCVER, XREFA, XSUBA,
     *   XFQID, XOFF, XBIF, XEIF, XDELC, UTONES(2,MAXAFL),
     *   BUFF1(UVBFSS), TIME1, TIME2,
     *   GNMOD, DELAR(MXANSF), ADELY(MXANS), IFTBW(MAXIF),
     *   IFCHBW(MAXIF), F1(MAXIF), DFREQ(MAXIF)
      INTEGER NTWOPI
      INTEGER SNKOLS(MAXSNC), SNNUMV(MAXSNC), ISNRNO, NUMNOD,
     *   SUBA, SNV, SNLUN, SNBUFF(1024), PCBUFF(1024), NPOLZN,
     *   OLDCNO, PCVER, PCLUN, PCTOT, REFANT, NSRC, SELQUA(30),
     *   SOURID(30), NANPC(MAXANT), NTIME, RANT, NNMAX, FREQID,
     *   CABOFF, NNNIF, IBIF, IEIF, MDELC
      INTEGER SEQIN, DISKIN, NUMHIS, JBUFSZ, ILOCWT, CATOLD(256),
     *   INCSI, INCFI, INCIFI, LRECI, NRPRMI, NANT, ANTNO(MAXANT),
     *   NNIF, NNF, NCORI, NUMPC, SIND, IFSID(MAXIF),
     *   TONE1(MAXIF), TONE2(MAXIF)
      DOUBLE PRECISION AFREQ, GST0, XA(MAXANT),YA(MAXANT),ZA(MAXANT),
     *   DCABL(MAXANT),IFFREQ(MAXIF)
      LOGICAL   ISCOMP, ISAPPL, SOUCHK, PCYES(MXANSF), DATNO(MAXANT)
      CHARACTER NAMEIN*12, CLAIN*6, HISCRD(10)*64, STANAM(MAXANT)*8,
     *   CALSOU(1)*16
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, TIMER, XSNVER,
     *   XPCVER, XREFA, XSUBA,  XFQID, XCALS, XOFF, XBIF, XEIF,
     *   XDELC, UTONES
      COMMON /PCPARM/ CATOLD, SEQIN, DISKIN, NUMHIS, ILOCWT, INCSI,
     *   INCFI, INCIFI, LRECI, NRPRMI, ISCOMP, NNIF, NNF, NCORI, TIME1,
     *   TIME2, NANT, NPOLZN, OLDCNO, SIND, DELAR, ADELY, NANPC, NTIME,
     *   CABOFF, NNNIF, RANT, F1, DFREQ, NNMAX, SOUCHK, PCYES, DATNO,
     *   NTWOPI, IBIF, IEIF
      COMMON /CHARPM/ NAMEIN, CLAIN, HISCRD, STANAM, CALSOU
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /CANIN/ XA, YA, ZA, AFREQ, GST0, DCABL, ANTNO, IFFREQ
      COMMON /SNPARM/ SNBUFF, PCBUFF, SNKOLS, SNNUMV, ISNRNO, NUMNOD,
     *   SNV, SNLUN, PCLUN, GNMOD, ISAPPL, PCVER, NUMPC, PCTOT,
     *   REFANT, NSRC, SELQUA, SOURID, IFTBW, IFCHBW, IFSID, FREQID,
     *   SUBA, TONE1, TONE2, MDELC
LOCAL END
      PROGRAM PCCOR
C-----------------------------------------------------------------------
C! Correct antennas gains using PC table data
C# Utility UV UV-util VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2003, 2009, 2011-2012, 2014-2015, 2018
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
C   PCCOR corrects antennas gains using cal tones information
C   recorded in PC table.
C   This is a new version which works better for uncontinuos IFs
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      TIMERANG(8)    TIMER          Timerange of the calibrator
C                                   1-4 = start Day, Hour, Min, Sec.
C                                   5-8 =  end  Day, Hour, Min, Sec.
C      SNVER          SNV           The SN table number
C      PCVER          PCVER         The PC table number
C      REFANT         REFANT        A reference antenna
C      CALSOUR        CALSOU        Calibrator source
C      SUBARRAY       SUBA          Subarray
C      FREQID         FREQID        Frequency ID
C      CUTOFF         CABOFF        Switch off the cabel correction
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'PCCOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
C
      DATA PRGM /'PCCOR '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL PCCIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Calculate corrections for
C                                       the calibrator
      CALL PCCAL (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       apply the corrections for
C                                       target sources and
C                                       record them into SN table
      CALL PCTAG (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL PCCHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE PCCIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   PCCIN gets input parameters for PCCOR
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code:  0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      ISCOMP  L  If true data is compressed
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, UTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, CALCS(1), I, SUBAR,
     *   KEYLOC, KEYTYP, ORIGIN, NUMKEY, IDN(1), NSOUR, NID
      REAL      DECNOD(25), RANOD(25), TEMP
      LOGICAL   T, F, CHSTAT, MULTI, TABLE, EXIST, FITASC
C
      INCLUDE 'INCS:PPCV.INC'
C
      INTEGER   IPCRNO, NOPOLZ, NTONES, NOBAND
      INTEGER   PCKOLS(MAXPCC), PCNUMV(MAXPCC)
C
      INCLUDE 'PCCOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
C                                       Fixed PPM 1996.09.30: was 24!!!
C                                       add MAXAFL*2 for the arrays of
C                                       user tones UTONES
      NPARM = 144 + MAXAFL*2
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999

      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBA = IROUND(XSUBA)
      FREQID = IROUND (XFQID)
      IF (FREQID.LE.0) FREQID = 1
      CABOFF = IROUND(XOFF)
      MDELC = IROUND(XDELC)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   UTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Check to see if PC table exists
      CALL FNDEXT ('PC', CATBLK, PCTOT)
      IF (PCTOT.EQ.0) THEN
         WRITE (MSGTXT,1040)
         JERR = 1
         GO TO 990
         END IF
C                                       PC number
      PCVER = IROUND(XPCVER)
      PCLUN = 30
      CALL PCINI ('READ', PCBUFF, DISKIN, OLDCNO, PCVER, CATBLK,
     *   PCLUN, IPCRNO, PCKOLS, PCNUMV, NOPOLZ, NOBAND, NTONES,
     *   JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1090) JERR
         GO TO 990
         END IF
      NUMPC = PCBUFF(5)
      CALL TABIO ('CLOS', 0, IPCRNO, PCBUFF, PCBUFF, JERR)
      JERR = 0
C                                       Set time range in days.
      TIME1 = TIMER(1)+TIMER(2)/24.+TIMER(3)/(24.*60.)
     *   +TIMER(4)/(24.*3600.)
      TIME2 = TIMER(5)+TIMER(6)/24.+TIMER(7)/(24.*60.)
     *   +TIMER(8)/(24.*3600.)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
      IF (ISCOMP) THEN
C                                       Find weight and scale.
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      NNF = 1
      IF (JLOCF.GE.0) NNF = CATBLK(KINAX+JLOCF)
      NNNIF = 1
      IF (JLOCIF.GE.0) NNNIF = CATBLK(KINAX+JLOCIF)
      IF (JLOCIF.GE.0) THEN
         IBIF = IROUND (XBIF)
         IBIF = MAX (1, MIN (IBIF, CATBLK(KINAX+JLOCIF)))
         IEIF = IROUND (XEIF)
         IF (IEIF.EQ.0) IEIF = CATBLK(KINAX+JLOCIF)
         IF (IBIF.GT.IEIF) IEIF = CATBLK(KINAX+JLOCIF)
         IEIF = MAX (1, MIN (IEIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         IBIF = 1
         IEIF = 1
         END IF
      NNIF = IEIF - IBIF + 1
C                                       PCAL can not be used if NNF or
C                                       NNIF.EQ.1
      IF (NNIF.LE.1) THEN
         WRITE (MSGTXT,1100) NNIF
         JERR = 1
         GO TO 990
         END IF
      IF (NNF.LE.1) THEN
         WRITE (MSGTXT,1110) NNF
         JERR = 1
         GO TO 990
         END IF
      TEMP = NNF * CATR(KRCIC+JLOCF) / 1.E6
      IF ((TEMP.LT.2.0*NTONES) .AND. (NTONES.GT.4)) THEN
         MSGTXT = 'YOU HAVE A LARGE PC TABLE: USE PCFLG OR PCRMS TO' //
     *      ' EDIT IT'
         CALL MSGWRT (7)
         MSGTXT = 'THEN USE PCFIT TO DETERMINE DELAY CHANGES WITH TIME'
         CALL MSGWRT (7)
         MSGTXT = 'SEE AIPS MEMO 123 FOR THE DETAILS'
         CALL MSGWRT (7)
         MSGTXT = 'YOU WILL STILL NEED TO USE FRING AS WELL'
         CALL MSGWRT (7)
         JERR = 10
         GO TO 999
         END IF

C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                        Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   UTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Prepare antennas data
      SUBAR = SUBA
      IF (SUBAR.LE.0) SUBAR = 1
      CALL ANTDAT (SUBAR, DISKIN, OLDCNO, IERR)
      IF (IERR.NE.0) THEN
         JERR = 1
         WRITE (MSGTXT, 1120)
         GO TO 990
         END IF
C                                       Refant
      REFANT = IROUND(XREFA)
      IF (REFANT.EQ.0) REFANT = ANTNO(1)
C                                       Calibrator and its ID
      CALL H2CHR (16, 1, XCALS(1,1), CALSOU(1))
C                                       use source selection?
      CALCS(1) = -1
      NSOUR = 1
      NID = 1
      CALL SOURNU (CALSOU, CALCS, NSOUR, DISKIN, OLDCNO, NID,
     *   BUFF1, IDN, IERR)
      IF (IERR.NE.0) THEN
         JERR = 1
         WRITE (MSGTXT,1130) CALSOU
         GO TO 990
         END IF
      SOUCHK = NID.GT.0
      SIND = IDN(1)
      UTYPE = 'UV'
      CHSTAT = .FALSE.
      CALL CATDIR ('INFO', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *   SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
C                                       Change status
      IF (STAT.EQ.  'READ') THEN
         STAT = 'CLRD'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *      SEQIN, UTYPE, NLUSER, STAT, BUFF1 , IERR)
         IF (IERR.NE.0) THEN
            JERR = 1
            WRITE (MSGTXT,1140) IERR, STAT
            GO TO 990
            END IF
         STAT = 'WRIT'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *      SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            JERR = 1
            WRITE (MSGTXT,1140) IERR, STAT
            GO TO 990
            END IF
         CHSTAT = .TRUE.
         END IF
C
      ISAPPL = F
      GNMOD = 1.0
      DO 60 I = 1, 25
         RANOD(I) = 0.0
         DECNOD(I) = 0.0
   60    CONTINUE
      SNLUN = 48
      NPOLZN = 2
      IF (NCOR.EQ.1) NPOLZN = 1
      NUMNOD = 0
C                                       SNVER
      SNV = IROUND(XSNVER)
C                                       terminate if the given SN table
C                                       exists
      IF (SNV.NE.0) THEN
         CALL ISTAB ('SN', DISKIN, OLDCNO, SNV, SNLUN, BUFF1, TABLE,
     *      EXIST, FITASC, JERR)
         IF (EXIST) THEN
            JERR = 1
            WRITE (MSGTXT, 1080) SNV
            GO TO 990
            END IF
         END IF
C                                       Open SN table
      CALL SNINI ('WRIT', SNBUFF, DISKIN, OLDCNO, SNV, CATBLK, SNLUN,
     *   ISNRNO, SNKOLS, SNNUMV, NANT, NPOLZN, NNNIF, NUMNOD,
     *   GNMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) THEN
         JERR = 1
         WRITE (MSGTXT,1010) SNV
         GO TO 990
         END IF
      ISNRNO = 1
      WRITE (MSGTXT,1070) SNV
      CALL MSGWRT (8)
C                                       history file
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2100) (TIMER(I),I=1,8)
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2200) CALSOU(1)
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2300) SNV, PCVER
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2500) REFANT, SUBA
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2600) FREQID
C                                       Add the ORIGIN keyword
      CALL MULSDB (CATBLK, MULTI)
      KEYLOC = 1
      KEYTYP = 4
      ORIGIN = 0
      NUMKEY = 1
      IF (.NOT.MULTI) ORIGIN = 1
      CALL TABKEY ('WRIT', 'SNORIGIN', NUMKEY, SNBUFF, KEYLOC,
     *   ORIGIN, KEYTYP, IERR)
      IF (IERR.NE.0) THEN
         JERR = 1
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Check if changed status
      IF (CHSTAT) THEN
         STAT = 'CLWR'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *      SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            JERR = 1
            WRITE (MSGTXT,1140) IERR, STAT
            GO TO 990
            END IF
         STAT = 'READ'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *      SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1140) IERR, STAT
            GO TO 990
            END IF
         END IF
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('PCCIN: ERROR in initialization of SN table ',I3)
 1020 FORMAT ('PCCIN: ERROR',I3,' READING KEYWORDS FROM SN TABLE')
 1030 FORMAT ('PCCIN: UNABLE TO DETERMINE DATA STOP TIME')
 1040 FORMAT ('No PC table found')
 1050 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1060 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('Writing to SN table ',I3)
 1080 FORMAT ('SN table', I3,' already exists')
 1090 FORMAT ('PCCIN: ERROR ',I3,' OPENING PC TABLE')
 1100 FORMAT ('Data have',I3, ' IFs. It is too small.')
 1110 FORMAT ('Data have',I3,' frequencies per IF. It is too small.')
 1120 FORMAT ('PCCIN: PROBREM OF READING DATA FROM ANTENNA TABLE')
 1130 FORMAT ('PCCIN: Source ',A16, ' not found in the SU table')
 1140 FORMAT ('PCCIN: ERROR ',I3,' CHANGING ',A4,' STATUS')
 2100 FORMAT (F3.0,1X,F3.0,F3.0,F4.1,' - ',F3.0,1X,F3.0,F3.0,F4.1,
     *   '/ Time range')
 2200 FORMAT ('Calibrator = ', A16)
 2300 FORMAT ('SN table =',I3, 2X, 'PC table =',I3)
 2500 FORMAT ('Reference antenna =',I4, 2X,' Subarray =', I2)
 2600 FORMAT ('Freqid =',I3)
      END
      SUBROUTINE PCCAL (IRET)
C-----------------------------------------------------------------------
C   PCCAL estimates single and multi band delays using the both
C   PC table data of calibrator and visibilities of calibrator
C   Input in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      ISCOMP  L  If true data is compressed
C      NNIF    I  Number of IFs
C      NF      I  Number of frequencies in each IF
C      NPOLZN  I  Number of polarization in antennas
C      TIME1   R  The start time of calibrator, in days
C      TIME2   R  The end time of calibrator, in days
C
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C   Output in common:
C      DELAR   R(*) single band delay for each stokes, IF, antenna
C                   which align phase for all IFs, using the calibrator
C      ADELY   R(*) additional delay for each stokes and antenna
C                   constant for all IF. This delay
C                   flatten the align phase for all IF.
C      DCABL   D(*) Mean delay in the cable for each
C                   antenna while observing the calibrator
C                   depending on IF, polarisation and time.
C      NTIME   I    Number of reference antenna appearences in PC table.
C      RANT    I    Reference antenna number
C-----------------------------------------------------------------------
      INCLUDE 'PCCOR.INC'
      INTEGER MXTOT, MJF, MXTONE
      PARAMETER (MXTOT = 100000)
      PARAMETER (MJF = 2*MAXCHA)
      PARAMETER (MXTONE = MAXIF*8)
      CHARACTER  IFILE*48, POL(2)*3
      INTEGER   INIO, IPTRI, LUNI, INDI, ILENBU, IBIND, I, IA1, IA2,
     *   INCX, BO, VO, JIF, JF, JS, IANT, INDEXI, IRET, ITIME, FQID,
     *   LUNFQ, ADR, IND1, IND2, IROW, NBASE, NMAX, LANT, MXT, MXOUR,
     *   CURSUB, CURSOU, ITONE, KTONES, KIF, LTONE
      LOGICAL   T, F, SOUFOU, FIRST, BAD
      REAL  BASEN,  CBUFF(UVBFSS), REAL1(MXANSF), IMAG1(MXANSF),
     *   REAL2(MXANSF), IMAG2(MXANSF), RE, IM, REOUT(MXTOT), WT, FA,
     *   FPULS, DATA(MJF), DELL, IMOUT(MXTOT), AMPP, DDAMP, DDEL,
     *   IDATA(MXTONE), FIIN1(MXANSF), FIIN2(MXANSF), DELIF(MAXIF),
     *   FTONE(MXTONE), FIACT1, FIACT2, FICOR1, FICOR2, DFREQM,
     *   DFTONM, DFA, COEFF, TAUA, ATANG2, DFI, RPIX
      DOUBLE PRECISION MXFTON, MNFTON
      CHARACTER BNDCOD(MAXIF)*8
C
      INCLUDE 'INCS:PPCV.INC'
C
      INTEGER   IPCRNO, NOPOLZ, NTONES
      INTEGER   NOSTA, IARRAY, IFQID, ISRC
      REAL      TINT, PCREL(2, MAXTON, MAXIF),
     *   PCIMG(2,  MAXTON,MAXIF), PCRAT(2, MAXTON, MAXIF),
     *   STATE(2, 4, MAXIF)
      DOUBLE PRECISION TIME, CABLCL, PCFRQ(2, MAXTON, MAXIF), PCABLC
      INTEGER   PCKOLS(MAXPCC), PCNUMV(MAXPCC)
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DGLB.INC'
C
      DATA FPULS /1.0E6/
      DATA LUNI /16/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      PCABLC = 0.0D0
      CALL PCINI ('READ', PCBUFF, DISKIN, OLDCNO, PCVER, CATBLK,
     *   PCLUN, IPCRNO, PCKOLS, PCNUMV, NOPOLZ, NOBAND, NTONES,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       STOP: number of tones is .LT.2
      IF (NTONES.LT.2) THEN
         IRET = 1
C         IF (2*NOBAND*NOPOLZ.GT.16) THEN
C            WRITE (MSGTXT,1005) NOBAND, NOPOLZ
C            CALL MSGWRT (8)
C            WRITE (MSGTXT,1006)
C            GO TO 990
C         ELSE
            WRITE (MSGTXT,1007)
            GO TO 990
C            END IF

         END IF


      DFTONM = FPULS
      IF (2*NNF.GT.MJF)  THEN
         IRET = 1
         WRITE (MSGTXT,1010) NNF, MJF/2
         GO TO 990
         END IF
C                                       Get FQ table information
      FQID = FREQID
      LUNFQ = 40
      CALL GETFQ (FQID, DISKIN, OLDCNO, CATBLK, LUNFQ, IFFREQ, IFTBW,
     *   IFCHBW, IFSID, BNDCOD, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
      IF (ISCOMP) INCX = 3
C                                       Number of visibilities in input
      NCORI = (LRECI - NRPRMI) / CATOLD(KINAX)
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
C                                       reference antenna number
      DO 40 IANT = 1, NANT
         IF (ANTNO(IANT).EQ.REFANT) GO TO 50
   40    CONTINUE
      IRET = 1
      WRITE (MSGTXT,1050) REFANT
      GO TO 990
   50 CONTINUE
      RANT = IANT
C
C                                       initialize real and image of
C                                       the tones and cablcal
      DO 70 I = 1, NANT
         DCABL(I) = 0.0D0
         NANPC(I) = 0
         DO 65 JIF = 1,NNNIF
            DO 60 JS = 1,NOPOLZ
               ADR = JS + (JIF-1)*NOPOLZ + (I-1)*NOPOLZ*NNNIF
               REAL1(ADR) = 0.0
               IMAG1(ADR) = 0.0
               REAL2(ADR) = 0.0
               IMAG2(ADR) = 0.0
   60          CONTINUE
   65          CONTINUE
   70    CONTINUE
      SOUFOU = F
C                                       Read the data from PC table
C                                       and average it through the
C                                       given timerange
C
C                                       Introduce TONE1(JIF), TONE2(JIF)
C                                       and IFSID(JIF) to handle both
C                                       LSB and USB
      ITIME = 0
      FIRST = .TRUE.
      DO 110 IROW = 1, NUMPC
         CALL TABPC ('READ', PCBUFF, IPCRNO, PCKOLS, PCNUMV,
     *      NOPOLZ, TIME, TINT, ISRC, NOSTA, IARRAY, IFQID,
     *      CABLCL, STATE, PCFRQ, PCREL, PCIMG, PCRAT, IRET)
         IF (IRET.NE.0) THEN
            CALL TABERR ('READ', 'TABPC ', 'PCCAL ', IRET)
            GO TO 999
            END IF
         IF (CABLCL.EQ.DBLANK) THEN
            CABLCL = PCABLC
         ELSE
            PCABLC = CABLCL
            END IF
C                                       take frequencies for the given
C                                       FREQID only at the first
C                                       appearence of the FREQID
         IF (FREQID.NE.IFQID) GO TO 110


C                                       Select subarray, FREQID
         IF ((SUBA.NE.IARRAY) .AND. (SUBA.NE.0)) GO TO 110
C                                       select time for calibrator
         IF (TIME.GT.TIME2  .OR.  TIME.LT.TIME1)
     *      GO TO 110
C                                       select calibrator source
         IF (SOUCHK) THEN
            IF (ISRC.NE.SIND) GO TO 110
            SOUFOU = SOUFOU .OR. (ISRC.EQ.SIND)
         ELSE
            SOUFOU = .TRUE.
            END IF
         DO 90 IANT = 1, NANT
            IF (ANTNO(IANT).EQ.NOSTA) GO TO 95
   90    CONTINUE
         GO TO 110
   95    CONTINUE
         IF (FIRST) THEN
            DFREQM = 0.0
            DO 80 JIF = 1,NNNIF
C                                       take the most left and right
C                                       tones if there are more than two
               MXFTON = 0
               MNFTON = 1.0E20
               LTONE = 0
               DO 75 ITONE = 1, NTONES
C                                       skip bad rows at the PC table
                  BAD = (PCFRQ(1, ITONE, JIF).LE.0) .OR.
     *               (PCREL(1, ITONE, JIF).EQ.0 .AND.
     *               PCIMG(1, ITONE, JIF).EQ.0) .OR.
     *               (PCREL(1, ITONE, JIF).EQ.FBLANK) .OR.
     *               (PCIMG(1, ITONE, JIF).EQ.FBLANK)
C                                       count the good tones number
C                                       for the given IF
                  IF (BAD) THEN
                     GO TO 75
                  ELSE
                     LTONE = LTONE + 1
                     END IF
                  IF (PCFRQ(1, ITONE, JIF).GT.MXFTON) THEN
                     MXFTON = PCFRQ(1, ITONE, JIF)
                     TONE2(JIF) = ITONE
                     END IF
                  IF (PCFRQ(1, ITONE, JIF).LT.MNFTON) THEN
                     MNFTON = PCFRQ(1, ITONE, JIF)
                     TONE1(JIF) = ITONE
                     END IF
   75             CONTINUE
C                                       Enter the input TONE1, TONE2
               IF (INT(UTONES(1,JIF)) .GT. 0 .AND.
     *            INT(UTONES(1,JIF)) .LE. NTONES)
     *            TONE1(JIF) = UTONES(1,JIF)

               IF (INT(UTONES(2,JIF)) .GT. 0 .AND.
     *            INT(UTONES(2,JIF)) .LE. NTONES)
     *            TONE2(JIF) = UTONES(2,JIF)
C                                       go to the next row if at least
C                                       one IF has only one (or 0) tones
               IF (LTONE.LT.2) GO TO 110
               IFSID(JIF) = 1
               DFREQ(JIF) = PCFRQ(1, TONE2(JIF), JIF) -
     *            PCFRQ(1, TONE1(JIF), JIF)
C                                       change sign of the Fourie if
C                                       the last tone frequency is less
C                                       the first one
               IF ((PCFRQ(1, LTONE, JIF) - PCFRQ(1, 1, JIF)).LE.0)
     *            IFSID(JIF) = -1
               F1(JIF) = PCFRQ(1, TONE1(JIF), JIF) - IFFREQ(JIF)
C                                       Find the maximum of DFREQ
               IF(DFREQ(JIF).GE.DFREQM) DFREQM = DFREQ(JIF)
   80          CONTINUE
            FIRST = .FALSE.
            END IF
         LANT = IANT
C                                       average cablecal data
         DCABL(LANT) = DCABL(LANT) + CABLCL
         NANPC(LANT) = NANPC(LANT) + 1
         DO 105 KIF = 1, NNIF
C                                       take only selected IFs
C                                       For all other IFs REAL, IMAG
C                                       stay zero
            JIF = KIF + IBIF - 1
C                                       average complex amplitudes
C                                       of tones in time
            DO 100 JS = 1,NOPOLZ
               ADR = JS + (JIF-1)*NOPOLZ + (LANT-1)*NOPOLZ*NNNIF
C                                       skip bad rows of PC table
               BAD = (PCIMG(JS, TONE1(JIF),JIF).EQ.0.0 .AND.
     *            PCREL(JS, TONE1(JIF),JIF).EQ.0) .OR.
     *            (PCIMG(JS, TONE2(JIF),JIF).EQ.0.0 .AND.
     *            PCREL(JS, TONE2(JIF),JIF).EQ.0) .OR.
     *            (PCREL(JS, TONE1(JIF),JIF).EQ.FBLANK) .OR.
     *            (PCIMG(JS, TONE1(JIF),JIF).EQ.FBLANK) .OR.
     *            (PCREL(JS, TONE2(JIF),JIF).EQ.FBLANK) .OR.
     *            (PCIMG(JS, TONE2(JIF),JIF).EQ.FBLANK)
               IF (BAD) GO TO 100
               REAL1(ADR) = REAL1(ADR) + PCREL(JS, TONE1(JIF),JIF)
               IMAG1(ADR) = IMAG1(ADR) + PCIMG(JS, TONE1(JIF),JIF) *
     *               IFSID(JIF)
               REAL2(ADR) = REAL2(ADR) + PCREL(JS, TONE2(JIF),JIF)
               IMAG2(ADR) = IMAG2(ADR) + PCIMG(JS, TONE2(JIF),JIF) *
     *            IFSID(JIF)
  100          CONTINUE
  105       CONTINUE
  110    CONTINUE
      NTIME = ITIME
C                                       Is there at least one good line
C                                       in PC table?
      IF(FIRST) THEN
         IRET = 1
         WRITE (MSGTXT,1090)
         GO TO 990
         END IF
C                                       Is there selected source
C                                       in PC table?
      IF(.NOT. SOUFOU) THEN
         IRET = 1
         WRITE (MSGTXT,1080) CALSOU(1)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1085)
         GO TO 990
         END IF
C                                       Is there PC tones?
      POL(1) = 'RCP'
      POL(2) = 'LCP'
      DO 140 IANT = 1, NANT
         IF (NANPC(IANT).NE.0) THEN
            DCABL(IANT) = DCABL(IANT) / NANPC(IANT)
         ELSE
            WRITE (MSGTXT,1095) IANT
            CALL MSGWRT (8)
            GO TO 140
            END IF
         DO 130 KIF = 1,NNIF
            JIF = KIF + IBIF - 1
            DO 120 JS = 1,NOPOLZ
               ADR = JS + (JIF-1)*NOPOLZ + (IANT-1)*NOPOLZ*NNNIF
               PCYES(ADR) = .TRUE.
               IF ((REAL1(ADR).EQ.0.0 .AND. IMAG1(ADR).EQ.0.0)
     *            .OR. (REAL2(ADR).EQ.0.0.AND.IMAG2(ADR).EQ.0.0))
     *            THEN
                  PCYES(ADR) = .FALSE.
                  WRITE (MSGTXT,1100) IANT, POL(JS), JIF
                  CALL MSGWRT (8)
                  END IF
  120          CONTINUE
  130       CONTINUE
  140    CONTINUE
C                                       Find the ambiguaty range
      NMAX = DFREQM / FPULS
      NMAX = MAX(1, NMAX)
      NMAX = 2*NMAX
C                                       the range is doubled because
C                                       we compare the difference
C                                       between two antennas
      NNMAX = 2*NMAX + 1
C                                       enough demension of arrays
C                                       REOUT(), IMOUT()?
      MXT = MXTOT
      MXOUR = NANT*NNNIF*NPOLZN*NNF
      IF (MXOUR.GT.MXT) THEN
         IRET = 1
         WRITE (MSGTXT,1110) MXOUR, MXT
         GO TO 990
         END IF
      DO 200 IANT = 1, NANT
         IF (NANPC(IANT).EQ.0) GO TO 200
         DO 180 KIF = 1,NNIF
            JIF = KIF + IBIF - 1
            DO 160 JS = 1,NOPOLZ
               ADR = JS + (JIF-1)*NOPOLZ + (IANT-1)*NOPOLZ*NNNIF
               IF (PCYES(ADR)) THEN
                  FIIN1(ADR) = ATANG2(IMAG1(ADR), REAL1(ADR))
                  FIIN2(ADR)=  ATANG2(IMAG2(ADR), REAL2(ADR))
                  END IF
  160          CONTINUE
  180       CONTINUE
  200    CONTINUE
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
      NBASE = (NANT * (NANT-1)) / 2
C                                       Initialize ampl. and phase to
C                                       zero before averaging in time
      DO 280 IANT = 1, NANT
         DO 260 JIF = 1,NNNIF
            DO 240 JS = 1,NPOLZN
               DO 220 JF = 1,NNF
                  ADR = JS + (JIF-1)*NPOLZN + (JF-1)*NPOLZN*NNNIF
     *               + (IANT-1)*NPOLZN*NNNIF*NNF
                  REOUT(ADR) = 0.0
                  IMOUT(ADR) = 0.0
  220             CONTINUE
  240             CONTINUE
  260       CONTINUE
  280    CONTINUE
C----------------------------------------------------------------
C                                       average in time visibilities
C                                       of interferometers with
C                                       reference antenna for
C                                       selected calibrator and time
  300 CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1140) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Out of data?
         IF (INIO.LE.0) GO TO 500
C                                       Loop over buffer
         DO 480 I = 1,INIO
            IF ( BUFF1(IPTRI+ILOCT).LT.TIME1 ) GO TO 460
            IF ( BUFF1(IPTRI+ILOCT).GT.TIME2 ) GO TO 500
            IF (ILOCB.GE.0) THEN
               BASEN = BUFF1(IPTRI+ILOCB)
               IA1 = BASEN / 256. + 0.1
               IA2 = BASEN - IA1*256. + 0.1
C                                       check subarray
               CURSUB = 1.1 + 100.0 * AMOD(BASEN, 1.0)
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               CURSUB = BUFF1(IPTRI+ILOCSA) + 0.1
               END IF
            IF ((CURSUB.NE.SUBA) .AND. (SUBA.NE.0)) GO TO 460
C                                       check cal. source
            CURSOU = SIND
            IF (ILOCSU.GE.0) CURSOU = BUFF1(IPTRI+ILOCSU) + 0.5
            IF ((CURSOU.NE.SIND) .AND. SOUCHK) GO TO 460
C                                       take only cros correlation
            IF (IA1.EQ.IA2) GO TO 460
C                                       take baselines with ref. ant.
            IF (IA1.NE.REFANT) THEN
               IF (IA2.NE.REFANT) GO TO 460
               DO 320 IANT = 1, NANT
                  IF (ANTNO(IANT).EQ.IA1) GO TO 340
  320             CONTINUE
               GO TO 460
  340          CONTINUE
               LANT = IANT
               COEFF = -1
            ELSE
               DO 360 IANT = 1, NANT
                  IF (ANTNO(IANT).EQ.IA2) GO TO 380
  360             CONTINUE
               GO TO 460
  380          CONTINUE
               LANT = IANT
               COEFF = 1
               END IF
C                                       Compressed data.
            IF (ISCOMP)  CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *         BUFF1(IPTRI+ILOCWT), CBUFF)
            DO 440 KIF = 1,NNIF
               JIF = KIF + IBIF - 1
                  DO 420 JS = 1,NPOLZN
                     DO 400 JF = 1,NNF
                        ADR = JS + (JIF-1)*NPOLZN + (JF-1)*NPOLZN*NNNIF
     *                     + (LANT-1)*NPOLZN*NNNIF*NNF
                        INDEXI = (JIF-1) * INCIFI + (JF-1) *
     *                     INCFI + (JS-1) * INCSI + 1
                        IF (ISCOMP) THEN
                           RE = CBUFF(1 + (INDEXI - 1)*3 )
                           IM = CBUFF(2 + (INDEXI - 1)*3 )*COEFF
                           WT = CBUFF(3 + (INDEXI -1)*3 )
                        ELSE
                           RE = BUFF1(IPTRI + NRPRMI +
     *                        (INDEXI -1)*3 )
                           IM = BUFF1(IPTRI + NRPRMI + 1 +
     *                        (INDEXI -1)*3 ) * COEFF
                           WT = BUFF1(IPTRI + NRPRMI + 2 +
     *                        (INDEXI -1)*3 )
                           END IF
C                                       average in time
                        REOUT(ADR) = REOUT(ADR) + RE*WT
                        IMOUT(ADR) = IMOUT(ADR) + IM*WT
  400                   CONTINUE
  420             CONTINUE
  440          CONTINUE
  460          IPTRI = IPTRI + LRECI
  480       CONTINUE
C                                       Read next buffer.
         GO TO 300
  500 CONTINUE
C                                       check the data exist for all
C                                       antennas with reference one
      DO 550 IANT = 1, NANT
         DATNO(IANT) = .FALSE.
         IF(IANT.EQ.RANT) GO TO 550
         ADR =  1 + (IBIF-1)*NPOLZN + (IANT-1)*NPOLZN*NNNIF*NNF
         IF ((REOUT(ADR).EQ.0.0) .AND.
     *      (IMOUT(ADR).EQ.0.0)) THEN
            DATNO(IANT) = .TRUE.
            WRITE (MSGTXT,1150) ANTNO(IANT), ANTNO(IANT), ANTNO(RANT)
            CALL MSGWRT (8)
            END IF
  550    CONTINUE
C-----------------------------------------------------------------
C      Estimate 'multiband' delay TAUA combining phases of data
C      at pc tones location. Estimate requied single band delays
C      DEL as a difference of phase slopes of data DELIF and TAUA
C-----------------------------------------------------------------
C                                       Store everything for reference
C                                       antenna
       IF ((NANPC(RANT).EQ.0) .OR. DATNO(RANT)) GO TO 600
       DO 580 JS = 1, NPOLZN
C
          IND1 = JS + (RANT-1)*NPOLZN
          ADELY(IND1) = 0
C
          DO 570 KIF = 1, NNIF
             JIF = KIF + IBIF - 1
C
             IND2 = JS + (JIF-1)*NPOLZN + (RANT-1)*NPOLZN*NNNIF
             DFI = FIIN2(IND2) - FIIN1(IND2)
             IF (DFI.GT.PI) DFI = DFI - TWOPI
             IF (DFI.LE.-PI) DFI = DFI + TWOPI
C                                       store delay at the reference
C                                       scan for reference antenna
             DELAR(IND2) = DFI / DFREQ(JIF) / TWOPI
  570        CONTINUE
  580     CONTINUE
C
  600 CONTINUE
C                                       reference pixel of the
C                                       reference frequency
C
      RPIX = CATR(KRCRP+1)
C                                       tones' delay of other
C                                       (not reference) antennas
      DO 660 IANT = 1, NANT
         IF ((NANPC(IANT).EQ.0) .OR. DATNO(IANT)) GO TO 660
         IF (IANT.EQ.RANT)  GO TO 660
         DO 650 JS = 1, NPOLZN
C
            ITONE = 0
C                                       calculate phases of data
C                                       at the tones points corrected
C                                       by pccal data
            DO 640 KIF = 1, NNIF
               JIF = KIF + IBIF - 1
               DO 615 JF = 1, NNF
                  ADR = JS + (JIF-1)*NPOLZN + (JF-1)*NPOLZN
     *               *NNNIF + (IANT-1)*NPOLZN*NNNIF*NNF
                  DATA(2*JF-1) = REOUT(ADR)
                  DATA(2*JF)   = IMOUT(ADR)
  615             CONTINUE
C                                       estimate amplitude, rate and
C                                       phase of the data
               CALL AFRFI (DATA, NNF, AMPP, DELL, FA, DDAMP, DDEL,
     *               DFA, IRET)
C                                       store delays for each IF
               DELIF(JIF) = DELL / IFCHBW(JIF)
               IND1 = JS + (JIF-1)*NPOLZN + (RANT-1)*NPOLZN*NNNIF
               IND2 = JS + (JIF-1)*NPOLZN + (IANT-1)*NPOLZN*NNNIF
C                                       the first tone
               ITONE = ITONE + 1
               FTONE(ITONE) = PCFRQ(JS, TONE1(JIF),JIF)
C                                       Fa is the phase of the first
C                                       point at the spectrum. So we
C                                       have to recalculate the first
C                                       point's phase to the place of
C                                       the first pulse cal
               FIACT1 = FA + TWOPI*DELIF(JIF)*
     *            (F1(JIF) - (1 - RPIX) * IFCHBW(JIF))
               FICOR1 = FIACT1 - FIIN1(IND2) + FIIN1(IND1)
               IDATA(2*ITONE-1) = COS(FICOR1)
               IDATA(2*ITONE) = SIN(FICOR1)
C                                       the second tone
               ITONE = ITONE + 1
               FTONE(ITONE) = PCFRQ(JS, TONE2(JIF),JIF)
               FIACT2 = FIACT1 + TWOPI*DELIF(JIF)*DFREQ(JIF)
               FICOR2 = FIACT2 - FIIN2(IND2) + FIIN2(IND1)
               IDATA(2*ITONE-1) = COS(FICOR2)
               IDATA(2*ITONE) = SIN(FICOR2)
  640          CONTINUE
            KTONES = ITONE
C                                       find 'multiband' delay TAUA,
C                                       using phases of data at pc tones
C                                       position
            CALL AFRF (IDATA, KTONES, FTONE, DFTONM, AMPP, TAUA, FA,
     *         DDAMP, DDEL, DFA, IRET)
C
            IND1 = JS + (IANT-1)*NPOLZN
            ADELY(IND1) = -TAUA
C
            DO 645 KIF = 1,NNIF
               JIF = KIF + IBIF - 1
               IND1 = JS + (JIF-1)*NPOLZN + (RANT-1)*NPOLZN*NNNIF
               IND2 = JS + (JIF-1)*NPOLZN + (IANT-1)*NPOLZN*NNNIF
C                                       unambiguos delay for difference
C                                       given - reference antenna
               DELAR(IND2) = DELIF(JIF) - TAUA
C                                       unambiguos delay for the given
C                                       antenna, adding delay of
C                                       reference antenna
               DELAR(IND2) = DELAR(IND2) + DELAR(IND1)
  645          CONTINUE
  650       CONTINUE
  660    CONTINUE
C                                       Close up
      CALL ZCLOSE (LUNI, INDI, IRET)
C                                       Close PC table
      CALL TABIO ('CLOS', 0, IPCRNO, PCBUFF, PCBUFF, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCCAL: ERROR ',I3,' OPENING PC TABLE')
 1005 FORMAT ('Number of tones <2 because 2*NIF (=',I2,
     *   ')*NPOL(=',I2,') exceeds ')
 1006 FORMAT ('number of available PC detectors (equaled 16)')
 1007 FORMAT ('Stop because number of tones <2')
 1010 FORMAT ('Frequency channels number ', I5, 'exceeds maximum', I5)
 1020 FORMAT ('PCCAL: ERROR ',I3,' READING FQ TABLE')
 1040 FORMAT ('PCCAL: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1050 FORMAT ('The reference antenna #',I3,
     *   ' is not in the antenna list')
 1080 FORMAT ('Source ',A16, ' does not exist in the PC table')
 1085 FORMAT ('for the given timerang')
 1090 FORMAT ('All rows of the PC table are bad for the selected'
     *   , ' parameters')
 1095 FORMAT ('Antenna ', I2, ' is not corrected: no PCAL  at the',
     *   ' selected scan')
 1100 FORMAT ('There is no PCAL for ANT=',I2,',  POL=',A3,',  IF=',I2)
 1110 FORMAT ('The product NANT*NIF*NPOLZN*NF = ',I7,
     *   ' exceeds',I6)
 1120 FORMAT ('PCCAL: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1140 FORMAT ('PCCAL: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('Antenna',I2,' is not corrected: baseline',
     *   I2,'-',I2,' absent at the scan')
      END
      SUBROUTINE PCTAG (IRET)
C-----------------------------------------------------------------------
C   PCTAG uses single and multi band delays obtained from a calibrator
C   and the pccal information from a row of PC table and transforms
C   the row to SN table row.
C   Input in common:
C      NNIF    I    Number of IFs
C      NF      I    Number of frequencies in each IF
C      NPOLZN  I    Number of polarization in antennas
C      DELAR   R(*) Single band delay for each stokes, IF, antenna
C                   which align phase for all IFs, using the calibrator
C      ADELY   R(*) additional delay for each stokes and antenna
C                   constant for all IF. This delay
C                   flatten the align phase for all IF.
C      DCABL   D(*) Mean delay in the cable for each
C                   antenna while observing the calibrator
C      NTIME   I    Number of reference antenna appearences in PC table.
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER MAXN, MXTOT, MAXND
      PARAMETER (MAXN = 200)
      PARAMETER (MAXND = MAXN*5)
      PARAMETER (MXTOT = 100000)
      INCLUDE 'PCCOR.INC'
      INTEGER   I, L, JIF, JS, IANT, MODENO, IRET, REFA(2,MAXIF), IND1,
     *   IROW, LANT, IND0, IROUND
      DOUBLE PRECISION CTIM
      REAL REAL(2,MAXIF), IMAG(2,MAXIF), DELAY(2,MAXIF), FI, DISP(2),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), MBDELY(2), TIMINT, IFRM,
     *   ADDEL, DELL, DFI, FIREF, DELL1, CABCOR, ATANG2, DDISP(2)
      LOGICAL REIM
C
      INCLUDE 'INCS:PPCV.INC'
C
      INTEGER   IPCRNO, NOPOLZ, NTONES
      INTEGER   NOSTA, IARRAY, IFQID, ISRC
      REAL      TINT, PCREL(2, MAXTON, MAXIF),
     *   PCIMG(2,  MAXTON,MAXIF), PCRAT(2, MAXTON, MAXIF),
     *   STATE(2, 4, MAXIF)
      DOUBLE PRECISION TIME, CABLCL, PCFRQ(2, MAXTON, MAXIF), PCABLC
      INTEGER   PCKOLS(MAXPCC), PCNUMV(MAXPCC)
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DGLB.INC'
C-----------------------------------------------------------------------
      PCLUN = 30
      PCABLC = 0.0D0
      CALL PCINI ('READ', PCBUFF, DISKIN, OLDCNO, PCVER, CATBLK,
     *   PCLUN, IPCRNO, PCKOLS, PCNUMV, NOPOLZ, NOBAND, NTONES,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) PCVER
         GO TO 990
         END IF
      NUMPC = PCBUFF(5)
C                                       Initials for a new SN table
Cnew
C      DO 4 L = 1, 2
C         MBDELY(L) = 0.0
C         DO 2 I = 1, NNIF
C            IMAG(L, I) = 0.0
C            REAL(L,I) = 1.0
C            DELAY(L, I) = 0.0
C            RATE(L, I) = 0.0
C            WEIGHT(L, I) = 1.0
C            REFA(L, I) = REFANT
C    2       CONTINUE
C    4    CONTINUE
C                                       Read the data from PC table
      DO 100 IROW = 1, NUMPC
         CALL TABPC ('READ', PCBUFF, IPCRNO, PCKOLS, PCNUMV,
     *      NOPOLZ, TIME, TINT, ISRC, NOSTA, IARRAY, IFQID,
     *      CABLCL, STATE, PCFRQ, PCREL, PCIMG, PCRAT, IRET)
         IF (IRET.NE.0) THEN
            CALL TABERR ('READ', 'TABPC ', 'PCCAL ', IRET)
            GO TO 999
            END IF
         DO 20 IANT = 1, NANT
            IF (ANTNO(IANT).EQ.NOSTA) GO TO 25
   20       CONTINUE
         GO TO 100
   25    CONTINUE
         IF (CABLCL.EQ.DBLANK) THEN
            CABLCL = PCABLC
         ELSE
            PCABLC = CABLCL
            END IF
         LANT = IANT
C                                       Initials for a new SN table's
C                                       row
         DO 40 L = 1, 2
            MBDELY(L) = 0.0
            DISP(L) = 0.0
            DDISP(L) = 0.0
            DO 30 I = 1, NNNIF
               IMAG(L, I) = 0.0
               REAL(L,I) = 1.0
               DELAY(L, I) = 0.0
               RATE(L, I) = 0.0
               WEIGHT(L, I) = 1.0
               REFA(L, I) = REFANT
   30          CONTINUE
   40       CONTINUE
C                                       Length and central of time
C                                       interval for SN table
         CTIM = TIME
         TIMINT = TINT
C                                       do not correct SN table row
C                                       if there is no
C                                       data of PC table or calibrator
C                                       was not observed by the antenna
         IF ((NANPC(LANT).EQ.0) .OR. DATNO(LANT)) GO TO 90
C                                       correct the cable length
C                                       change
C                                       Switch off the cable correction?
         CABCOR = DCABL(LANT) - CABLCL
         IF (CABOFF.GT.0) CABCOR = 0.0

C                                       Select subarray, FREQID
         IF ((SUBA.NE.IARRAY) .AND. (SUBA.NE.0)) GO TO 100
         IF (FREQID.NE.IFQID) GO TO 100
         IFRM = 0.0
         MODENO = 0
         DO 80 JS = 1,NOPOLZ
            IND1 = JS + (LANT-1)*NPOLZN
            ADDEL = ADELY(IND1) + CABCOR
            IF (MDELC.EQ.0) MBDELY(JS) = ADDEL
            DO 75 JIF = 1, NNNIF
C                                       skip if IFs not selected
               IF ((JIF.LT.IBIF) .OR. (JIF.GT.IEIF)) GO TO 75
               IND0 = JS + (JIF-1)*NPOLZN +(LANT-1)*NPOLZN*NNNIF
C                                       check the tones are not blank
               REIM = (PCIMG(JS, TONE1(JIF),JIF).EQ.0.0 .AND.
     *            PCREL(JS, TONE1(JIF),JIF).EQ.0) .OR.
     *            (PCIMG(JS, TONE2(JIF),JIF).EQ.0.0 .AND.
     *            PCREL(JS, TONE2(JIF),JIF).EQ.0) .OR.
     *            PCIMG(JS, TONE1(JIF),JIF).EQ.FBLANK .OR.
     *            PCREL(JS, TONE1(JIF),JIF).EQ.FBLANK .OR.
     *            PCIMG(JS, TONE2(JIF),JIF).EQ.FBLANK .OR.
     *            PCREL(JS, TONE2(JIF),JIF).EQ.FBLANK
               FI = IFSID(JIF) * ATANG2(PCIMG(JS, TONE1(JIF),JIF),
     *            PCREL(JS, TONE1(JIF),JIF))
               DFI = IFSID(JIF) * ATANG2(PCIMG(JS, TONE2(JIF),JIF),
     *            PCREL(JS, TONE2(JIF),JIF)) - FI
C                                       blank if tones are zero
               IF (REIM) THEN
                  REAL(JS,JIF) = FBLANK
                  IMAG(JS,JIF) = FBLANK
                  DELAY(JS,JIF) = 0.0
                  GO TO 75
                  END IF
C
               IF (DFI.GT.PI) DFI = DFI - TWOPI
               IF (DFI.LE.-PI) DFI = DFI + TWOPI
               DELL = -DFI / DFREQ(JIF) / TWOPI
C                                       resolve ambiguaty in DELL
C                                       using DELAR found early in
C                                       PCCAL
               NTWOPI = IROUND((-DELAR(IND0) - DELL) * DFREQ(JIF))

               DELL = DELL + NTWOPI / DFREQ(JIF)
               IF(JIF.EQ.1) DELL1 = DELL
C               FIREF = - FI - TWOPI*
C     *            ((DELL - DELL1) * F1(JIF) -
C     *            ADDEL * (IFFREQ(JIF) - IFFREQ(1)))
C                                       IFFREQ(1) is  the reference
C                                       frequecy. So I recalculate to
C                                       the reference frequency
C                                       (that is good) not to the most
C                                       left one
               FIREF = - FI - TWOPI * ( (DELL + ADDEL) * F1(JIF) -
     *            ADDEL * (PCFRQ(1, TONE1(JIF), JIF)
C     *            -PCFRQ(1, TONE1(1), 1)))
     *            -PCFRQ(1, TONE1(IBIF), IBIF)))

C               FIREF = - FI - TWOPI * ( (DELL + ADDEL) * F1(JIF) -
C     *            ADDEL * (IFFREQ(JIF) - IFFREQ(1)))
               REAL(JS,JIF) = COS (FIREF)
               IMAG(JS,JIF) = SIN (FIREF)
               DELAY(JS,JIF) = DELL  + ADDEL
   75          CONTINUE
   80       CONTINUE
   90    CONTINUE
C                                       write SN table
         CALL TABSN ('WRIT', SNBUFF, ISNRNO, SNKOLS, SNNUMV, NPOLZN,
     *      CTIM, TIMINT, ISRC, NOSTA, IARRAY, FREQID, IFRM, MODENO,
     *      MBDELY, DISP, DDISP, REAL, IMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
  100    CONTINUE
C                                       Close SN table
      CALL TABIO ('CLOS', 0, ISNRNO, SNBUFF, SNBUFF, IRET)
C                                       Close PC table
      CALL TABIO ('CLOS', 0, IPCRNO, PCBUFF, PCBUFF, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT ('PCTAG: ERROR in initialization of PC table ',I3)
      END

      SUBROUTINE ANTDAT (VER, DISKI, CNOIN, IERR)
C-----------------------------------------------------------------------
C   Selects station information
C   Inputs:
C      VER           I    Antenna array number (AN file ver.)
C      DISKI         I    Vol number
C      CNOIN         I    CNO
C   Outputs in common:
C      XA,YA,ZA(*)   D    Cartesian coordinates for stations in wavelengths.
C      STANAM(*)     C*8  Names of stations
C      NANT          I    Number of antennas
C      ANTNO(*)      I    Array of antennas' numbers
C-----------------------------------------------------------------------
      INTEGER   IERR, IA, LUNA, CNOIN, IABUF(512), VER,  DISKI
      REAL UT1XXX, IATUTC
      CHARACTER STNAME*8
      DOUBLE PRECISION  DX, DY, DZ, LAMBDA
      LOGICAL DOVLBI, DOATCA
      INCLUDE 'PCCOR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                     open antenna file
      LUNA = 28
      CALL ANTINI ('READ', IABUF, DISKI, CNOIN, VER, CATBLK, LUNA,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GST0, DEGPDY, AFREQ, RDATE,
     *   POLRXY, UT1XXX, IATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      DX = ARRAYC(1)
      DY = ARRAYC(2)
      DZ = ARRAYC(3)
      NANT = IABUF(5)
      DOATCA = (ANAME.EQ.'ATCA') .OR. (ANAME.EQ.'ATLBA')
C                                       note this includes EVLA
      DOVLBI = (ABS(DX).LE.1000.0D0) .AND. (ABS (DY).LE.1000.0D0)
     *    .AND. (ABS (DZ).LE.1000.0D0) .AND. (.NOT.DOATCA)
      LAMBDA = 299792.D3/AFREQ
C                                     Get antenna info.
      DO 30 IA = 1,NANT
         IANRNO = IA
         CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, STNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IA, IERR
            CALL MSGWRT (8)
            END IF
         XA(NOSTA) = (STAXYZ(1) + DX)/LAMBDA
         IF (DOVLBI) THEN
            YA(NOSTA) = (STAXYZ(2) + DY)/LAMBDA
         ELSE
            YA(NOSTA) = -(STAXYZ(2) + DY)/LAMBDA
            END IF
         ZA(NOSTA) = (STAXYZ(3) + DZ)/LAMBDA
         STANAM(NOSTA) = STNAME
         ANTNO(IA) = NOSTA
 30      CONTINUE
C                                     close antenna file
      CALL TABIO ('CLOS', 1, IANRNO, IABUF, IABUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANTDAT: ERROR IN OPEN AN-FILE IERR = ',I6)
 1010 FORMAT ('ANTDAT: ERROR IN FINDING STATION',I3,' IERR=',I3)
 1030 FORMAT ('ANTDAT: ERROR IN CLOSING AN-FILE IERR = ',I6)
      END
      SUBROUTINE PCCHIS
C-----------------------------------------------------------------------
C   PCCHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12,  HILINE*72, LABEL*8
      INTEGER   LUN, IERR, TIM(3), DATE(3), I
      LOGICAL   T
      INCLUDE 'PCCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN, DISKIN, FCNO(NCFILE), SNBUFF, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIM)
      CALL TIMDAT (TIM, DATE, CTIME(2)(1:8), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, SNBUFF, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1020) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, SNBUFF, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUN, T, SNBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCCHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6, 'RELEASE =''',A7,' ''  /********* Start ',
     *   A12, 2X, A8)
 1020 FORMAT (A6)
 1200 FORMAT ('PCCHIS: ERROR COPYING TABLES')
      END
      SUBROUTINE AFRF (DATA, NNN, XARR, DXARM, AMPP, RATE, FI, DDAMP,
     *   DDRATE, DDFI, IERR)
C-----------------------------------------------------------------------
C   Estimates the values of frequency, initial phase and amplitude of
C   a complex exponent given (with noise) in the data set.
C   The DATA are determined on non aquidistant array XARR
C   (XARR(I) > XARR(I-1)) and can be approximated BY:
C   DATA(I) = A * EXP (J*TWOPI*RATE*(XARR(I)-XARR(1)) + FI)
C   where   RATE is unknown rate of phase change;
C           FI   is unknown phase at the first data point;
C           A    is unknown amplitude.
C   A two step process is used. The first is an Fourie analisys with step
C   1/XARR(N) at the range +- 1/DXARM;
C   The final solution of the amplitude, frequency and phase is found by
C   least square method.
C-----------------------------------------------------------------------
C   Programmer  L.R. Kogan
C-----------------------------------------------------------------------
C   Inputs:
C      DATA     R(2*NNN) input array of data
C      NNN        I      Number of complex points in the DATA
C      XARR     R(NNN)   array of argument of input data
C      DXARM    R        Element of space of input data
C   Outputs
C      AMPP     found amplitude
C      RATE     found frequency in parts of window. -0.5<RATE<0.5
C               Dimensional RATE can be obtained having devided
C               RATE by period of samples in data.
C      FI       found phase of the most left point of the data, in radian
C      DDAMP    rms of amplitude
C      DDRATE   rms of frequency
C      DDFI     rms of phase
C      IERR     error
C-----------------------------------------------------------------------
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   NNN, I, LESOL, IK, K, ITER, IERR, L, LL, LF
      REAL DATA(*), RATE, FI, AMPP, TAMP, R(3), MATR(9), NOBS, SUM, SSQ,
     *   SOL(3),VX(3), SSQRES, VARRES, VARY, FIT,DRE, DIM, ARG,
     *   SINK, COSK, DDAMP, DDRATE,DDFI, XARR(*), RE, IM, DXARR,
     *   DXARM, FSTEP, FE, ATANG2
      IERR = 0
      IF (NNN.GT.1024)  THEN
         IERR = 1
         WRITE (MSGTXT,1000) NNN
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Fourie analisys
      FSTEP = 0.5/XARR(NNN)
      L = (1.0 / DXARM) / FSTEP
      LL = 2*L + 1
C                                       find max of amplitude
      AMPP = 0.0
      DO 50 LF = 1, LL
         FE = (LF - L -1) * FSTEP
         RE = 0.0
         IM = 0.0
         DO 40 I = 1, NNN
            ARG = TWOPI*FE*(XARR(I) - XARR(1))
            RE = RE + DATA(2*I-1)*COS(ARG) + DATA(2*I)*SIN(ARG)
            IM = IM - DATA(2*I-1)*SIN(ARG) + DATA(2*I)*COS(ARG)
   40       CONTINUE
         TAMP = RE * RE + IM * IM
C                                       find max of amplitude
         IF (TAMP.GE.AMPP) THEN
            AMPP = TAMP
            RATE = FE
            FI = ATANG2 (IM, RE)
            END IF
   50    CONTINUE
      AMPP = SQRT(AMPP) / NNN
C --------------------------------------------------------------------
C                   L E A S T    S Q U A R E
C
C        precise AMP, RATE and FI using non linear least square
C              fitting a  complex exponent to the given data.
C                We have NNN2 = 2*NNN points of data
C             (real and image for each point of data)
C         We have 3 unknown magnitudes: ampl., rate and phase
C --------------------------------------------------------------------
      ITER = 1
  100 CONTINUE
C                                       Force result vector R(3),
C                                       matrix M(3*3) to zero
      DO 120 I = 1, 3
         R(I) = 0.0
         DO 110 K = 1, 3
            IK = K + (I - 1)*3
            MATR (IK) = 0.0
 110        CONTINUE
 120     CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(3)
C                                       and matrix MATR(3*3)
C                                       for routine LEASQR

      DO 140 I = 1, NNN
         DXARR = TWOPI * (XARR(I) - XARR(1))
C                                       take only the points.NE.0
         IF ((DATA(2*I-1).EQ.0.0) .AND.
     *      (DATA(2*I).EQ.0.0)) GOTO 140
         NOBS = NOBS + 1
         DRE = DATA (2*I -1)
         DIM = DATA (2*I)
C                                       difference of DATA and model
         ARG = DXARR * RATE + FI
         SINK = SIN(ARG)
         COSK = COS(ARG)
         DRE = DRE - AMPP * COSK
         DIM = DIM - AMPP * SINK
         SUM = SUM + DRE + DIM
         SSQ = SSQ + DRE*DRE + DIM*DIM
C
C                                       1-ampl; 2-rate; 3-fi.
         R(1) =  R(1) + DRE * COSK
     *      + DIM * SINK
         R(2) =  R(2) - DRE * AMPP * SINK * DXARR
     *      + DIM * AMPP * COSK * DXARR
         R(3) =  R(3) - DRE * AMPP*SINK
     *      + DIM * AMPP*COSK
C                                       calculate upper/right
C                                       triangle of MATR
         MATR(1) = MATR(1) + COSK*COSK + SINK*SINK
C         MATR(2) = MATR(2) - COSK*AMPP*SINK*DXARR
C     *      + SINK*AMPP*COSK*DXARR
C         MATR(3) = MATR(3) - COSK*AMPP*SINK + SINK*AMPP*COSK
         MATR(4) = MATR(4) - AMPP*SINK*DXARR*COSK
     *      + AMPP*COSK*DXARR*SINK
         MATR(5) = MATR(5) + AMPP*SINK*DXARR*AMPP*SINK*DXARR
     *      + AMPP*COSK*DXARR*AMPP*COSK*DXARR
C         MATR(6) = MATR(6) + AMPP*SINK*DXARR*AMPP*SINK
C     *      + AMPP*COSK*DXARR*AMPP*COSK
         MATR(7) = MATR(7) - AMPP*SINK*COSK + AMPP*COSK*SINK
         MATR(8) = MATR(8) + AMPP*SINK*AMPP*SINK*DXARR
     *      + AMPP*COSK*AMPP*COSK*DXARR
         MATR(9) = MATR(9) + AMPP*SINK*AMPP*SINK
     *      + AMPP*COSK*AMPP*COSK
 140     CONTINUE
      NOBS = 2*NOBS
C                                       NOBS need to be real for LEASQR
      CALL LEASQR (3, NOBS, SUM, SSQ, R, MATR, SOL, VX, SSQRES,
     *   VARRES, VARY, FIT, LESOL)
C                                       find the solution
      AMPP = AMPP + SOL(1)
      RATE = RATE + SOL(2)
      FI = FI + SOL(3)
      ITER = ITER + 1
      IF (ITER.LE.2) GO TO 100
      DDAMP = SQRT (VX(1))
      DDRATE = SQRT (VX(2))
      DDFI = SQRT (VX(3))
C-----------------------------------------------------------------------
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AFRFI: The number of points ',I6,' is more MAX=1024')
      END
      REAL FUNCTION ATANG2 (Y, X)
C-----------------------------------------------------------------------
C! Calculate ATAN2(Y,X) considering ATAT2(0, 0) = 0
C# UV-util
C-----------------------------------------------------------------------
C   Inputs:
C      Y, X    R       Y, X arguments of ATAN
C-----------------------------------------------------------------------
      REAL    X, Y
C-----------------------------------------------------------------------
      IF (X.EQ.0.0  .AND.  Y.EQ.0.0) THEN
         ATANG2 = 0.0
      ELSE
         ATANG2 = ATAN2(Y, X)
         END IF
C
 999  RETURN
      END






