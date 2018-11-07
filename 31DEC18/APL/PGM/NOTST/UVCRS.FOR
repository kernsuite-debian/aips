LOCAL INCLUDE 'UVCRS.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:DMSG.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XOFILE(12), XXSTOK
      REAL      XSIN, XDISIN, APARM(10), BUFF1(UVBFSS), XANT(50),
     *   BUFF2(UVBFSS), UVRADI, UVRADX, UVRADY, XIF, XF, IATUTC,
     *   UT1XXX, HRAD, GRRAD, DELTA, ALFA, PI, DAYRAD, XSNVER,
     *   GNMOD, TIMINT
      INTEGER SNKOLS(MAXSNC), SNNUMV(MAXSNC), ISNRNO, NUMNOD,
     *   SNV, SNLUN, SNBUFF(1024), NPOLZN, NUMBIF, NUMTEL
      INTEGER   SEQIN, DISKIN, NUMHIS, JBUFSZ, ILOCWT, CATOLD(256),
     *   INCSI, INCFI, INCIFI, LRECI, NRPRMI, NANT, ANTNO(MAXANT),
     *   NNVIS, NUMCR, ISTOK, NIF, NF, NCORI, ANTS(50), NANTS, NNIF,
     *   NNF, SNOLD
      DOUBLE PRECISION AFREQ, GST0, XA(MAXANT),YA(MAXANT),ZA(MAXANT),
     *   CTIM, ARRLON
      LOGICAL   ISCOMP, RECT, ELIP, ISAPPL
      CHARACTER NAMEIN*12, CLAIN*6, HISCRD(50)*64, STANAM(MAXANT)*8,
     *   OFILE*48, STOKES*4
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN,  XIF, XF,
     *   XXSTOK, XANT, XSNVER, XOFILE, APARM, SEQIN, DISKIN, NUMHIS,
     *   ILOCWT, CATOLD, INCSI, INCFI, INCIFI, LRECI, NRPRMI, ISCOMP,
     *   NIF, NF, NNF, NNIF
      COMMON /CHARPM/ NAMEIN, CLAIN, OFILE,  HISCRD, STANAM, STOKES
      COMMON /GRHRAD/ HRAD, GRRAD, DELTA, ALFA, PI, DAYRAD, UVRADI,
     *   UVRADX, UVRADY, RECT, ELIP, NNVIS, NUMCR, ISTOK, NCORI,
     *   NUMTEL, NPOLZN, NUMBIF
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ, ANTS, NANTS
      COMMON /CANIN/ XA, YA, ZA, ARRLON, AFREQ, GST0, CTIM, IATUTC,
     *   UT1XXX, NANT, ANTNO
      COMMON /SNPARM/ SNBUFF, SNKOLS, SNNUMV, ISNRNO,
     *   NUMNOD, SNV, SNLUN, GNMOD, ISAPPL, TIMINT, SNOLD
LOCAL END
      PROGRAM UVCRS
C-----------------------------------------------------------------------
C! Find intersections of UV ellipses
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000, 2003, 2009, 2012, 2015
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
C-----------------------------------------------------------------------
C   UVCRS finds intersections of UV ellipses and detemines corresponding
C   ratio of complex correlation coefficients
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      STOKES         STOKES        STOKE's type
C      BIF            NIF           The IF's number
C      BCHAN          NF            The F's number
C      ANTENNAS       ANTS          The well calibrated antenna,
C                                   the gain is forced to 1.
C      SNVER          SNV           The number of SN table
C      OUTFILE        OFILE         Filename in which to write
C                                   the table of crossings
C      APARM(10)      APARM         Aparm(1) = UVRADX
C                                   Aparm(2) = UVRADY
C                                   Aparm(3)=0 => rectangular
C                                           >0 => eliptical
C
C   Programmer L. R. Kogan, December 1993.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'UVCRS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DSOU.INC'
      DATA PRGM /'UVCRS '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create inputs for RATIO
      CALL UVCRIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Find crossing, ratios and
C                                       print the table
      CALL RATIO (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVCHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVCRIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVCRIN gets input parameters for RATIO
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
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
      CHARACTER STAT*4, UTYPE*2, STOK(4)*4
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX, VER, I, L, KEYLOC,
     *   KEYTYP, ORIGIN, NUMKEY
      REAL      DECNOD(25), RANOD(25)
      LOGICAL   T, F, CHSTAT, MULTI
      INCLUDE 'UVCRS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA STOK /'RR  ', 'LL  ', '    ', '    '/
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
      NPARM = 83
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
      SNV = IROUND(XSNVER)
      SNOLD = SNV
      UVRADX = APARM(1)*1.0E6
      IF (UVRADX.EQ.0.0) THEN
         UVRADX = 0.5E6
         UVRADY = 0.5E6
      ELSE
         IF (APARM(2).EQ.0.0) THEN
            UVRADY = UVRADX
         ELSE
            UVRADY = APARM(2) * 1.0E6
         END IF
      END IF
      UVRADI = SQRT (UVRADX*UVRADX + UVRADY*UVRADY)
      IF (APARM(3).EQ.0) THEN
         RECT = .TRUE.
         ELIP = .FALSE.
      ELSE
         RECT = .FALSE.
         ELIP = .TRUE.
         END IF
      NANTS = 0
      DO 10 I = 1,50
         ANTS(I) = IROUND(XANT(I))
         IF ( (I.GT.1) .AND. ( (ANTS(I).EQ.ANTS(I-1)) .OR.
     *      (ANTS(I-1).EQ.0) ) ) ANTS(I) = 0
         IF (ANTS(I).NE.0) NANTS = NANTS + 1
   10    CONTINUE
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (48, 1, XOFILE, OFILE)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      NIF = IROUND (XIF)
      IF (NIF.EQ.0) NIF = 1
      NF = IROUND (XF)
      IF (NF.EQ.0) NF = 1
      OLDCNO = 1
C                                       Get CATBLK from old file.
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   UTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
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
C                                       does the given Stokes exist
C                                       at the data? If yes what is
C                                       its number?
      ISTOK = 0
      DO 20 I = 1,NCOR
         L = I - ICOR0 - 1
         IF (STOK(L).EQ.STOKES) THEN
            ISTOK = I
            GO TO 30
            END IF
   20    CONTINUE
   30    CONTINUE
      IF (ISTOK.EQ.0) THEN
         JERR = 1
         WRITE (MSGTXT, 1060) STOKES
         GO TO 990
         END IF
      NNIF = 1
      NNF = 1
      IF (JLOCIF.GE.0) NNIF = CATBLK(KINAX+JLOCIF)
      IF (JLOCF.GE.0) NNF = CATBLK(KINAX+JLOCF)
      IF ((NIF.GT.NNIF) .OR. (NIF.LE.0)) THEN
         JERR = 1
         WRITE (MSGTXT, 1080) NIF, NNIF
         GO TO 990
         END IF
      IF ((NF.GT.NNF) .OR. (NF.LE.0)) THEN
         JERR = 1
         WRITE (MSGTXT, 1100) NF, NNF
         GO TO 990
         END IF
C                                       Get a source coordinates
      CALL GETSOU (1, DISKIN, OLDCNO, CATBLK, 40, JERR)
      ALFA = RAAPP
      DELTA = DECAPP
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
      JERR = 0
C                                       Prepare antennas data
      VER = 1
      CALL ANTDAT (VER, DISKIN, OLDCNO, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1120)
         GO TO 990
         END IF
C                                       Open SN table
      IF (SNV.GE.0) THEN
C                                       Change status to 'writ'
C                                       Determine status of file
         UTYPE = 'UV'
         CHSTAT = .FALSE.
         CALL CATDIR ('INFO', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *      SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
C                                       Change status
         IF (STAT.EQ.  'READ') THEN
            STAT = 'CLRD'
            CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *         SEQIN, UTYPE, NLUSER, STAT, BUFF1 , IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1140) IERR, STAT
               GO TO 990
               END IF
            STAT = 'WRIT'
            CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *         SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
            IF (IERR.NE.0) THEN
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
   60       CONTINUE
         SNLUN = 48
         NUMBIF = NNIF
         NUMTEL = NANT
         NPOLZN = 2
         IF (NCOR.EQ.1) NPOLZN = 1
         NUMNOD = 0
         CALL SNINI ('WRIT', SNBUFF, DISKIN, OLDCNO, SNV, CATBLK, SNLUN,
     *      ISNRNO, SNKOLS, SNNUMV, NUMTEL, NPOLZN, NUMBIF, NUMNOD,
     *      GNMOD, RANOD, DECNOD, ISAPPL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) SNV
            GO TO 990
            END IF
         ISNRNO = 1
C                                       Add the ORIGIN keyword
         CALL MULSDB (CATBLK, MULTI)
         KEYLOC = 1
         KEYTYP = 4
         ORIGIN = 0
         NUMKEY = 1
         IF (.NOT.MULTI) ORIGIN = 1
         CALL TABKEY ('WRIT', 'SNORIGIN', NUMKEY, SNBUFF, KEYLOC,
     *      ORIGIN, KEYTYP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1015) IERR
            GO TO 990
            END IF
C                                       Check if changed status
         IF (CHSTAT) THEN
            STAT = 'CLWR'
            CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *         SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1140) IERR, STAT
               GO TO 990
               END IF
            STAT = 'READ'
            CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *         SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1140) IERR, STAT
               GO TO 990
               END IF
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVCRIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('UVCRIN: Writing to SN table ',I3)
 1015 FORMAT ('UVCRIN: ERROR',I3,' READING KEYWORDS FROM SN TABLE')
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('UVCRIN: ERROR. STOKES ',A4,' is not compatible')
 1080 FORMAT ('UVCRIN: IFs NUMBER ',I4, '> MAXIMUM IFs NUMBER -',I2)
 1100 FORMAT ('UVCRIN: Fs NUMBER ',I4, '> MAXIMUM Fs NUMBER -',I2)
 1120 FORMAT ('UVCRIN: PROBREM OF READING DATA FROM ANTENNA TABLE')
 1140 FORMAT ('UVCRIN: ERROR ',I3,' CHANGING ',A4,' STATUS')
      END
      SUBROUTINE RATIO (IRET)
C-----------------------------------------------------------------------
C   RATIO finds the all crossings inside UVRADI and calculates
C   the average ratio of visibilities' amplitudes
C   Input in common:
C      UVRADX     R  Semi axis of the crossings region
C      UVRADY     R
C      RECT, ELIP L  A form of the region
C      LRECI      I  Input file record length
C      NRPRMI     I  Input number of random parameters.
C      INCSI      I  Input Stokes' increment in vis.
C      INCFI      I  Input frequency increment in vis.
C      INCIFI     I  Input IF increment in vis.
C      ISCOMP     L  If true data is compressed
C   Output:
C      IRET       I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INCLUDE 'UVCRS.INC'
      INTEGER   IRET, NCR, NCROSS(5000), LUNPR, PFIND, NCH, ITRIM
      CHARACTER IFILE*48, LINE*132, SIMB(5000)*1, ST1*8, ST2*8,
     *          ST3*8, ST4*8, QUEST*1, STREF(50)*8
      INTEGER   LUNI, INDI, INCX, BASN(1000), SBASN(1000), ANN1(1000),
     *   ANN2(1000), A1, A2, B1, B2, I, K, L, TEMP1, TEMP2, III, NB,
     *   ANTN, K0(50), NREG(5000), A(MAXANT), AN(MAXANT), ANREF, INDOBS,
     *   K00, KOBS, ANREF0, REFA(2,MAXIF), LESOL
      LOGICAL   T, F, WPRINT
      REAL      UVX, UVY, RAT, TT1(10), TT2(10), TALL(50000),
     *          TB(1000), TE(1000), TDI(20000), TDK(20000), NOBS,
     *          R(MAXANT), M(MAXANT*MAXANT), GAIN(MAXANT),RATL,
     *          RATLOG(5000), RATCOR, GLOG(MAXANT), REAL(2,MAXIF),
     *          IMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *          WEIGHT(2,MAXIF), MBDELY(2), DISP(2), DDISP(2),
     *          VX(MAXANT), SSQRES, VARRES, VARY, FIT, G(MAXANT),
     *          UALL(50000), VALL(50000), AMALL(50000), WEALL(50000)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      PI = 3.1415926
      GRRAD = PI/180.0
      HRAD = PI/12.0
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
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Store  and sort the data
      CALL BASVIS (LUNI, INDI, ANN1, ANN2, BASN, SBASN,
     *      NB, TB, TE, TALL, UALL, VALL, AMALL, WEALL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       find number of possible crossing
C                                       baselines - NCR, and array of
C                                       its identificators - NCROSS
      CALL CROSAT (TB, TE, NCR, NCROSS, TDI, TDK, SIMB)
      IF (NCR.EQ.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1040)
         GO TO 990
         END IF
C                                       ANTS -  antennas are not
C                                       corrected;  gain=1.
C                                       Exclude ANTS from the list
      ANTN = 0
      ANREF = 0
      DO 20 K = 1, NANT
         ANREF0 = ANREF
         DO 10 I = 1, NANTS
            IF (ANTNO(K).EQ.ANTS(I)) THEN
               ANREF = ANREF + 1
               K0(ANREF) = K
               STREF(ANREF) = STANAM(K)
               END IF
   10       CONTINUE
         IF (ANREF.EQ.ANREF0) THEN
            ANTN = ANTN + 1
            AN(ANTN) = ANTNO(K)
            END IF
   20    CONTINUE
C                                       Force a result vector R and
C                                       matrix M to zero
      DO 40 K = 1, ANTN
         R(K) = 0.0
         DO 30 I = 1, ANTN
            M(I + (K-1)*ANTN) = 0.0
   30       CONTINUE
   40    CONTINUE
      UVX = UVRADX / 1.0E6
      UVY = UVRADY / 1.0E6
C
      WPRINT = .FALSE.
      IF (OFILE(1:1).NE.' ') WPRINT = .TRUE.
C                                       open the outfile and print
      IF (WPRINT) THEN
C                                       print the title of the table
         LUNPR = 10
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, F, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET
            GO TO 990
            END IF
         WRITE (LINE,1080) NAMEIN, CLAIN, SEQIN, DISKIN
         NCH = ITRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         WRITE (LINE,1100) NNVIS
         NCH = ITRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         IF (RECT) WRITE (LINE,1120) UVX, UVY
         IF (ELIP) WRITE (LINE,1140) UVX, UVY
         NCH = ITRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         WRITE (LINE,1160) NCR
         NCH = ITRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       print the blank line
         LINE = ' '
         NCH = 1
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         END IF
C                                       print the title of the table
      IF (APARM(4).GT.0) THEN
         WRITE (MSGTXT,1080) NAMEIN, CLAIN, SEQIN, DISKIN
         CALL MSGWRT (4)
         WRITE (MSGTXT,1100) NNVIS
         CALL MSGWRT (4)
         IF (RECT) THEN
            WRITE (MSGTXT,1120) UVX, UVY
            CALL MSGWRT (4)
            END IF
         IF (ELIP) THEN
            WRITE (MSGTXT,1140) UVX, UVY
            CALL MSGWRT (4)
            END IF
         WRITE (MSGTXT,1160) NCR
         CALL MSGWRT (4)
         END IF
C
      III = 0
      KOBS = 0
C                                       find the ratios for predicted
C                                       crossings
      DO 100 I = 1, NCR
C                                       restore momemts of intersections
         DO 45 K = 1,10
            TT1(K) = TDI(K + 10*(I-1))
            TT2(K) = TDK(K + 10*(I-1))
   45       CONTINUE
         A1 = NCROSS(I)/1000000
         TEMP1 = NCROSS(I) - A1*1000000
         A2 = TEMP1/10000
         TEMP2 = TEMP1 - A2*10000
         B1 = TEMP2/100
         B2 = TEMP2 - B1*100
         CALL RATCRO (A1, A2, B1, B2, NB, TT1, TT2, BASN, SBASN, ANN1,
     *      ANN2, TALL, UALL, VALL, AMALL, WEALL, RAT, RATL)
         IF (RAT.NE.0.0) THEN
            III = III + 1
            RATLOG(III) = RATL
            NCROSS(III) = NCROSS(I)
            NREG(III) = NUMCR
            IF (APARM(4).GT.0.0) THEN
               WRITE (MSGTXT,1200) III, NREG(III), A1, A2, B1, B2, RAT
               CALL MSGWRT (4)
               END IF
            DO 55 K = 1, ANTN
               A(K) = 0
   55          CONTINUE
C                                       Prepare a row of a matrix A
            INDOBS = 0
            DO 60 K = 1, ANTN
               IF (AN(K).EQ.A1) THEN
                  IF ((B1.NE.A1) .AND. (B2.NE.A1)) A(K) = 1
                  INDOBS = INDOBS + 1
                  END IF
               IF (AN(K).EQ.A2) THEN
                  IF ((B1.NE.A2) .AND. (B2.NE.A2)) A(K) = 1
                  INDOBS = INDOBS + 1
                  END IF
               IF (AN(K).EQ.B1) THEN
                  IF ((A1.NE.B1) .AND. (A2.NE.B1)) A(K) = -1
                  INDOBS = INDOBS + 1
                  END IF
               IF (AN(K).EQ.B2) THEN
                  IF ((A1.NE.B2) .AND. (A2.NE.B2)) A(K) = -1
                  INDOBS = INDOBS + 1
                  END IF
   60          CONTINUE
            IF (INDOBS.NE.0) KOBS = KOBS + 1
C                                       Prepare result vector R=A*RAT
C                                       and matrix M = AT * A
            DO 80 K = 1, ANTN
               R(K) = RATL * A(K) + R(K)
               DO 70 L = 1, ANTN
                  M(L + (K-1)*ANTN) = A(L)*A(K) + M(L + (K-1)*ANTN)
   70             CONTINUE
   80          CONTINUE
            END IF
  100    CONTINUE
      IF (III.EQ.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1220)
         GO TO 990
         END IF
      NOBS = KOBS
      CALL LEASQR (ANTN, NOBS, 1, 1, R, M, GAIN, VX, SSQRES, VARRES,
     *             VARY, FIT, LESOL)
C                                       LESOL=0 => solution founded
      IF (LESOL.EQ.0) THEN
         IF (WPRINT) THEN
            LINE = ' '
            NCH = 1
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            WRITE (LINE,1230)
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            DO 110 I = 1, ANREF
               WRITE (LINE,1235) STREF(I)
               NCH = ITRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
  110          CONTINUE
            LINE = ' '
            NCH = 1
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            END IF
C                                       Combine both found and
C                                       forced to 1 gains
         IF (SNOLD.EQ.0) THEN
            DO 120 L = 1, 2
               MBDELY(L) = 0.0
               DISP(L) = 0.0
               DDISP(L) = 0.0
               DO 115 I = 1, NNIF
                  IMAG(L, I) = 0.0
                  REAL(L,I) = 1.0
                  DELAY(L, I) = 0.0
                  RATE(L, I) = 0.0
                  WEIGHT(L, I) = 1.0
                  REFA(L, I) = ANTNO(1)
  115             CONTINUE
  120          CONTINUE
            END IF
         I = 1
         K00 = K0(1)
         IF (K00.EQ.0) K00 = 100
         DO 140 K = 1, NANT
            IF (K.LT.K00) THEN
               GLOG(K) = GAIN(K - I + 1)
               G(K) = EXP(GAIN(K - I + 1))
            ELSE
               IF (K.EQ.K00) THEN
                  GLOG(K) = 0.0
                  G(K) = 1.0
                  I = I + 1
                  IF (I.LE.ANREF) THEN
                     K00 = K0(I)
                  ELSE
                     K00 = 100
                     END IF
                  END IF
               END IF
C                                       write SN table
            IF (SNV.GE.0) THEN
               IF (SNOLD.GT.0) THEN
                  CALL TABSN ('READ', SNBUFF, ISNRNO, SNKOLS, SNNUMV,
     *               NPOLZN, CTIM, TIMINT, 0, ANTNO(K), 1, -1, 0.0, 0,
     *               MBDELY, DISP, DDISP, REAL, IMAG, DELAY, RATE,
     *               WEIGHT, REFA, IRET)
                  ISNRNO = ISNRNO - 1
                  END IF
               REAL(ISTOK, NIF) = 1.0/G(K)
               CALL TABSN ('WRIT', SNBUFF, ISNRNO, SNKOLS, SNNUMV,
     *            NPOLZN, CTIM, TIMINT, 0, ANTNO(K), 1, -1, 0.0, 0,
     *            MBDELY, DISP, DDISP, REAL, IMAG, DELAY, RATE, WEIGHT,
     *            REFA, IRET)
               NUMHIS = NUMHIS + 1
               WRITE (HISCRD(NUMHIS),1310) STANAM(K), G(K)
               END IF
C         PRINT 1240, K, STANAM(K), G(K)
            IF (WPRINT) THEN
               WRITE (LINE,1240)  K, STANAM(K), G(K)
               NCH = ITRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
               END IF
  140       CONTINUE
         END IF
C                                       end of yes solution
      IF (WPRINT) THEN
         LINE = ' '
         NCH = 1
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         LINE = ' '
         NCH = 1
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         IF (LESOL.EQ.0) THEN
            WRITE (LINE,1260)
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         ELSE
            WRITE (LINE,1270)
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            END IF
         WRITE (LINE,1280)
         NCH = ITRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         WRITE (LINE,1290)
         NCH = ITRIM(LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         LINE = ' '
         NCH = 1
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
         END IF
C                                       Print the table of ratios
C                                       before and after correction
      DO 200 I = 1, III
         RATCOR = RATLOG(I)
         A1 = NCROSS(I)/1000000
         TEMP1 = NCROSS(I) - A1*1000000
         A2 = TEMP1/10000
         TEMP2 = TEMP1 - A2*10000
         B1 = TEMP2/100
         B2 = TEMP2 - B1*100
         DO 160 K = 1,NANT
            IF (LESOL.NE.0) GLOG(K) = 1.0
            IF (ANTNO(K).EQ.A1) THEN
               ST1 = STANAM(K)
               RATCOR = RATCOR - GLOG(K)
               END IF
            IF (ANTNO(K).EQ.A2) THEN
               ST2 = STANAM(K)
               RATCOR = RATCOR - GLOG(K)
               END IF
            IF (ANTNO(K).EQ.B1) THEN
               ST3 = STANAM(K)
               RATCOR = RATCOR + GLOG(K)
               END IF
            IF (ANTNO(K).EQ.B2) THEN
               ST4 = STANAM(K)
               RATCOR = RATCOR + GLOG(K)
               END IF
  160       CONTINUE
            RATCOR = EXP(RATCOR)
C                                       mark bad points
         IF ( (RATLOG(I).GE.ALOG(2.0)) .OR.
     *        (RATLOG(I).LE.ALOG(0.5)) ) THEN
            QUEST = '?'
         ELSE
            QUEST = ' '
            END IF
         RAT = EXP(RATLOG(I))

C         PRINT 1300, QUEST, I, NREG(I), ST1, ST2,
C     *            ST3, ST4, A1, A2, B1, B2, RAT, RATCOR
         IF (WPRINT) THEN
            WRITE (LINE,1300) QUEST, I, NREG(I), ST1,
     *             ST2, ST3, ST4, A1, A2, B1, B2, RAT, RATCOR
            NCH = ITRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            END IF
  200    CONTINUE
C                                       Close SN table
      IF (SNV.GE.0) THEN
         CALL TABIO ('CLOS', 0, ISNRNO, SNBUFF, SNBUFF, IRET)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RATIO: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1020 FORMAT ('RATIO: ERROR',I3,' A PROBLEM INTO BASVIS')
 1040 FORMAT ('RATIO: NOONE CROSSING HAS BEEN PREDICTED!')
 1060 FORMAT ('RATIO: ERROR',I3,' A PROBLEM INTO ZTXOPN')
 1080 FORMAT (1X,'Filename = ', A12,3X,'.',A6,'.',I5, I5 )
 1100 FORMAT (1X,'Number of visibilities = ', I6)
 1120 FORMAT (1X,'Rectangular with half the length and width',
     *        1X, F4.1,' x ', F4.1,' Megalamda')
 1140 FORMAT (1X,'Ellipse with half axes',
     *        1X, F4.1,' x ', F4.1,' Megalamda ')
 1160 FORMAT (1X,'Number of possible crossings = ',I5)
 1200 FORMAT (1X, I4, I4, 1X, I3,'-',I3,' /',I3,'-',I3, 1X, F6.3)
 1220 FORMAT ('      RATIO: NOONE CROSSING HAS BEEN FOUND!')
 1230 FORMAT (' The least square solution for the antannas gains')
 1235 FORMAT (' The gain for antenna ' ,A8, ' is forced to 1')
 1240 FORMAT (1X, I3, 1X, A8, 1X, F10.3)
 1260 FORMAT (' The table of original ratios and resulting ratios',
     *        ' after applying the solution')
 1270 FORMAT (' The table of original ratios. The data is not',
     *        ' enough to find solution.')
 1280 FORMAT (' The sign ? marks the lines with a ratio less than 0.5',
     *        ' or greater than 2.0')
 1290 FORMAT (' The last two columns are the original and resulting',
     *        ' ratios')
 1300 FORMAT (1X, A1, I4, I4, 1X, A8,'-',A8,' / ',A8,'-',
     *        A8, I3,'-',I3,' /',I3,'-',I3, 1X, F6.3, 1X, F6.3)
 1310 FORMAT (A8, F10.3)
      END
      SUBROUTINE UVCHIS
C-----------------------------------------------------------------------
C   FUGHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12,  HILINE*72, LABEL*8
      INTEGER   LUN, IERR, TIME(3), DATE(3), I
      LOGICAL   T
      INCLUDE 'UVCRS.INC'
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
      CALL HIOPEN (LUN, DISKIN, FCNO(NCFILE), BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2)(1:8), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1020) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUN, T, BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6, 'RELEASE =''',A7,' ''  /********* Start ',
     *   A12, 2X, A8)
 1020 FORMAT (A6)
      END
      SUBROUTINE BASVIS (LUNI, INDI, AN1, AN2, BASN, SBASN,
     *      NB, TB, TE, TALL, UALL, VALL, AMALL, WEALL, KERR)
C--------------------------------------------------------------------
C    BASVIS calculates number of visibilities BASN(I)
C    corresponding to the baseline AN1-AN2, and sums of these numbers
C    SBUSN(I) for all previous baselines; accumalates (sorts) U,V and
C    amplitudes. The LOG of amplitudes are taken instead of amplitudes
C    itselfs.
C   Inputs:
C      LUNI    I  Port's number
C      INDI    I  Disk number
C   Input in common:
C      NANT    I  Number of antennas
C      ANTNO   I  Array of antennas numbers
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Outputs:
C       AN1    I  Array of the first antennas of a baseline
C       AN2    I  Array of the second antennas of abaseline
C       BASN   I  Array of number of visibilities of
C                 a baseline
C      SBASN   I  Array of last visibilities numbers on the border
C                 between neighbours base lines
C       NB     I  Number of baselines
C      TB(*)   R  Array of beginings
C      TE(*)   R  Array of endings
C      TALL(*) R  Times moment sorted by baselines
C      UALL(*) R  U sorted by baselines
C      VALL(*) R  V sorted by baselines
C      AMALL(*)R  Amplitude sorted by baselines
C      WEALL(*)R  Weight sorted by baselines
C      KERR    I  Error code:  0 => OK
C-----------------------------------------------------------------------
      INTEGER   IA1, IA2, KERR, BASN(*), SBASN(*), AN1(*), AN2(*), NB,
     *   TSBASN(1000), I, K, L, INIO, IPTRI, IBIND, ILENBU, VO, BO,
     *   LUNI, INDI, INDEXI, INDEX, ITB
      INCLUDE 'UVCRS.INC'
      REAL      RE, IM, UALL(*), VALL(*), AMALL(*), WEALL(*),
     *   TB(*), TE(*), TIM, TALL(*), CBUFF(UVBFSS), TMAX, TMIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA VO,BO /0, 1/
C---------------------------------------------------------------------
      TMAX = 0.0
      TMIN = 100.0
      KERR = 0
      NB = 0
      DO 20 I = 1, NANT-1
         DO 10 K = I+1, NANT
            NB = NB + 1
            AN1(NB) = ANTNO(I)
            AN2(NB) = ANTNO(K)
   10       CONTINUE
   20    CONTINUE
      DO 25 I = 1,NB
         BASN(I) = 0
         TB(I) = 100.0
         TE(I) = 0.0
   25    CONTINUE
C                                       Read all data to determine
C                                       the number of visibilities
C                                       corresponding to a baseline
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, KERR)
      IF (KERR.NE.0) THEN
         WRITE (MSGTXT,1000) KERR
         GO TO 990
         END IF
C                                       Loop
 30   CONTINUE
C                                       Read vis. record.
      CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, KERR)
      IF (KERR.NE.0) THEN
         WRITE (MSGTXT,1020) KERR
         GO TO 990
         END IF
      IPTRI = IBIND
C                                       Out of data?
      IF (INIO.LE.0) GO TO 60
C                                       Loop over buffer
      DO 50 I = 1,INIO
         IF (ILOCB.GE.0) THEN
            IA2 = BUFF1(IPTRI+ILOCB) + 0.1
            IA1 = IA2 / 256
            IA2 = IA2 - IA1*256
         ELSE
            IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
            IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
            END IF
         TIM = BUFF1(IPTRI+ILOCT)
         IF (TIM.GT.TMAX) TMAX = TIM
         IF (TIM.LT.TMIN) TMIN = TIM
         DO 40 K = 1, NB
            IF ((IA1.EQ.AN1(K)) .AND. (IA2.EQ.AN2(K))) THEN
               BASN(K) = BASN(K) + 1
C                                       find the min and max time
               IF (TIM.GT.TE(K)) TE(K) = TIM
               IF (TIM.LT.TB(K)) TB(K) = TIM
               END IF
   40       CONTINUE
         IPTRI = IPTRI + LRECI
   50 CONTINUE
      GO TO 30
   60 CONTINUE
      SBASN(1) = 0.0
      TSBASN(1)= 0.0
      DO 80 I = 2, NB
         SBASN(I) = SBASN(I-1) + BASN(I-1)
         TSBASN(I) = SBASN(I)
   80    CONTINUE
      NNVIS = SBASN(NB) + BASN(NB)
      IF (NNVIS.GT.50000) THEN
         KERR = -1
         WRITE (MSGTXT, 1040) KERR, NNVIS
         GO TO 990
         END IF
      CTIM = (TMAX + TMIN) / 2.0
      TIMINT = TMAX - TMIN
C                                       Again read all data to store
C                                       sorted by baseline U, V,
C                                       amplitude and weight
      L = 0
      ILENBU = 0
      INDEXI = 3*( (NIF - 1) * INCIFI + (NF - 1) * INCFI +
     *         (ISTOK - 1) * INCSI )
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, KERR)
      IF (KERR.NE.0) THEN
         WRITE (MSGTXT,1000) KERR
         GO TO 990
         END IF
C                                       Loop
 130   CONTINUE
C                                       Read vis. record.
      CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, KERR)
      IF (KERR.NE.0) THEN
         WRITE (MSGTXT,1020) KERR
         GO TO 990
         END IF
      IPTRI = IBIND
C                                       Out of data?
      IF (INIO.LE.0) GO TO 160
C                                       Loop over buffer
      DO 150 I = 1,INIO
         IF (ILOCB.GE.0) THEN
            IA2 = BUFF1(IPTRI+ILOCB) + 0.1
            IA1 = IA2 / 256
            IA2 = IA2 - IA1*256
         ELSE
            IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
            IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
            END IF
         DO 140 K = 1, NB
            IF ((IA1.EQ.AN1(K)) .AND. (IA2.EQ.AN2(K))) THEN
               TSBASN(K) = TSBASN(K) + 1
               L = TSBASN(K)
               UALL(L) = BUFF1(IPTRI + ILOCU)
               VALL(L) = BUFF1(IPTRI + ILOCV)
               TALL(L) = BUFF1(IPTRI + ILOCT)
C                                       get the visibilities
               IF (ISCOMP) THEN
C                                       Compressed data.
                  CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *               BUFF1(IPTRI+ILOCWT), CBUFF)
                  RE = CBUFF (INDEXI + 1)
                  IM = CBUFF (INDEXI + 2)
C                                       Take logarithm of amplitudes
                  AMALL(L) = 0.5 * ALOG(RE*RE + IM*IM)
                  WEALL(L) = CBUFF(INDEXI + 3)
               ELSE
C                                       Un compressed data
                  INDEX = INDEXI + IPTRI + NRPRMI
                  RE = BUFF1(INDEX)
                  IM = BUFF1(INDEX + 1)
C                                       Take logarithm of amplitudes
                  AMALL(L) = 0.5 * ALOG(RE*RE + IM*IM)
                  WEALL(L) = BUFF1(INDEX + 2)
                  END IF
               END IF
  140       CONTINUE
         IPTRI = IPTRI + LRECI
  150 CONTINUE
      GO TO 130
  160 CONTINUE
C                                       Does time interval include
C                                       midnight?
      DO 200 I = 1,NB
         ITB = TB(I)
         TB(I) = TB(I) - ITB
         TE(I) = TE(I) - ITB
         IF (TE(I).GT.1.0) THEN
            IF (TB(I).LT.1.0) THEN
               TE(I) = -TE(I)
            ELSE
               TB(I) = TB(I) - 1
               TE(I) = TE(I) - 1
               END IF
            END IF
  200    CONTINUE
      GO TO 999
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BASVIS: ERROR', I3, ' INIT-FOR-READ VIS FILE')
 1020 FORMAT ('BASVIS: ERROR', I3, ' READING VIS FILE')
 1040 FORMAT ('BASVIS: ERROR', I3,' Number of vis.',I6,' > Max = 50000')
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
      CHARACTER STNAME*8
      DOUBLE PRECISION  DX, DY, DZ, LAMBDA
      INCLUDE 'UVCRS.INC'
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
     *   NOPCAL, ANTNIF, ANFQID,IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      DX = ARRAYC(1)
      DY = ARRAYC(2)
      DZ = ARRAYC(3)
      NANT = IABUF(5)
      LAMBDA = 299792.D3/AFREQ
      ARRLON = 0.0D0
      IF ((ABS(DX).GT.1.0D2) .AND. (ABS(DY).GT.1.D2)) ARRLON =
     *   ATAN2 (DY, DX)
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
C                                       RH system here
         XA(NOSTA) = STAXYZ(1) / LAMBDA
         YA(NOSTA) = STAXYZ(2) / LAMBDA
         ZA(NOSTA) = STAXYZ(3) / LAMBDA
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
 1000 FORMAT ('ANTIN: ERROR IN OPEN AN-FILE IERR = ',I6)
 1010 FORMAT ('ANTIN: ERROR IN FINDING STATION',I3,' IERR=',I3)
 1030 FORMAT ('ANTIN: ERROR IN CLOSING AN-FILE IERR = ',I6)
      END
      SUBROUTINE CROSAT (TB, TE, NCR, NCROSS, TDI, TDK, SIMB)
C-----------------------------------------------------------------------
C   Find possible crossing comparing UVRADI with precalculated minimum
C   distance between points of the ellipses
C   Inputs:
C      TB            R    Array of beginning times
C      TE            R    Array of ending times
C   Inputs in common:
C      UVRADI        R    The threshold determining the crossing in
C                         billions of wavelength.
C      DELTA         R    Declination of the source in radians
C      ALFA          R    Right assension of the source in radians
C      ANTNO(*)      I    Array of antennas' numbers
C      XA,YA,ZA(*)   D    Cartesian coordinates for stations in billion
C                         of wavelengths.
C      STANAM(*)     C*8  Names of stations
C      NANT          I    Number of antennas
C   Outputs:
C      NCR           I    Number of possible crossing
C      NCROSS(*)     I    Array of identificators of crossing: 8 digits:
C                         each consecutive pair of digits corresponds
C                         to the antenna number.
C      TDI(*)        R    Moments of intersection at the first antenna
C      TDK(*)        R    Moments of intersection at the first antenna
C      SIMB          C*1  Simbol marking an nonintersect crossing
C-----------------------------------------------------------------------
      INTEGER    NB, ANI, ANK, NCR, NBAI, NBAK, NCROSS(*),
     *           NBASE(100),I, K, L, M
      CHARACTER  SIMB(*)*1
      LOGICAL    INTER1, INTER2, INTERS, MINRA1, MINRAN, TOT,
     *           NONINT, TIME1, TIME2, TIMEK, TIMEI, NONCON, TIMEIK
      REAL       BX, BY, BQ(1125), BZ(1125), RANMI1, RMIN1, RMIN2,
     *           RMIN3, RMIN4, RMIN5, RMIN6, TI(10), RMIN7, RMIN8,
     *           R1MIN, TB(*), TE(*), GSALF, TK(10), DEZ1, DEZ2, BSQR,
     *           A(1000), HI, HK, UTI, UTK, TAND, DELTZ1, DELTZ2,
     *           DELTBQ, BQS, TDI(*), TDK(*)
      INCLUDE 'UVCRS.INC'
C---------------------------------------------------------------------
C                                       Determine pairs of baselines
C                                       which can give a crossing
      GSALF = GST0*GRRAD - ALFA
      DAYRAD = (1.0 - 1.0/365) / (2.0*PI)
      TAND = TAN(DELTA)
      NB = 0
      DO 20 I = 1, NANT-1
         ANI = ANTNO(I)
         DO 10 K = I+1, NANT
            ANK = ANTNO(K)
            NB = NB + 1
            NBASE(NB) = 100*ANI + ANK
C                                       The first two digits at NBASE
C                                       is the first antenna number;
C                                       The second two digits is the
C                                       second antenna number.
            BX = XA(ANK) - XA(ANI)
            BY = YA(ANK) - YA(ANI)
            BQ(NB) = SQRT (BX*BX + BY*BY)
            BZ(NB) = ZA(ANK) - ZA(ANI)
C                                       make left handed
            A(NB) = -ARRLON + ATAN2 (-BY, BX)
C                                       force negative BZ
            IF (BZ(NB).GT.0.0) THEN
               BZ(NB) = -BZ(NB)
               A(NB) = A(NB) + PI
               END IF
   10       CONTINUE
   20    CONTINUE
C                                       increase timerange by 60 min
C                                       for including nearest crosing
C-----------------------------------------------
      DO 30 I=1,NB
         TB(I) = TB(I) - 0.04
         IF (TE(I).GT.0) THEN
            TE(I) = TE(I) + 0.04
            ELSE
            TE(I) = TE(I) -0.04
            END IF
   30    CONTINUE
C-------------------------------------------------
C                                       looking for crossing
      NCR = 0
      DO 100 I = 1, NB-1
         NBAI = NBASE(I)
         DO 90 K = I+1,NB
C                                       negative values for time of
C                                       intersection if is not realized
            DO 40 M = 1,10
               TI(M) = -1.0
               TK(M) = -1.0
   40       CONTINUE
            NBAK = NBASE(K)
            DELTZ1 = BZ(I) - BZ(K)
            DELTZ2 = BZ(I) + BZ(K)
            DEZ1 = DELTZ1 / TAND
            DEZ2 = DELTZ2 / TAND
            DELTBQ = BQ(I) - BQ(K)
            BQS =    BQ(I) + BQ(K)
            BSQR = BQ(I)*BQ(I) - BQ(K)*BQ(K)
C---------------------------------------------------------------------
C                                       condition of intersection
C                                       with up-ellipse
            INTER1 = ((TAND.LT.ABS(DELTZ1/DELTBQ))
     *          .AND. (TAND.GT.ABS(DELTZ1/BQS)))
            IF (INTER1) THEN
C                                       the first pair of intersection
               TIME1 = .FALSE.
               DO 50 L = 1,2
                  HI = ((-1)**L)*ACOS((BSQR+DEZ1**2)/(2*BQ(I)*DEZ1))
                  HK = ((-1)**L)*ACOS((BSQR-DEZ1**2)/(2*BQ(K)*DEZ1))
C                                       time of an intersection at days
                  UTI = (HI - GSALF + A(I)) * DAYRAD
                  UTI = AMOD(UTI,1.0)
                  IF (UTI.LT.0.0) UTI = UTI + 1
C
                  UTK = (HK - GSALF + A(K)) * DAYRAD
                  UTK = AMOD(UTK,1.0)
                  IF (UTK.LT.0.0) UTK = UTK + 1
C
                  IF (TE(I).LT.0) THEN
C                                       Midnight between beg. and end
                      TIMEI = ((UTI.GT.TB(I)) .AND. (UTI.LT.1.0))
     *               .OR. ((UTI.GT.0.0) .AND. (UTI.LT.(-TE(I)-1 )))
                   ELSE
                      TIMEI = (UTI.GT.TB(I)) .AND. (UTI.LT.TE(I))
                      END IF
                   IF (TE(K).LT.0) THEN
C                                       Midnight between beg. and end
                      TIMEK = ((UTK.GT.TB(K)) .AND. (UTK.LT.1.0))
     *               .OR. ((UTK.GT.0.0) .AND. (UTK.LT.(-TE(K) -1)))
                   ELSE
                      TIMEK = (UTK.GT.TB(K)) .AND. (UTK.LT.TE(K))
                      END IF
                   IF ( (TIMEI .AND. TIMEK)) THEN
                      TI(L) = UTI
                      TK(L) = UTK
                      END IF
                   TIME1 = TIME1 .OR. (TIMEI .AND. TIMEK)
   50           CONTINUE
               END IF
C-----------------------------------------------------------------------
C                                       condition of intersection
C                                       with down-ellipse
            INTER2 = ((TAND.LT.ABS(DELTZ2/DELTBQ))
     *          .AND. (TAND.GT.ABS(DELTZ2/BQS)))
            IF (INTER2) THEN
C                                       the second pair of intersection
               TIME2 = .FALSE.
               DO 60 L = 1,2
                 HI = ((-1)**(L+1))*ACOS((BSQR+DEZ2**2)/(2*BQ(I)*DEZ2))
                  HK = ((-1)**L)*ACOS(-(BSQR-DEZ2**2)/(2*BQ(K)*DEZ2))
C                                       time of an intersection at days
                  UTI = (HI - GSALF + A(I)) * DAYRAD
                  UTI = AMOD(UTI,1.0)
                  IF (UTI.LT.0.0) UTI = UTI + 1
C
                  UTK = (HK - GSALF + A(K)) * DAYRAD
                  UTK = AMOD(UTK,1.0)
                  IF (UTK.LT.0.0) UTK = UTK + 1
C
                  IF (TE(I).LT.0) THEN
C                                       Midnight between beg. and end
                      TIMEI = ((UTI.GT.TB(I)) .AND. (UTI.LT.1.0))
     *               .OR. ((UTI.GT.0.0) .AND. (UTI.LT.(-TE(I)-1 )))
                   ELSE
                      TIMEI = (UTI.GT.TB(I)) .AND. (UTI.LT.TE(I))
                   END IF
                   IF (TE(K).LT.0) THEN
C                                       Midnight between beg. and end
                      TIMEK = ((UTK.GT.TB(K)) .AND. (UTK.LT.1.0))
     *               .OR. ((UTK.GT.0.0) .AND. (UTK.LT.(-TE(K) -1)))
                   ELSE
                      TIMEK = (UTK.GT.TB(K)) .AND. (UTK.LT.TE(K))
                   END IF
                   IF ( (TIMEI .AND. TIMEK)) THEN
                      TI(L+2) = UTI
                      TK(L+2) = UTK
                      END IF
                   TIME2 = TIME2 .OR. (TIMEI .AND. TIMEK)
   60           CONTINUE
                END IF
C-----------------------------------------------------------------------
            INTER1 = INTER1 .AND. TIME1
            INTER2 = INTER2 .AND. TIME2
C                                       condition of any intersect
            INTERS = INTER1 .OR. INTER2
C-----------------------------------------------------------------------
C                                       unconditional minimum
            RMIN1 = ABS(DELTBQ*SIN(DELTA) + DELTZ1*COS(DELTA))
            RMIN2 = ABS(DELTBQ*SIN(DELTA) - DELTZ1*COS(DELTA))
            RMIN3 = ABS(BQS*SIN(DELTA) + DELTZ1*COS(DELTA))
            RMIN4 = ABS(BQS*SIN(DELTA) - DELTZ1*COS(DELTA))
            RMIN5 = ABS(DELTBQ*SIN(DELTA) + DELTZ2*COS(DELTA))
            RMIN6 = ABS(DELTBQ*SIN(DELTA) - DELTZ2*COS(DELTA))
            RMIN7 = ABS(BQS*SIN(DELTA) + DELTZ2*COS(DELTA))
            RMIN8 = ABS(BQS*SIN(DELTA) - DELTZ2*COS(DELTA))
            R1MIN = MIN(RMIN1, RMIN2, RMIN3, RMIN4, RMIN5,
     *                  RMIN6, RMIN7, RMIN8)
C----------------------------------------------------------------------
            NONCON = R1MIN.LT.UVRADI
C                                       Looking for times of uncon-
C                                       ditional crossing; h=0, h=PI
C                                       condition of intersection
C                                       with down-ellipse
            IF (NONCON) THEN
               TIME1 = .FALSE.
               DO 65 L = 1,2
C                                       time of an intersection at days
                  UTI = ( (L-1)*PI - GSALF + A(I)) * DAYRAD
                  UTI = AMOD(UTI,1.0)
                  IF (UTI.LT.0.0) UTI = UTI + 1
                  IF (TE(I).LT.0) THEN
C                                       Midnight between beg. and end
                      TIMEI = ((UTI.GT.TB(I)) .AND. (UTI.LT.1.0))
     *               .OR. ((UTI.GT.0.0) .AND. (UTI.LT.(-TE(I)-1 )))
                   ELSE
                      TIMEI = (UTI.GT.TB(I)) .AND. (UTI.LT.TE(I))
                   END IF
                   DO 62 M = 1,2
                      UTK = ((M-1)*PI - GSALF + A(K)) * DAYRAD
                      UTK = AMOD(UTK,1.0)
                      IF (UTK.LT.0.0) UTK = UTK + 1
                      IF (TE(K).LT.0) THEN
C                                       Midnight between beg. and end
                         TIMEK = ((UTK.GT.TB(K)) .AND. (UTK.LT.1.0))
     *                      .OR. ((UTK.GT.0.0) .AND.
     *                            (UTK.LT.(-TE(K) -1)))
                      ELSE
                         TIMEK = (UTK.GT.TB(K)) .AND. (UTK.LT.TE(K))
                      END IF
                      TIMEIK = TIMEI . AND. TIMEK
                      IF (TIMEIK) THEN
                         TI(M + 2*(L-1) + 4) = UTI
                         TK(M + 2*(L-1) + 4) = UTK
                         END IF
                      TIME1 = TIME1 .OR. TIMEIK
   62                 CONTINUE
   65              CONTINUE
                END IF
C-----------------------------------------------------------------------
                NONCON = NONCON .AND. TIME1
C                                       condition of random minimum
            MINRA1 = ((TAND.LT.ABS(DELTBQ/DELTZ1)))
C                                        random minimum
            RANMI1 = SQRT(DELTBQ**2 + DELTZ1**2)
            MINRAN = MINRA1 .AND. (RANMI1.LT.UVRADI)
C                                       time of up-random minimum
            IF (MINRAN) THEN
               TIME1 = .FALSE.
               DO 70 L = 1,2
                  HI = ((-1)**(L+1))*ACOS(-(DELTZ1*TAND)/(DELTBQ))
                  HK = HI
C                                       time of an intersection at days
                  UTI = (HI - GSALF + A(I)) * DAYRAD
                  UTI = AMOD(UTI,1.0)
                  IF (UTI.LT.0.0) UTI = UTI + 1
                  IF (TE(I).LT.0) THEN
C                                       Midnight between beg. and end
                      TIMEI = ((UTI.GT.TB(I)) .AND. (UTI.LT.1.0))
     *               .OR. ((UTI.GT.0.0) .AND. (UTI.LT.(-TE(I)-1 )))
                   ELSE
                      TIMEI = (UTI.GT.TB(I)) .AND. (UTI.LT.TE(I))
                   END IF
                   UTK = (HK - GSALF + A(K)) * DAYRAD
                   UTK = AMOD(UTK,1.0)
                   IF (UTK.LT.0.0) UTK = UTK + 1
                   IF (TE(K).LT.0) THEN
C                                       Midnight between beg. and end
                       TIMEK = ((UTK.GT.TB(K)) .AND. (UTK.LT.1.0))
     *                      .OR. ((UTK.GT.0.0) .AND.
     *                            (UTK.LT.(-TE(K) -1)))
                    ELSE
                         TIMEK = (UTK.GT.TB(K)) .AND. (UTK.LT.TE(K))
                    END IF
                    TIMEIK = TIMEI . AND. TIMEK
                    IF (TIMEIK) THEN
                       TI(L + 8) = UTI
                       TK(L + 8) = UTK
                       END IF
                      TIME1 = TIME1 .OR. TIMEIK
   70              CONTINUE
                END IF
C-----------------------------------------------------------------------
            MINRAN = MINRAN .AND. TIME1
C                                       condition of crossing
            NONINT = MINRAN .OR. NONCON
            TOT = INTERS .OR. NONINT
            IF (TOT) THEN
               NCR = NCR + 1
C                                       each next two digits at NCROSS
C                                       is the number of antenna
               NCROSS(NCR) = 10000*NBAI + NBAK
C                                       store moments of crossing
               DO 80 M = 1,10
                  TDI(M + 10*(NCR-1)) = TI(M)
                  TDK(M + 10*(NCR-1)) = TK(M)
 80               CONTINUE
                IF (.NOT. NONINT) THEN
                  SIMB(NCR) = ' '
                 ELSE
                  SIMB(NCR) = '*'
                  END IF
               END IF
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RATCRO (A1, A2, B1, B2, NB, TT1, TT2,
     *       BASN, SBASN, ANN1, ANN2, TALL, UALL, VALL, AMALL,
     *       WEALL, RAT, RATL)
C-----------------------------------------------------------------------
C   Find average ratio of amplitudes of baselines A1-A2 and B1-B2
C   Inputs:

C      A1      I    Number of the first antenna of the first base
C      A2      I    Number of the second antenna of the first base
C      B1      I    Number of the first antenna of the second base
C      B2      I    Number of the second antenna of the second base
C      NB      I    Number of possible baselines
C      TT1     R    Moments of intersections of the first antenna
C      TT2     R    Moments of intersections of the second antenna
C      BASN    I    Array of numbers of visibilities for a given antenna
C      SBASN   I    Array of number of  visibilities of preceded
C                   anntennas
C      ANN1    I    Array of the first antennas numbers
C      ANN2    I    Array of the second antennas numbers
C      TALL    R    Array of all T
C      UALL    R    Array of all U
C      VALL    R    Array of all V
C      AMALL   R    Array of all amplitudes
C      WEALL   R    Array of all weeghts
C   Inputs in COMMON:
C      UVRADX  R    Threshold at U billion of wavelength
C      UVRADY  R    Threshold at V billion of wavelength
C   Outputs:
C      RAT     R    Average ratio of amplitudes
C      RATL    R    Natural logarithm of the ratio
C-----------------------------------------------------------------------
      INTEGER A1, A2, B1, B2, KSUM, ICROS, BASN(*), SBASN(*)
      INTEGER NUMREF, NUMNEI, I, K, L, M, NB, ANN1(*), ANN2(*),
     *        KREF, KNEI, NUMRE, NUMNE
      REAL  UREF(1000), VREF(1000), AMPREF(1000), WEREF(1000),
     *      UNEI(1000), VNEI(1000), AMPNEI(1000), WENEI(1000),
     *      TALL(*), UALL(*), VALL(*), AMALL(*), WEALL(*)
      REAL  RAT, RATL, RATT, UR1, UR2, VR1, VR2, TT1(*), TT2(*)
      REAL  TRES, TGIVEN, DT, TCROS, TR, H, R, RER
      LOGICAL GOOD
      INCLUDE 'UVCRS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C---------------------------------------------------------------------
C                                       time interval near crossing
      TR = 0.04
C                                       find a baseline number KREF
C                                       corresponding to A1&A2
      DO 30 I = 1, NB
         IF ( (A1.EQ.ANN1(I)) .AND. (A2.EQ.ANN2(I))) THEN
            KREF = I
            GO TO 40
            END IF
 30      CONTINUE
 40   NUMREF = BASN(KREF)
C                                       find a baseline number KNEI
C                                       corresponding to B1&B2
      DO 50 I = 1, NB
         IF ( (B1.EQ.ANN1(I)) .AND. (B2.EQ.ANN2(I))) THEN
            KNEI = I
            GO TO 60
            END IF
 50      CONTINUE
 60   NUMNEI = BASN(KNEI)
C                                       Loop for groups of
C                                       intersections
      TRES = TR
      KSUM = 0
      RAT = 0.0
      DO 200 M = 1,10
C                                       find the data for A1&A2
         L = 0
         IF (M.GT.4) TRES = TR * 2
         TCROS = TT1(M)
         IF (TCROS.GT.0.0) THEN
            DO 70 I = 1, NUMREF
               TGIVEN = TALL(SBASN(KREF) + I)
               IF (TGIVEN.GT.1.0) TGIVEN = TGIVEN - 1.0
               DT = ABS (TCROS - TGIVEN)
               DT = MIN(DT, (1-DT))
               IF (DT.LT.TRES) THEN
                  L = L + 1
                  UREF(L) = UALL(SBASN(KREF) + I)
                  VREF(L) = VALL(SBASN(KREF) + I)
                  AMPREF(L) = AMALL(SBASN(KREF) + I)
                  WEREF(L) = WEALL(SBASN(KREF) + I)
                  END IF
 70            CONTINUE
            END IF
         NUMRE = L
C                                       find the data for B1&B2
         L = 0
         TCROS = TT2(M)
         IF (TCROS.GT.0.0) THEN
            DO 80 I = 1, NUMNEI
               TGIVEN = TALL(SBASN(KNEI) + I)
               IF (TGIVEN.GT.1.0) TGIVEN = TGIVEN - 1.0
               DT = ABS (TCROS - TGIVEN)
               DT = MIN(DT, (1-DT))
               IF (DT.LT.TRES) THEN
                  L = L + 1
                  UNEI(L) = UALL(SBASN(KNEI) + I)
                  VNEI(L) = VALL(SBASN(KNEI) + I)
                  AMPNEI(L) = AMALL(SBASN(KNEI) + I)
                  WENEI(L) = WEALL(SBASN(KNEI) + I)
                  END IF
 80            CONTINUE
            END IF
         NUMNE = L
C
         IF ((NUMRE.GT.0) .AND. (NUMNE.GT.0)) THEN
            DO 100 I = 1, NUMRE
               RATT = 0.0
               ICROS = 0
               DO 90 K = 1, NUMNE
                  UR1 = ABS(UREF(I) - UNEI(K))
                  VR1 = ABS(VREF(I) - VNEI(K))
                  UR2 = ABS(UREF(I) + UNEI(K))
                  VR2 = ABS(VREF(I) + VNEI(K))
C                                       criteria of crossing
C-----------------------------------------------------------------
                  GOOD = .FALSE.
C                                       rectangular criteria
                  IF (RECT) THEN
                     IF (   ((UR1.LE.UVRADX) .AND.
     *                       (VR1.LE.UVRADY))
     *                  .OR.((UR2.LE.UVRADX) .AND.
     *                  (VR2.LE.UVRADY)) ) GOOD = .TRUE.
                     END IF
C                                       elliptical criteria
                  IF (ELIP) THEN
                     IF (UR1.EQ.0.0) THEN
                        H = PI/2.0
                     ELSE
                        H = ATAN(VR1 / UR1)
                     END IF
                     R = (UVRADX*COS(H))**2 + (UVRADY*SIN(H))**2
                     RER = VR1*VR1 + UR1*UR1
                     IF (RER.LE.R) GOOD = .TRUE.
                     IF (UR2.EQ.0.0) THEN
                        H = PI/2.0
                     ELSE
                        H = ATAN(VR2 / UR2)
                     END IF
                     R = (UVRADX*COS(H))**2 + (UVRADY*SIN(H))**2
                     RER = VR2*VR2 + UR2*UR2
                     IF (RER.LE.R) GOOD = .TRUE.
                     END IF
C---------------------------------------------------------------------
                  IF (GOOD) THEN
                     ICROS = ICROS + 1
C                                       Logarithm of amplitudes are
C                                       used. So we subtract them
C                                       instead of devide
                     RATT = RATT + AMPNEI(K) - AMPREF(I)
                     END IF
 90               CONTINUE
               IF (ICROS.NE.0) THEN
                  RATT = RATT/ICROS
                  KSUM = KSUM + 1
                  RAT = RAT + RATT
                  END IF
 100           CONTINUE
            END IF
 200     CONTINUE
      NUMCR = KSUM
      IF (KSUM.NE.0) THEN
         RAT = RAT/KSUM
C                                       Return back to ratios from LOG
         RATL = RAT
         RAT = EXP (RATL)
      ELSE
         RAT = 0.0
         END IF
C
 999  RETURN
      END
