LOCAL INCLUDE 'CLCOR.INC'
C                                                         Include CLCOR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLTAB.INC'
C                                       Inputs and general info
      INTEGER NDAT
      PARAMETER (NDAT= 100000)
      INTEGER   NLINES, IAN(NDAT), REFANT
      DOUBLE PRECISION TMES(NDAT), SPACEX(NDAT), SPACEY(NDAT),
     *   SPACEZ(NDAT)
C                                       for 'EOPS'
      INTEGER MAXGRO, MAXROW
      PARAMETER (MAXGRO = 200)
      PARAMETER (MAXROW = MAXGRO*10)
      INTEGER   BCOUNT, ECOUNT, NCOUNT, NCTROW, ICASE,
     *   NACUMU(MAXGRO), NATGRO(MAXGRO), NGROUP
C                                       for 'IONO'
      INTEGER ISFAC
      REAL RATIO
C
      DOUBLE PRECISION UT1TAI(40), XYWOB(2,40), JD
      DOUBLE PRECISION UT1C(MAXROW), WOBX(MAXROW), WOBY(MAXROW),
     *   LEAPS(MAXROW), TBEG(MAXGRO), TEND(MAXGRO), INDAY(MAXROW)
      REAL VERDEL(NDAT), CLKDEL(NDAT), DVERDE(NDAT), DCLKDE(NDAT),
     *   RADR(NDAT), DECDR(NDAT)
C
      INTEGER   SEQIN, SUBA, DISKIN, CNOIN, NUMHIS, CLVER, CLUSE,
     *   NSOUWD, SOUWAN(30), NANTSL, ANTENS(50), BIF, EIF, ISTOK,
     *   FREQID, NTERM
      INTEGER    NPLANE
      CHARACTER  PLANET(10)*12
      DOUBLE PRECISION PLMASS(10)
      LOGICAL   DOSWNT, DOAWNT, DESEL
      REAL      XSIN, XDISIN, XFQID, XBAND, XFREQ, XBIF, XEIF, XTIME(8),
     *   XANT(50), XSUBA, XGVER, XGUSE, BPARM(20), XBAD(10), SELBAN,
     *   AXOFF
      CHARACTER  HISCRD(30)*64, NAMEIN*12, CLAIN*6, XSOUR(30)*16,
     *   XSTOK*4, OPCODE*4, INFILE*48
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSTOK, XOPCOD,
     *   XINFIL(12)
      DOUBLE PRECISION FRQOFF(MAXIF), SELFRQ
C                                       Buffers and file info
      INTEGER   BUFFER(1024)
C                                       Important constants
      INCLUDE 'INCS:PSTD.INC'
C                                       Internal storage
      INTEGER   CLRECI(13+32*MAXIF), CLKOLS(MAXCLC), CLNUMV(MAXCLC),
     *   NUMANT, NUMPOL, NUMIF, ICODE, FIXCNT, TIMCL,  INTCL, SOUCL,
     *   ANTCL, SUBCL, FRQCL, IFRCL, GDLCL, DOPCL, ATMCL, DATMCL,
     *   MBD1CL, CLK1CL, DCK1CL, DIS1CL, DDS1CL, RE1CL, IM1CL, DE1CL,
     *   RA1CL, WE1CL, RF1CL, MBD2CL, CLK2CL, DCK2CL, DIS2CL, DDS2CL,
     *   RE2CL, IM2CL, DE2CL, RA2CL, WE2CL, RF2CL
      REAL      GMMOD, CLRECR(13+32*MAXIF), PARM(40), PANGLE(MAXANT)
      DOUBLE PRECISION COSDEC, SINDEC, CLRECD(13+32*MAXIF)
C                                       Inputs and general info
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XXSTOK,
     *   XBAND, XFREQ, XFQID, XBIF, XEIF, XTIME, XANT, XSUBA, XGVER,
     *   XGUSE, XOPCOD, BPARM, XBAD, XINFIL, SELBAN, SEQIN,
     *   DISKIN, CNOIN, SUBA, CLVER, CLUSE
      COMMON /CINFO/ FRQOFF, SELFRQ, DOSWNT, DOAWNT, DESEL, NSOUWD,
     *   SOUWAN, NANTSL, ANTENS, BIF, EIF, ISTOK, FREQID, NUMHIS
      COMMON /GETA/ NLINES, IAN, VERDEL, CLKDEL, DVERDE, DCLKDE,
     *   RADR, DECDR, REFANT
      COMMON /GETAD/ TMES, SPACEX, SPACEY, SPACEZ, UT1TAI, XYWOB,JD
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, XSOUR, XSTOK, OPCODE,
     *   INFILE
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFER
C                                       Common for selected planets
      COMMON /PLANI/ NPLANE
      COMMON /PLANC/ PLANET
      COMMON /PLANM/ PLMASS
C                                       for 'EOPS'
      COMMON /EOPS/ UT1C, WOBX, WOBY, LEAPS, TBEG, TEND, INDAY, BCOUNT,
     *   ECOUNT, NCOUNT, NCTROW, ICASE, NACUMU, NATGRO, NGROUP
C                                       for 'IONO'
      COMMON /IONO/ ISFAC, RATIO
C                                       Internal storage
      COMMON /CLRECC/ COSDEC, SINDEC, CLRECD, GMMOD, PARM, PANGLE,
     *   AXOFF, NTERM, FIXCNT, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *   ICODE
      EQUIVALENCE (CLRECI, CLRECR, CLRECD)
      EQUIVALENCE (CLKOLS(CLDTIM), TIMCL), (CLKOLS(CLRTMI), INTCL),
     *   (CLKOLS(CLISID),SOUCL), (CLKOLS(CLIANT),ANTCL),
     *   (CLKOLS(CLISUB),SUBCL), (CLKOLS(CLIFQI),FRQCL),
     *   (CLKOLS(CLRIFR),IFRCL), (CLKOLS(CLDDEL),GDLCL),
     *   (CLKOLS(CLRDOP),DOPCL), (CLKOLS(CLRATM),ATMCL),
     *   (CLKOLS(CLRDAT),DATMCL)
      EQUIVALENCE (CLKOLS(CLRMD1),MBD1CL),
     *   (CLKOLS(CLRCK1),CLK1CL), (CLKOLS(CLRDC1),DCK1CL),
     *   (CLKOLS(CLRDS1),DIS1CL), (CLKOLS(CLRDD1),DDS1CL),
     *   (CLKOLS(CLRRE1),RE1CL), (CLKOLS(CLRIM1),IM1CL),
     *   (CLKOLS(CLRRA1),RA1CL), (CLKOLS(CLRDE1),DE1CL),
     *   (CLKOLS(CLRWE1),WE1CL), (CLKOLS(CLIRF1),RF1CL)
      EQUIVALENCE (CLKOLS(CLRMD2),MBD2CL),
     *   (CLKOLS(CLRCK2),CLK2CL), (CLKOLS(CLRDC2),DCK2CL),
     *   (CLKOLS(CLRDS2),DIS2CL), (CLKOLS(CLRDD2),DDS2CL),
     *   (CLKOLS(CLRRE2),RE2CL), (CLKOLS(CLRIM2),IM2CL),
     *   (CLKOLS(CLRRA2),RA2CL), (CLKOLS(CLRDE2),DE2CL),
     *   (CLKOLS(CLRWE2),WE2CL), (CLKOLS(CLIRF2),RF2CL)
C                                                          End CLCOR
LOCAL END
      PROGRAM CLCOR
C-----------------------------------------------------------------------
C! Determines applies calibration corrections to the CL table.
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2001-2012, 2014-2017
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
C   Task CLCOR applies corrections to CL tables.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'CLCOR '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL CLCLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Apply corrections
      CALL CLCUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy and update HI file.
      CALL CLCLHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
 999  STOP
      END
      SUBROUTINE CLCLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   CLCLIN gets input parameters for CLCOR.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                1 => Invalid request
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      CHARACTER STAT*4, UTYPE*2
      LOGICAL   T, F, ALLANT, MATCH
      INTEGER   NPARM, IERR, I, NEXT, IARG, LIMIT, J, IROUND, LUN,
     *   LUN2, IIVER, NUMCL, TABUFF(512)
      INCLUDE 'CLCOR.INC'
      INTEGER   DUMMY(MAXIF)
      REAL      FINC(MAXIF), BUFF1(2048)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, LUN2  /29, 28/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      NUMHIS = 0
      REFANT = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 237
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBA = XSUBA + 0.5
      IF (SUBA.LE.0) SUBA = 1
C
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 20      CONTINUE
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 25      CONTINUE
C                                       Find file, read CATBLK
      CNOIN = 1
      STAT = 'SRCH'
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      NRPARM = CATBLK(KIPCN)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FREQID = IROUND (XFQID)
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FREQID, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       determine number of CL tables
      CALL FNDEXT ('CL', CATBLK, NUMCL)
      IF (NUMCL.LE.0) THEN
         MSGTXT = 'NO CL TABLES FOUND, CANNOT CLCOR'
         JERR = 1
         GO TO 990
         END IF
      CLVER = IROUND (XGVER)
      IF ((CLVER.LE.0) .OR. (CLVER.GT.NUMCL)) CLVER = NUMCL
      CLUSE = IROUND (XGUSE)
C                                       copy CLVER table to CLUSE table
      IF (CLUSE.NE.CLVER) THEN
         CLUSE = NUMCL + 1
         CALL TABCOP ('CL', CLVER, CLUSE, LUN, LUN2, DISKIN, DISKIN,
     *      CNOIN, CNOIN, CATBLK, BUFF1, TABUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
      ELSE IF (CLUSE.EQ.1) THEN
         JERR = 1
         MSGTXT = 'MODIFYING CL TABLE VER. 1 IS NOT ALLOWED'
         CALL MSGWRT (8)
         MSGTXT = 'USE GAINUSE = 0 TO MAKE A NEW ONE'
         GO TO 990
         END IF
      WRITE (MSGTXT,1055) CLVER, CLUSE
      CALL MSGWRT (4)
C                                       IF range
      BIF = IROUND (XBIF)
      EIF = IROUND (XEIF)
      IF (BIF.LE.0) BIF = 1
      IF ((EIF.LE.0) .AND. (JLOCIF.GT.0)) EIF = CATBLK(KINAX+JLOCIF)
      IF (EIF.LE.0) EIF = 1
      IF ((JLOCIF.GT.0) .AND. (BIF.GT.CATBLK(KINAX+JLOCIF)))
     *   BIF = CATBLK(KINAX+JLOCIF)
      IF ((JLOCIF.GT.0) .AND. (EIF.GT.CATBLK(KINAX+JLOCIF)))
     *   EIF = CATBLK(KINAX+JLOCIF)
C                                       Stokes' type.
      ISTOK = 0
      IF (XSTOK.EQ.'R   ') ISTOK = 1
      IF (XSTOK.EQ.'L   ') ISTOK = 2
      IF (XSTOK.EQ.'I   ') ISTOK = -1
C                                       Check Stokes'
      IF (ISTOK.EQ.0) THEN
C                                       If none selected take what you
C                                       have.
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND. (ABS (CATD(KDCRV+JLOCS)
     *      +1.0D0).LE.0.5D0)) ISTOK = 1
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND. (ABS (CATD(KDCRV+JLOCS)
     *      +2.0D0).LE.0.5D0)) ISTOK = 2
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND. (ABS (CATD(KDCRV+JLOCS)
     *      -1.0D0).LE.0.5D0)) ISTOK = -1
      ELSE
C                                       Is selected Stokes' available?
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND.
     *      (ABS (CATD(KDCRV+JLOCS)+ISTOK).GT.0.5D0)) THEN
            JERR = 1
            MSGTXT = 'STOKES ' // XSTOK // ' UNAVAILABLE IN DATA'
            GO TO 990
            END IF
         END IF
      IF ((ISTOK.EQ.2) .AND. (NCOR.EQ.1)) ISTOK = 1
C                                       Check sort order of input
      IF (ISORT(1:2).NE.'TB') THEN
         WRITE (MSGTXT,1060) ISORT
         JERR = 1
         GO TO 990
         END IF
      JERR = 0
C                                       Antenna list
      ALLANT = T
      DESEL = F
      DO 100 I = 1,50
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
         DESEL = DESEL .OR. (XANT(I).LT.-0.5)
 100     CONTINUE
      NEXT = 1
      IF (ALLANT) GO TO 160
C                                       Not all selected - make list
C                                       ANTENNAS array.
         DO 150 I = 1,50
            IARG = ABS (XANT(I)) + 0.5
            IF (IARG.EQ.0) GO TO 150
C                                       See if already have
               LIMIT = NEXT - 1
               IF (LIMIT.LT.1) GO TO 140
               DO 130 J = 1,LIMIT
                  IF (IARG.EQ.ANTENS(J)) GO TO 150
 130              CONTINUE
C                                       New antenna
 140              ANTENS(NEXT) = IARG
                  NEXT = NEXT + 1
 150           CONTINUE
 160  DOAWNT = .NOT. DESEL
      NANTSL = NEXT - 1
C                                       Get source numbers
      CALL FNDSOU (DISKIN, CNOIN, XSOUR, BUFFER, NSOUWD, DOSWNT,
     *   SOUWAN, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBA, CATBLK, BUFFER, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Get IF information
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NUMIF, FRQOFF, DUMMY, FINC, BNDCOD, FREQID, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Get observing bandwidth. Assume
C                                       all IFs have same increment.
      BANDW  = CATBLK(KINAX+JLOCF) * FINC(BIF)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLCLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR: COPYING INPUT CL TO OUTPUT:',I4)
 1055 FORMAT ('CL version input',I4,' output',I4)
 1060 FORMAT ('INPUT VIS RECORDS MISORDERED, SORTED = ',A2,
     *   ' SHOULD BE = TB')
       END
      SUBROUTINE CLCUV (IERR)
C-----------------------------------------------------------------------
C   CLCUV is called from CLCOR. CLCUV reads throught the CL table,
C   passing the records selected to the correction routine CLCCOR.
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN, IRCODE, THSOU, LSTSOU, ANT, I, JERR, IFNO(2),
     *   ICLRNO, NUMREC, LOOP
      LOGICAL   SLCTD
      DOUBLE PRECISION TIMBEG, TIMEND
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /29/
C-----------------------------------------------------------------------
      FIXCNT = 0
C                                       If OPCODE='POLR' modify AN table
      IF (OPCODE.EQ.'POLR') THEN
         IFNO(1) = BIF
         IFNO(2) = EIF
C                                       Too many IFs, > 20.
         IF ((EIF-BIF+1).GT.20) THEN
            IERR = 10
            I = EIF - BIF + 1
            WRITE (MSGTXT,1000) I
            GO TO 990
            END IF
         CALL ANTCOR (DISKIN, CNOIN, SUBA, CATBLK, IFNO, BPARM, BUFFER,
     *      FREQID, IERR)
         IF (IERR.GT.0) GO TO 999
         IERR = 0
         END IF
C                                       Timerange
      TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
C                                       If OPCODE='ANAX' modify AN table
C                                       and exclude time influence
      IF (OPCODE.EQ.'ANAX') THEN
         CALL AXCOR (DISKIN, CNOIN, SUBA, CATBLK, BUFFER, NANTSL,
     *      ANTENS, BPARM, IERR)
         IF (IERR.NE.0) GO TO 999
         TIMBEG = 0.0
         TIMEND = 1.0E20
         END IF
C                                       Open CL table
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Reformat table?
      CALL CLREFM (DISKIN, CNOIN, CLUSE, CATBLK, LUN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CALINI ('WRIT', BUFFER, DISKIN, CNOIN, CLUSE, CATBLK, LUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      IRCODE = 0
C                                       Initial call to CLCCOR
      CALL CLCCOR (1, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Update table
      DO 500 LOOP = 1,NUMREC
         ICLRNO = LOOP
         CALL TABIO ('READ', IRCODE, ICLRNO, CLRECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 900
         IF (IERR.LT.0) GO TO 500
C                                       Check data
C                                       Time:
         IF ((CLRECD(TIMCL).LT.TIMBEG) .OR.
     *       (CLRECD(TIMCL).GT.TIMEND)) GO TO 500
C                                       Subarray
         IF ((CLRECI(SUBCL).NE.SUBA) .AND. (CLRECI(SUBCL).GT.0))
     *      GO TO 500
C                                       Freq id
         IF ((CLRECI(FRQCL).NE.FREQID) .AND. (CLRECI(FRQCL).GT.0) .AND.
     *      (FREQID.GT.0)) GO TO 500
         IF (NSOUWD.LE.0) GO TO 70
C                                       Check source
         THSOU = CLRECI(SOUCL)
         IF (.NOT.SLCTD (THSOU, SOUWAN, NSOUWD, DOSWNT)) GO TO 500
 70      LSTSOU = THSOU
C                                       Check antenna
         ANT = CLRECI(ANTCL)
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 500
C                                       Correct record.
         IF (OPCODE.EQ.'ANAX') THEN
            DO 100 I = 1, NANTSL
               IF (ANTENS(I).EQ.ANT) AXOFF = PARM(I)
  100          CONTINUE
            END IF
         CALL CLCCOR (2, JERR)
         IF (JERR.NE.0) GO TO 500
C                                       Rewrite record
         CALL TABIO ('WRIT', IRCODE, ICLRNO, CLRECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 900
 500     CONTINUE
C                                       Final call to CLCCOR
      CALL CLCCOR (3, JERR)
C                                       Close table.
      CALL TABIO ('CLOS', IRCODE, LOOP, CLRECR, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IERR
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TOO MANY IFS SPECIFIED, ',I3,' > 10')
 1900 FORMAT ('TABIO ERROR',I3,' CORRECTING CL TABLE')
      END
      SUBROUTINE CLCCOR (IOP, IERR)
C-----------------------------------------------------------------------
C   CLCCOR applies corrections to the CL record passed thru common
C   /CLRECC/.
C   Input:
C    IOP        I    Operation code, 1=init, 2=process, 3=finish
C   Input from common:
C    CLRECI(*)  I    The CL table record to be corrected.
C    BIF        I    First IF number
C    EIF        I    Highest IF number
C    ISTOK      I    Stokes number, 0=both, 1=first, 2=second.
C    OPCODE     C*4  Operation code.
C    ICODE      I    Operation code number, set on first call.
C    BPARM(20)  R    parameters.
C   Output in common:
C    CLRECI(*)  I    Modified record.
C   Output:
C    IERR       I    Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IOP, IERR
C
      DOUBLE PRECISION  DEG2RD
      INTEGER   NOP
C                                       CT table
C      INTEGER CTVER, BCOUNT, ECOUNT, XINC, NCOUNT
      INTEGER CTVER
C
      PARAMETER (NOP=26, DEG2RD=57.29577951D0)
C
      CHARACTER CHTM8*8, OPS(NOP)*4, STRING*8
      CHARACTER OBSDAT*8
      INTEGER   I, IANT, LSTSOU, THSOU, LUN, NTERMS, LIM1, LIM2, J,
     *   IPNT, IINC, ANTNO
      INTEGER   DAYS(12), IDATE(3), MONTHN, DAYST, NHUNDR, IROUND
      LOGICAL   LEAP
      REAL      XT, YT, XXT, YYT, FACTOR, HA, ZA, ELV, POLYN, PFAC,
     *   CFAC, SFAC, AZ, RHUNDR

      REAL      REARTH, HIONO
      DOUBLE PRECISION    FREQS, SINLAT, COSLAT, CLTIME, R8T1, R8T2, DX,
     *   DY
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA OPS /'PHAS','OPAC','ADEL','GAIN','CLOC','POLR','PANG','PONT',
     *          'IONS','ANTP','PCAL','SBDL','SSLO','RATE','PCFX','MBDL',
     *          'ANAX','ATMO','TROP','ANTC','SUND','EOPS','IONO','POGN',
     *          'DISP','DUMY'/
      DATA DAYS /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA LUN /28/
C-----------------------------------------------------------------------
C                                       Determine operation
      IF (IOP.EQ.2) GO TO 200
      IF (IOP.EQ.3) GO TO 900
C                                       Initialize - find OPCODE
      ICODE = -1
      DO 30 I = 1,NOP
         IF (OPS(I).EQ.OPCODE) ICODE = I
 30      CONTINUE
C                                       If an invalid opcode
      IF (ICODE.LE.0) THEN
         IERR = 1
C                                       Tell User
         WRITE (MSGTXT,1030) OPCODE
         GO TO 990
         END IF
C                                       History - OPCODE
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2040) OPCODE
C                                       Setup
      GO TO (50, 55, 55, 60, 70, 50, 75, 80,
     *   85, 90, 95, 100, 105, 45, 110, 100,
     *   120, 130, 130, 90, 140, 170, 130, 60,
     *   135), ICODE
C                                       'RATE' (14)
C                                       Phase rotation
C                                       Phase at "origin"
 45      PARM(1) = BPARM(1) / DEG2RD
C                                       History
         WRITE( MSGTXT, 1045) BPARM(1), BIF, EIF
         CALL MSGWRT(3)
C                                       Else 'RATE' Phase Rotation
C                                       Phase rate (deg/day -> rad/sec)
         PARM(2) = BPARM(2) / DEG2RD / 86400.0
C                                       Time of "origin"
         PARM(3) = BPARM(3) + BPARM(4)/24.0 + BPARM(5)/(24.0*60.0)
     *           + BPARM(6)/(24.0*60.0*60.0)
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2100) BPARM(1)
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2110) BPARM(2)
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2120) (BPARM(I),I=3,6)
         WRITE (MSGTXT,2100) BPARM(1)
         CALL MSGWRT(3)
         WRITE (MSGTXT,2110) BPARM(2)
         CALL MSGWRT(3)
         WRITE (MSGTXT,2120) (BPARM(I),I=3,6)
         CALL MSGWRT(3)
C                                       Source number
         PARM(12) = -10
         GO TO 999
C                                       OPCODE='PHAS' (1) or 'POLR' (6)
C                                       Phase rotation
 50      LIM1 = 1
         LIM2 = EIF - BIF + 1
         DO 52 I = LIM1,LIM2
            IPNT = (I-1) * 2 + 1
            PARM(IPNT) = COS (BPARM(I) / DEG2RD)
            PARM(IPNT+1) = SIN (BPARM(I) / DEG2RD)
C                                       History
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2050) I, BPARM(I)
C                                       Tell user
            WRITE( MSGTXT, 1050) I, BPARM(I), BIF + I - 1
            CALL MSGWRT(3)
 52         CONTINUE
C                                       Set only the 0th term for 'PHAS'
         IF (ICODE.EQ.1) THEN
            MSGTXT = 'Use OPCODE = "RATE" to Rotate Phase with Time'
            CALL MSGWRT(3)
            END IF
         GO TO 999
C                                       Atmosphere
C                                       Either opacity(2) or pressure(3)
 55      PARM(1) = BPARM(1)
C                                       Partial pressure of water (3)
         PARM(2) = BPARM(2)
C                                       Temperature
         PARM(3) = BPARM(3)
C                                       Tropospheric lapse rate
         IF (ABS (BPARM(4)).GT.1.0E-10) THEN
            PARM(4) = BPARM(4)
         ELSE
            PARM(4) = -4.0
            END IF
C                                       Height of tropospause
         IF (ABS (BPARM(5)).GT.1.0E-10) THEN
            PARM(5) = BPARM(5)
         ELSE
            PARM(5) = 15.0
            END IF
C                                       Scale height of water
         IF (ABS (BPARM(6)).GT.1.0E-10) THEN
            PARM(6) = BPARM(6)
         ELSE
            PARM(6) = 2.2
            END IF
C                                       Source number
         PARM(7) = -10
C                                       History
         NUMHIS = NUMHIS + 1
         IF (ICODE.EQ.2) WRITE (HISCRD(NUMHIS),2055) BPARM(1)
         IF (ICODE.EQ.3) THEN
            WRITE (HISCRD(NUMHIS),2056) PARM(1), PARM(2), PARM(3)
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2057) PARM(4), PARM(5), PARM(6)
            END IF
         GO TO 999
C                                       Poly. gain curve
C                                       OPCODE='GAIN' (4)
C                                       OPCODE='POGN' (24)
 60      NTERMS = 0
         DO 65 I = 1,10
            PARM(I+1) = BPARM(I)
            IF (ABS (BPARM(I)).GT.1.0E-20) NTERMS = I
 65         CONTINUE
         PARM(1) = NTERMS
C                                       Last source number
         PARM(12) = -10
C                                       History
         NUMHIS = NUMHIS + 1
         CHTM8 = 'Voltage'
         IF (ICODE.EQ.24) CHTM8 = 'Power'
         WRITE (HISCRD(NUMHIS),2065) BPARM(1), BPARM(2), CHTM8
         IF (NTERMS.GT.2) THEN
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2066) (BPARM(I),I=3,6)
            END IF
         IF (NTERMS.GT.6) THEN
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2066) (BPARM(I),I=7,10)
            END IF
         GO TO 999
C                                       OPCODE = 'CLOC' (5)
C                                       Clock rate (nsec/day -> sec/sec)
 70      PARM(1) = BPARM(1) * 1.0E-9 / 86400.0
C                                       Clock value at origin
C                                       (nsec -> sec)
         PARM(2) = BPARM(2) * 1.0E-9
C                                       Time of origin
         PARM(3) = BPARM(3) + BPARM(4)/24.0 + BPARM(5)/(24.0*60.0)
     *           + BPARM(6)/(24.0*60.0*60.0)
C                                       Correction mode
         PARM(4) = BPARM(7)
         IF (BPARM(7).EQ.0) PARM(2) = 0.0
         IF (BPARM(7).LT.0.0 .OR. BPARM(7).GT.2) THEN
            WRITE (MSGTXT,1040) BPARM(7)
            IERR = 2
            GO TO 990
            END IF
C                                       Source number
         PARM(12) = -10
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2070) BPARM(1)
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2071) BPARM(2)
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2072) BPARM(3),BPARM(4),BPARM(5),
     *      BPARM(6)
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2073) BPARM(7)
         GO TO 999
C                                       OPCODE = 'PANG' (7)
C                                       Parallactic angle (PA) corr.
C                                       PARM(1) = +/- 1.0 factor
C                                       PARM(2) = fract. last time of PA
C                                       PARM(3) = last source id.
C                                       Set PARM(1) to remove or add
C                                       P.A. corrections. NOTE:
C                                       phase of R (pol. 1) decreases
C                                       with increasing PA.
 75      PARM(1) = 1.0
         IF (BPARM(1).GT.0.0) PARM(1) = -1.0
C                                       Initialize last time , source
         PARM(2) = -1.0
         PARM(3) = -2.0
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2075) BPARM(1)
         IF (BPARM(1).GT.0.0) WRITE (HISCRD(NUMHIS),2076) BPARM(1)
         GO TO 999
C                                       OPCODE = 'PONT' (8)
C                                       Corrects antenna gain for
C                                       gross pointing error.
C                                       Temp(1) = time gain measured
C                                       Temp(2) = rate of change of gain
 80      PARM(1) = XTIME(1) + XTIME(2)/24.0 + XTIME(3)/(24.0*60.0)
     *           + XTIME(4)/(24.0*60.0*60.0)
         PARM(2) = BPARM(1)
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2077) BPARM(1)
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2078) XTIME(1),XTIME(2),XTIME(3),
     *      XTIME(4)
         GO TO 999
C                                       OPCODE = 'IONS' (9)
C                                       Ionispheric Faraday rot. corr.
C                                       PARM(1) = model type
C                                       PARM(2) = last source number
 85      PARM(2) = -10
         IF (BPARM(1).GT.0.0) THEN
C                                       Chiu model, type 1
C                                       PARM(3)=Zurich sunspot number
C                                       PARM(4)=annual time (mo)
            PARM(1) = 1.0
            PARM(3) = BPARM(3)
C                                       Get day of year number
            CALL H2CHR (8, 1, CATH(KHDOB), CHTM8)
            CALL JULDAY (CHTM8, R8T1)
            IF (CHTM8(3:3).EQ.'/') THEN
               STRING = '01/01' // CHTM8(6:8)
            ELSE
               STRING = CHTM8(:4) // '0101'
               END IF
            CALL JULDAY (STRING, R8T2)
C                                       Annual time in months since
C                                       15 Dec
            PARM(4) = ((R8T1 - R8T2) + 16.0) / 30.0
            IF (PARM(4).GT.12.0) PARM(4) = PARM(4) - 12.0
C                                       History
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2085) BPARM(1)
            GO TO 999
            END IF
C                                       No recognizible model
         IERR = 5
         MSGTXT = 'ERROR: NO RECOGNIZABLE IONISPHERIC MODEL'
         GO TO 990
C                                       OPCODE = 'ANTP' (10) ANTC (20)
C                                       Antenna and source
C                                       position error,
C                                       Position correction:
 90      PARM(1) = BPARM(1)
         PARM(2) = BPARM(2)
         PARM(3) = BPARM(3)
C
         IF ((PARM(1).NE.0) .OR. (PARM(2).NE.0) .OR.
     *      (PARM(3).NE.0)) THEN
C                                       Must select one antenna
C                                       if antenna position
C                                       is corrected
            IF (DESEL .OR. NANTSL.NE.1) THEN
               IERR = 9
               WRITE (MSGTXT,1060)
               GO TO 990
               END IF
C                                       correct AN table
            ANTNO = XANT(1)
            CALL ANTMOD (DISKIN, CNOIN, SUBA, ANTNO, PARM, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       The warning
            WRITE (MSGTXT,1075)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1065)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1066)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1067)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1075)
            CALL MSGWRT (8)
C                                       history
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2088)
            END IF
C                                       correction RA and
C                                       declination in radians
         PARM(6) = BPARM(5)/3600.0D0/DEG2RD
         PARM(7) = BPARM(6)/3600.0D0/DEG2RD
C                                       Must select one source
C                                       if source position
C                                       is corrected
         IF ((PARM(6).NE.0) .OR. (PARM(7).NE.0)) THEN
            IF (.NOT.DOSWNT .OR. NSOUWD.NE.1) THEN
               IERR = 9
               WRITE (MSGTXT,1070)
               GO TO 990
               END IF
C                                       correct SU table
C                                       corrections in degrees,
C                                       given at the picture plane
            DX = BPARM(5) / 3600.0D0
            DY = BPARM(6) / 3600.0D0
            J = ICODE / 10 - 1
            CALL SOUMOD (J, DISKIN, CNOIN, SOUWAN(1), DX, DY, IERR)
            IF (IERR.NE.0) GO TO 990
            PARM(6) = DX / DEG2RD
            PARM(7) = DY / DEG2RD
C                                       The warning
            WRITE (MSGTXT,1075)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1080)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1081)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1082)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1075)
            CALL MSGWRT (8)
C                                       history
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2089)
            END IF
C                                       Last source number
         PARM(5) = -10
C                                       variable correction of the
C                                       source position
         IF (INFILE.EQ.' ') THEN
C                                       drift of RA, DEC in rad/day
C                                       of the given mas/hr
            PARM(9) = BPARM(8) / 3.6D6 *24.0 / DEG2RD
            PARM(10) = BPARM(9) / 3.6D6 *24.0 / DEG2RD
         ELSE
C                                       read RA,DEC drift from INFILE
            CALL GETRAD (INFILE, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'Error reading INFILE'
               GO TO 990
               END IF
            END IF
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2090) BPARM(1), BPARM(2), BPARM(3)
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2092) BPARM(5), BPARM(6)
         NUMHIS = NUMHIS + 1
         IF (INFILE.EQ.' ') THEN
            WRITE (HISCRD(NUMHIS),2093) BPARM(8), BPARM(9)
         ELSE
            WRITE (HISCRD(NUMHIS),3000) INFILE
            END IF
         GO TO 999
C                                       OPCODE='PCAL' (11)
C                                       Replace complex gains
 95      LIM1 = 1
         LIM2 = EIF - BIF + 1
         IPNT = 1
         DO 97 I = LIM1,LIM2
            PARM(IPNT) = COS (BPARM(I) / DEG2RD)
            PARM(IPNT+1) = SIN (BPARM(I) / DEG2RD)
            IPNT = IPNT + 2
C                                       History
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2095) I, BPARM(I)
 97         CONTINUE
         GO TO 999
C                                       OPCODE='SBDL'(12) or 'MBDL'(16)
C                                       Correction to residual delays
C                                       or corresponded multiband phase
 100     LIM1 = 1
         LIM2 = EIF - BIF + 1
         IPNT = 1
         DO 102 I = LIM1,LIM2
            PARM(IPNT) = BPARM(IPNT) * 1.0E-9
            IPNT = IPNT + 1
C                                       History
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2099) I, BPARM(I)
 102        CONTINUE
         GO TO 999
C                                       OPCODE='SSLO' (13)
C                                       Correction for fringe stopping
C                                       using the wrong Signed Sum LOs.
C
 105     CONTINUE
         PARM(1) = BPARM(1) * 1.0E6
C                                       Last source number
         PARM(5) = -10
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2130) BPARM(1)
         GO TO 999
C                                       OPCODE='PCFX' (15)
C                                       Patch up phase cals.
 110     LIM1 = 1
         LIM2 = EIF - BIF + 1
         IPNT = 1
         DO 112 I = LIM1,LIM2
            PARM(IPNT) = BPARM(I) / DEG2RD
            IPNT = IPNT + 1
C                                       History
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2111) I, BPARM(I)
 112        CONTINUE
         GO TO 999
C                                       OPCODE='ANAX'(17) Correction
C                                       for antennas axis offset
 120     CONTINUE
         DO 125 I = 1, NANTSL
            PARM(I) = BPARM(I)
C                                       History
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2200) I, BPARM(I), ANTENS(I)
  125       CONTINUE
C                                       last source number
         PARM(NANTSL+1) = -10
         GO TO 999
C                                       OPCODE='ATMO'(18) or
C                                       OPCODE='TROP'(19) correction
C                                       of the antenna list by the
C                                       vertical atmosphere delay or
C                                       OPCODE='IONO'(23)) correction
C                                       of the antenna list by the
C                                       vertical ionosphere delay
C
C                                       given at the INFILE
 130     CONTINUE
C
         ISFAC = 1
C                                       change the sign of phase-delay
C                                       correction if OPCODE='IONO'
         IF (ICODE.EQ.23) THEN
            ISFAC = -1
            REARTH = 6378.0
            HIONO = 400.0
            RATIO = REARTH / (REARTH + HIONO)
            END IF
C                                       Source number
         PARM(1) = -10
C                                       PARM(2) = 0  =>
C                                       only atmosphere is corrected
C                                       PARM(2) = 1  =>
C                                       sum of atmosphere and clock
C                                       is corrected
         PARM(2) = BPARM(1)
         CALL GETINP (INFILE, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'Error reading INFILE'
            GO TO 990
            END IF
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),3000) INFILE
         GO TO 999
C                                       OPCODE='DISP' (25)
C                                       vertical dispersion
C                                       in INFILE
 135     CONTINUE
C                                       Source number
         PARM(1) = -10
C                                       read infile
         CALL GETINP (INFILE, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'Error reading INFILE'
            GO TO 990
            END IF
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),3000) INFILE
         GO TO 999
C
 140     CONTINUE

C                                       OPCODE='SUND'(21) correction
C                                       to the time delay caused by
C                                       the binding of the light ray
C                                       passing through  the
C                                       gravitational field of the
C                                       Sun.
C                                       initial source number
         PARM(1) = -10
C                                       initial CL time
         PARM(2) = -10
C                                       Get observation time in string
         CALL H2CHR (8, 1, CATH(KHDOB), CHTM8)
C                                       Get date
         CALL DATEST (CHTM8, IDATE)
C                                       Year
         PARM(3) = IDATE(1)
C                                       leaps year?
         LEAP = .FALSE.
         NHUNDR = IDATE(1)/100
         RHUNDR = IDATE(1) - NHUNDR*100
C                                       Julian calendar
         IF (MOD(IDATE(1),4).EQ.0) LEAP = .TRUE.
C                                       Gregorian calendar
         IF (RHUNDR.EQ.0 .AND.MOD(NHUNDR,4).NE.0) LEAP = .FALSE.
C                                       Month
         MONTHN = IDATE(2)
C
         DAYST = 0
         DO 160 I = 1, MONTHN-1
            DAYST = DAYST + DAYS(I)
C                                       leap year?
            IF (LEAP .AND. I.EQ.2) DAYST = DAYST + 1
  160       CONTINUE
C                                       Day number since the beginning
C                                       of the year
C         PARM(4) = DAYST + IDATE(3)
C                                       Month number
         PARM(4) = MONTHN
C                                       Day of month
         PARM(5) = IDATE(3)
C                                       0 => correct actual space craft
C                                       position minus ifinite position
C                                       1 => correct just actual space
C                                       craft position
         PARM(6) = BPARM(1)
C
C                                       Print out the test data?
C                                       0 => yes
C                                       1 => no
         PARM(7) = BPARM(2)
C                                       Print additional test data?
C                                       0 => no:
C                                       1 => yes
         PARM(8) = BPARM(3)
C                                       initial row in CL table
         PARM(10) = -10
C                                       get the spacecraft position
C                                       XSPACE, YSPACE, ZSPACE
C                                       at the coordinate system
C                                       origin at the Earth center
         CALL SPACES (IERR)
C                                       read the planet list from
C                                       the INFILE
         CALL PLLIST (INFILE, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'Error reading INFILE'
            GO TO 990
            END IF
         GO TO 999
C
 170     CONTINUE
C                                       OPCODE='EOPS'(22) correction
C                                       of UT1-UTC, and the Earth
C                                       pole position
         MSGTXT = '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         CALL MSGWRT (8)
         MSGTXT = '!  OPTYPE=EOPS is used for proper CT tables only !'
         CALL MSGWRT (8)
         MSGTXT = '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         CALL MSGWRT (8)
C
C                                       Use CT table
C                                       CT table version
         CTVER = 1
C                                       fix NCTROW to 5, because
C                                       John Benson told that
C                                       5 first row used always!
         NCTROW = 5
C                                       get arrays UT1C, WOBX, WOBY
C                                       from CT table
         CALL CTTAB (DISKIN, CNOIN, CTVER, IERR)

C                                       UT1C in sec, WOBX,WOBY in arcsec
         IF (IERR.NE.0) THEN
            MSGTXT = 'Bad CT table.'
            GO TO 990
            END IF
C                                       Last source number
         PARM(7) = -10
C                                       Find time of observation
         CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
C                                       Find Julian day, JD
         CALL JULDAY(OBSDAT, JD)
C                                       Read the file USNO with EOP
C                                       data
         IF (BPARM(1).EQ.0.0) BPARM(1) = 1.0
         BCOUNT = IROUND (BPARM(1))
         IF (BPARM(2).LE.0.0) BPARM(2) = 5.0
         NCOUNT = IROUND (BPARM(2))
C                                       read the USNO file with the
C                                       right eop data
         CALL USNORE (INFILE, IERR)
         IF (IERR.GT.0) GO TO 999
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),3000) INFILE
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),3010) CLVER, CLUSE
         GO TO 999
C                                       Process record
 200  FIXCNT = FIXCNT + 1
      IANT = CLRECI(ANTCL)
      GO TO (250, 300, 300, 350, 400, 280, 450, 500,
     *       550, 600, 610, 620, 630, 250, 640, 810,
     *       820, 830, 830, 600, 850, 860, 830, 350,
     *       840), ICODE
C                                       'PHAS' (1) or 'RATE' (14)
 250     IF (ISTOK.NE.2) THEN
            CLTIME = CLRECD(TIMCL)
            IPNT = 1
            IINC = 2
C                                       If correcting Phase vs time
            IF (ICODE.EQ.14) THEN
               PFAC = ((CLTIME-PARM(3))*86400.0*PARM(2)) + PARM(1)
               CFAC = COS (PFAC)
               SFAC = SIN (PFAC)
               END IF
            DO 270 I = BIF,EIF
C                                       Use precomputed phases
               IF (ICODE.EQ.1) THEN
                  CFAC = PARM(IPNT)
                  SFAC = PARM(IPNT+1)
                  END IF
               XT = CLRECR(RE1CL+I-1)
               YT = CLRECR(IM1CL+I-1)
               IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
                  CLRECR(RE1CL+I-1) = XT * CFAC - YT * SFAC
                  CLRECR(IM1CL+I-1) = XT * SFAC + YT * CFAC
                  END IF
               IPNT = IPNT + IINC
 270           CONTINUE
            END IF
         IF (ABS(ISTOK).EQ.1) GO TO 999
C                                       'PHAS' (1) or 'RATE' (14)
C                                          for 2nd polarization
C                                       'POLR' (6)  (LCP only)
 280     IPNT = 1
         IINC = 2
C                                       If correcting Phase vs time
         CLTIME = CLRECD(TIMCL)
         IF (ICODE.EQ.14) THEN
            PFAC = ((CLTIME-PARM(3))*86400.0*PARM(2)) + PARM(1)
            CFAC = COS (PFAC)
            SFAC = SIN (PFAC)
            END IF
C                                       For all IFs
         DO 290 I = BIF,EIF
C                                       Use precomputed phases
            IF ((ICODE.EQ.1) .OR. (ICODE.EQ.6)) THEN
               CFAC = PARM(IPNT)
               SFAC = PARM(IPNT+1)
               END IF
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM2CL+I-1) = XT * SFAC + YT * CFAC
               END IF
            IPNT = IPNT + IINC
 290        CONTINUE
         GO TO 999
C                                       Atmosphere
C                                       Either opacity(2) or delay(3)
 300     CALL ATMOS (IERR)
         GO TO 999
C                                       Polynomial gain curve
C                                       GAIN, POGN
 350     LSTSOU = PARM(12) + 0.5
         THSOU = CLRECI(SOUCL)
         IF (LSTSOU.NE.THSOU) THEN
C                                       Get source info
            CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
            IF (IERR.NE.0) GO TO 999
            PARM(12) = THSOU
            FREQS = FREQ + FREQO(BIF)
            SINDEC = SIN (DECAPP)
            COSDEC = COS (DECAPP)
            END IF
C                                       Compute zenith angle (deg).
         CALL SOUELV (IANT, CLRECD(TIMCL), HA, ELV, AZ)
         COSLAT = COS (STNLAT(IANT))
         SINLAT = SIN (STNLAT(IANT))
         ZA = (1.570796327 - ELV)
         ZA = 180.0 * ZA / PI
         NTERMS = PARM(1) + 0.5
C                                       Polynomial expansion.
         FACTOR = POLYN (NTERMS, ZA, PARM(2))
         IF (ICODE.EQ.24) FACTOR = SQRT (MAX (0.0, FACTOR))
         IF (ISTOK.NE.2) THEN
            DO 360 I = BIF,EIF
               XT = CLRECR(RE1CL+I-1)
               YT = CLRECR(IM1CL+I-1)
               IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
                  CLRECR(RE1CL+I-1) = XT * FACTOR
                  CLRECR(IM1CL+I-1) = YT * FACTOR
                  END IF
 360           CONTINUE
            END IF
         IF (ABS(ISTOK).NE.1) THEN
            DO 380 I = BIF,EIF
               XT = CLRECR(RE2CL+I-1)
               YT = CLRECR(IM2CL+I-1)
               IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
                  CLRECR(RE2CL+I-1) = XT * FACTOR
                  CLRECR(IM2CL+I-1) = YT * FACTOR
                  END IF
 380           CONTINUE
            END IF
         GO TO 999
C                                       Clock error
C                                       OPCODE='CLOK' (5)
 400     CALL CLOKER (IERR)
         GO TO 999
C                                       OPCODE='PANG' (7)
C                                       Parallactic angle correction
C                                       Check if source info current
 450     THSOU = CLRECI(SOUCL)
         LSTSOU = PARM(3) + 0.5
C                                       Get new source info.
         IF (LSTSOU.NE.THSOU) THEN
            CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
            IF (IERR.NE.0) GO TO 999
            PARM(3) = THSOU
            END IF
C                                       Check time.
         XT = CLRECD(TIMCL)
         I = XT
         YT = CLRECD(TIMCL) - I
C                                       0.02 sec. tolerance.
         IF (ABS(YT-PARM(2)).GT.2.315E-7) THEN
            CALL PARANG (XT, PANGLE)
            PARM(2) = YT
            END IF
C                                       Apply (remove) correction
         IANT = CLRECI(ANTCL)
         XT = COS (PARM(1) * PANGLE(IANT))
         YT = SIN (PARM(1) * PANGLE(IANT))
C                                       Polarization 1 corrections:
         IF (ISTOK.NE.2) THEN
            DO 455 I = BIF,EIF
               XXT = CLRECR(RE1CL+I-1)
               YYT = CLRECR(IM1CL+I-1)
               IF ((XXT.NE.FBLANK) .AND. (YYT.NE.FBLANK)) THEN
                  CLRECR(RE1CL+I-1) = XXT*XT - YYT*YT
                  CLRECR(IM1CL+I-1) = XXT*YT + YYT*XT
                  END IF
 455           CONTINUE
            END IF
C                                       Polarization 2 corrections:
         IF (ABS(ISTOK).NE.1) THEN
            DO 460 I = BIF,EIF
               XXT = CLRECR(RE2CL+I-1)
               YYT = CLRECR(IM2CL+I-1)
               IF ((XXT.NE.FBLANK) .AND. (YYT.NE.FBLANK)) THEN
C                                       Opposite phase for Pol. 2.
                  CLRECR(RE2CL+I-1) = XXT*XT + YYT*YT
                  CLRECR(IM2CL+I-1) = -XXT*YT + YYT*XT
                  END IF
 460           CONTINUE
            END IF
         GO TO 999
C                                       Pointing correction
C                                       'PONT' (8)
 500     CLTIME = CLRECD(TIMCL)
         IF (CLTIME.LT.PARM(1)) GO TO 999
         FACTOR = (CLTIME - PARM(1)) * 24.0 * PARM(2)
         IF (ISTOK.NE.2) THEN
            DO 510 I = BIF,EIF
               IF ((CLRECR(RE1CL+I-1).NE.FBLANK) .AND.
     *            (CLRECR(IM1CL+I-1).NE.FBLANK)) THEN
                  XT = SQRT (CLRECR(RE1CL+I-1) * CLRECR(RE1CL+I-1) +
     *               CLRECR(IM1CL+I-1) * CLRECR(IM1CL+I-1))
                  XT = XT + FACTOR
                  CLRECR(RE1CL+I-1) = CLRECR(RE1CL+I-1) / SQRT(XT)
                  CLRECR(IM1CL+I-1) = CLRECR(IM1CL+I-1) / SQRT(XT)
                  END IF
 510           CONTINUE
            END IF
         IF (ABS(ISTOK).NE.1) THEN
            DO 520 I = BIF,EIF
               IF ((CLRECR(RE2CL+I-1).NE.FBLANK) .AND.
     *            (CLRECR(IM2CL+I-1).NE.FBLANK)) THEN
                  XT = SQRT (CLRECR(RE2CL+I-1) * CLRECR(RE2CL+I-1) +
     *               CLRECR(IM2CL+I-1) * CLRECR(IM2CL+I-1))
                  XT = XT + FACTOR
                  CLRECR(RE2CL+I-1) = CLRECR(RE2CL+I-1) / SQRT(XT)
                  CLRECR(IM2CL+I-1) = CLRECR(IM2CL+I-1) / SQRT(XT)
                  END IF
 520           CONTINUE
            END IF
         GO TO 999
C                                       Ionispheric Faraday rot (9)
 550     CALL FARADA (IERR)
         GO TO 999
C                                       Antenna and source
C                                       position error (10,20)
 600     CALL ANTPOS (IERR)
         GO TO 999
C                                       OPCODE='PCAL' (11)
C                                       Replace complex gains
 610     CALL REPGAI
         GO TO 999
C                                       OPCODE='SBDL' (12)
C                                       Correction to residual delays.
 620     CALL CORSBD
         GO TO 999
C                                       OPCODE='SSLO' (13)
C                                       Correction for fringe stopping
C                                       using the wrong Signed Sum LOs.
 630     CONTINUE
         CALL CORRFQ (IERR)
         GO TO 999
C                                       OPCODE='PCFX' (15)
C                                       PAtch Phase cals.
 640     CALL PTCHPC
         GO TO 999
C                                       OPCODE='MBDL' (16)
C                                       Correction to multiband phase
 810     CALL CORMBD
         GO TO 999
C                                       OPCODE='ANAX' (17)
C                                       Correction for antenna axis
C                                       offset
 820     CALL ANAXIS (IERR)
         GO TO 999
C                                       OPCODE='ATMO' (18) or
C                                       'TROP' (19) or 'IONO' (23)
C                                       Correction of the antenna list
C                                       by the vertical atmosphere
C                                       delay given at the INFILE
C
 830     CALL ATMOV (IERR)
         GO TO 999
C                                       OPCODE='DISP' (25)
C                                       Vertical dispersion
 840     CALL DISPV (IERR)
         GO TO 999
C                                       OPCODE='SUND' (21)
C                                       correction to the time
C                                       delay caused by the binding of
C                                       the light ray passing at the
C                                       gravitational field of the Sun.
C
 850     CALL SUNDEL (IERR)
         GO TO 999
C                                       OPCODE='EOPS' (22)
C                                       correction of UT1-UTC and  the
C                                       Earth pole position
C
 860     CALL UTPOL (IERR)
         GO TO 999
C                                       Finish - number changed.
 900  NUMHIS= NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2900) FIXCNT
      WRITE (MSGTXT,2901) FIXCNT
      CALL MSGWRT (6)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('ERROR: UNKNOWN OPCODE: ',A4)
 1040 FORMAT ('ERROR: UNKNOWN CORRECTION MODE: ',F3.0)
 1045 FORMAT ('CLPARM( 1)=',F8.2,' / Phase(deg) to rotation for',
     *   ' IF(s)',I3,' to',I3)
 1050 FORMAT ('CLPARM(',I2,')=',F8.2,' / Phase(deg) to rotation for',
     *   ' IF ',I3)
 1060 FORMAT ('You have to select the one antenna corrected')
 1065 FORMAT ('!!! AN table is corrected for the selected antenna !!!')
 1066 FORMAT ('!!! So you should apply the corrected CL table     !!!')
 1067 FORMAT ('!!! to match the data. See HELP.                   !!!')
 1070 FORMAT ('You have to select the one source corrected')
 1075 FORMAT ('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
 1080 FORMAT ('!!! SU table is corrected for the selected source  !!!')
 1081 FORMAT ('!!! so you should apply the corrected CL table     !!!')
 1082 FORMAT ('!!! to match the data. See HELP.                   !!!')
 2040 FORMAT ('OPCODE = ''',A4,''' / Operation code')
 2050 FORMAT ('CLPARM(',I2,')=',F8.2,' / Phase(deg) to rotate gains')
 2055 FORMAT ('CLPARM(1)=',F8.2,' / Zenith opacity')
 2056 FORMAT ('CLPARM=',F8.2,',',F8.2,',',F8.2,
     *   ', / Atm. pres., PP H2O, Temp')
 2057 FORMAT ('      ',F8.2,',',F8.2,',',F8.2,' / Lapse, ht. ',
     *   ' tp, scl. ht. H20')
 2065 FORMAT ('CLPARM =',1PE12.5,',',E12.5,' / Gain curve ',A)
 2066 FORMAT ('      ,',1PE12.5,3(',',E12.5))
 2070 FORMAT ('CLPARM(1)=',F8.3,' / Clock drift (nanosec/day)')
 2071 FORMAT ('CLPARM(2)=',F12.3,' / Clock at "zero" time (nsec)')
 2072 FORMAT (F3.0,1X,F3.0,F3.0,F4.1,' / "Zero" time')
 2073 FORMAT ('CLPARM(7)=',F4.0,' / Delay correction mode')
 2075 FORMAT ('CLPARM(1)=',F4.0,' / Parallactic angle correction',
     *   ' removed')
 2076 FORMAT ('CLPARM(1)=',F4.0,' / Parallactic angle correction',
     *   ' APPLIED')
 2077 FORMAT ('CLPARM(2)=',F10.4,' / Rate of change of gain (/hour)')
 2078 FORMAT (F3.0,1X,F3.0,F3.0,F4.1,' / Time when antenna gain set')
 2085 FORMAT ('CLPARM(2)=',F10.2,' / Chiu model sunspot number')
 2088 FORMAT ('!!! AN table is corrected for the selected antenna !!!')
 2089 FORMAT ('!!! SU table is corrected for the selected source !!!')
 2090 FORMAT ('CLPARM =',1PE12.5,2(',',E12.5),' / Ant. pos error')
 2092 FORMAT ('CLPARM(5,6) =',2(F9.4),' / Source position error')
 2093 FORMAT ('CLPARM(8,9) =',2(F9.4),' / Source RA, DEC drift')
 2095 FORMAT ('CLPARM(',I2,')=',F8.2,' / Complex gain phase(deg)')
 2099 FORMAT ('CLPARM(',I2,')=',F8.2,' / Residual delay corr. (nsec)')
 2100 FORMAT ('CLPARM(1)=',F8.3,' / Phase at "Zero" time (degrees)')
 2110 FORMAT ('CLPARM(2)=',F10.3,' / Phase rate (deg/day)')
 2111 FORMAT ('CLPARM(',I2,')=',F8.2,' / Phase relationship(deg)')
 2120 FORMAT (F3.0,1X,F3.0,F3.0,F4.1,' / "Zero" time')
 2130 FORMAT ('CLPARM(1)=',F8.8,' / SSLO Frequency error (MHz)')
 2200 FORMAT ('CLPARM(',I2,')=',F6.2,' /Axis offset (meters) for',
     *        ' antenna', I3)
 2900 FORMAT (' / ',I6,' Records modified')
 2901 FORMAT (I6,' Records modified')
 3000 FORMAT ('INFILE=', A48)
 3010 FORMAT ('CLVER =', I3, 'CLUSE =', I3)
      END
      SUBROUTINE CLCLHI
C-----------------------------------------------------------------------
C   CLCLHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME(2)*12, LABEL*8
      INTEGER   LUN1, IERR, I, TIME(3), DATE(3), LIMIT, LIMIT2, J
      REAL      TIMBEG, TIMEND
      LOGICAL   T
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1 /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HIOPEN (LUN1, DISKIN, FCNO(NCFILE), BUFFER, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
C                                       Task message
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Sources
      IF (NSOUWD.LE.0) THEN
         WRITE (HILINE,3000) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,3001) TSKNAM
         IF (DOSWNT) WRITE (HILINE,3002) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       1st 2 and label.
         WRITE (HILINE,3003) TSKNAM, XSOUR(1), XSOUR(2)
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (NSOUWD.LE.2) GO TO 25
C                                       Rest of sources
         DO 20 I = 1,NSOUWD,2
            WRITE (HILINE,3004) TSKNAM, XSOUR(I), XSOUR(I+1)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 20         CONTINUE
         END IF
C                                       Antennas
 25   IF (NANTSL.LE.0) THEN
         WRITE (HILINE,3005) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,3006) TSKNAM
         IF (DOAWNT) WRITE (HILINE,3007) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       1st 12 and label.
         LIMIT = MIN (12, NANTSL)
         WRITE (HILINE,3008) TSKNAM, (ANTENS(J),J=1,LIMIT)
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (NANTSL.LE.12) GO TO 35
C                                       Rest of antennas
         DO 30 I = 13,NANTSL,12
            LIMIT = I
            LIMIT2 = I + 11
            LIMIT2 = MIN (NANTSL, LIMIT2)
            WRITE (HILINE,3009) TSKNAM, (ANTENS(J),J=LIMIT,LIMIT2)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 30         CONTINUE
         END IF
C                                       Timerange
 35   TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
      CALL HITIME (TIMBEG, TIMEND, LUN1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Stokes'
      WRITE (HILINE,2005) TSKNAM, XSTOK
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       IF range
      WRITE (HILINE,2004) TSKNAM, BIF, EIF
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       SUBARRAY, GAINVER, GAINUSE
      WRITE (HILINE,2002) TSKNAM, SUBA, CLVER, CLUSE
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                      Add any other history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1011) TSKNAM
         HILINE(1:8) = LABEL(1:8)
         DO 90 I = 1,NUMHIS
            HILINE(9:72) = HISCRD(I)(1:64)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 90         CONTINUE
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN1, T, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLCLHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1011 FORMAT (A6,'  ')
 2002 FORMAT (A6, ' SUBARRAY =',I3,' GAINVER = ',I4,
     *   ' GAINUSE = ',I4,' /CL table')
 2004 FORMAT (A6,' BIF =',I4,', EIF =',I4,'/ IF range')
 2005 FORMAT (A6,' STOKES = ''',A4,''' / Stokes type')
 3000 FORMAT (A6,' SOURCES = ''''     /All sources selected')
 3001 FORMAT (A6,' /Sources excluded:')
 3002 FORMAT (A6,' /Sources included:')
 3003 FORMAT (A6,' SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'          ,''',A16,''',''',A16,'''')
 3005 FORMAT (A6,' ANTENNAS = 0     /All antennas selected')
 3006 FORMAT (A6,' /Antennas excluded:')
 3007 FORMAT (A6,' /Antennas included:')
 3008 FORMAT (A6,' ANTENNAS = ',12(I3,' '))
 3009 FORMAT (A6,'            ',12(I3,' '))
      END
      SUBROUTINE ANTCOR (DISK, CNO, INVER, CATBLK, IFNO, PCOR, BUFFER,
     *   FREQID, IERR)
C-----------------------------------------------------------------------
C   ANTCOR corrects the left hand polarization solutions by a specified
C   amount.  The correction depends on the polarization solution type
C   indicated by the table header keyword 'POLTYPE'.  Also corrects the
C   source table values of Q and U.
C   Inputs:
C      DISK      I      Volume number
C      CNO       I      Catalog slot number
C      INVER     I      Input version number (subarray number)
C      CATBLK(*) I      Catalog header block
C      IFNO(2)   I      Range of IFs; 0 => 1.
C      PCOR(*)   R      Phase offsets of IFs in degrees
C      BUFFER(*) I      I/O Buffer
C      FREQID    I      FQ ID user wishes to change
C   Output:
C      IERR      I      Error code, 0=OK, >0 failed, <0 not PCAL found
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, INVER, CATBLK(256), IFNO(2), BUFFER(8),
     *   FREQID, IERR
      REAL      PCOR(*)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CHPOLT*8, CHSOL(4)*8, SOLTYP*8, CALCOD*4, SOUNAM*16,
     *   VELTYP*8, VELDEF*8
      INTEGER   IIF, LUN, IANT, INDEX, LOCS, KEYTYP, ISTYPE, BIF, EIF,
     *   NXIF, NUMREC, MSGSAV, IDSOU, SUKOLS(MAXSUC), SUNUMV(MAXSUC),
     * QUAL, NSOURC, ISURNO, SUFQID, NUMIF, IREF
      REAL      POLP1, POLP2, SPCOR, CPCOR, PD(MAXIF), FLUX(4,MAXIF)
      HOLLERITH XSOLTY(2)
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC,  LSRVEL(MAXIF), FREQO(MAXIF), LRESTF(MAXIF),
     *   RAOBS, DECOBS
      LOGICAL   ISOPEN
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHSOL /'ORI-ELP ', 'APPROX  ', 'X-Y LIN', 'VLBI'/
      DATA CHPOLT /'POLTYPE '/
      DATA LUN /28/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                      Open AN extension file.
      CALL ANTINI ('WRIT', BUFFER, DISK, CNO, INVER, CATBLK, LUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING AN FILE FOR WRITE'
         GO TO 990
         END IF
      ISOPEN = .TRUE.
C                                       Check FREQID compatibility.
      IF ((ANFQID.GT.0) .AND. (FREQID.GT.0) .AND. (ANFQID.NE.FREQID)
     *   .AND. (ANAME.NE.'ATCA') .AND. (ANAME.NE.'ATLBA') .AND.
     *   (ANAME.NE.'LBA')) THEN
         MSGTXT = 'WARNING - POTENTIALLY FATAL ERROR'
         CALL MSGWRT (7)
         MSGTXT = '   The polarization variables in your AN table have'
         CALL MSGWRT (7)
         WRITE (MSGTXT,1030) ANFQID
         CALL MSGWRT (7)
         WRITE (MSGTXT,1040) FREQID
         CALL MSGWRT (7)
         MSGTXT = '   Are you sure this is what you want to do?'
         CALL MSGWRT (7)
         END IF
      NUMREC = BUFFER(5)
C                                       IF range to modify.
C                                       This is a risky to tell the
C                                       number of IFs.
      NXIF = ANTNIF
      BIF = IFNO(1)
      IF (BIF.GT.NXIF) BIF = NXIF
      IF (BIF.LE.0) BIF = 1
      EIF = IFNO(2)
      IF (EIF.GT.NXIF) EIF = NXIF
      IF (EIF.LE.0) EIF = 1
C                                       Check solution type keyword.
      MSGSUP = 32000
      CALL TABKEY ('READ', CHPOLT, 1, BUFFER, LOCS, XSOLTY, KEYTYP,
     *   IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         IF (IERR.GT.20) THEN
            MSGTXT = 'WARNING: NO PCAL SOLUTION FOUND, ' //
     *         'SO NONE CORRECTED'
            CALL MSGWRT (7)
            GO TO 90
         ELSE
            WRITE (MSGTXT,1000) IERR, 'FINDING POL. SOLUTION TYPE'
     *         // ' KEYWORD'
            END IF
         GO TO 990
         END IF
C                                       Decide solution type:
      ISTYPE = 0
      CALL H2CHR (8, 1, XSOLTY, SOLTYP)
      IF (SOLTYP.EQ.CHSOL(1)) ISTYPE = 1
      IF (SOLTYP.EQ.CHSOL(2)) ISTYPE = 2
      IF (SOLTYP.EQ.CHSOL(3)) ISTYPE = 3
      IF (SOLTYP.EQ.CHSOL(4)) ISTYPE = 4
C                                       Unknown pol. solution type.
      IF (ISTYPE.EQ.0) THEN
         MSGTXT = 'WARNING: PCAL SOLUTION UNKNOWN, SO NOT CORRECTED'
         CALL MSGWRT (6)
         WRITE (MSGTXT,1070) SOLTYP
         CALL MSGWRT (6)
         GO TO 90
         END IF
C                                       For ISTYPE=1 (ORI-ELP) only need
C                                       to modify R-L phase differences
      IF (SOLTYP.EQ.'ORI-ELP ') THEN
C                                       Close AN table
         CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1200) IERR
            GO TO 990
            END IF
         ISOPEN = .FALSE.
C                                       Fetch old phase differences
         CALL PDRGET (DISK, CNO, INVER, LUN, CATBLK, NXIF, IREF, PD,
     *      BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Update values (radians)
         INDEX = 1
         DO 20 IIF = BIF,EIF
            PD(IIF) = PD(IIF) + PCOR(INDEX) * 1.745329E-2
            INDEX = INDEX + 1
 20         CONTINUE
C                                       Save results
         CALL PDRSET (DISK, CNO, INVER, LUN, CATBLK, NXIF, IREF, PD,
     *      BUFFER, IERR)
C                                       Read AN records
      ELSE
         DO 50 IANT = 1,NUMREC
            IANRNO = IANT
            CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR, 'READ'
               GO TO 990
               END IF
C                                       Feed polarizations
            INDEX = 1 + (BIF-1) * 2
            DO 30 IIF = BIF,EIF
C                                       Make appropriate correction
               IF ((ISTYPE.GE.2) .AND. (ISTYPE.LE.4)) THEN
C                                       Linear approximation
                  CPCOR = COS (PCOR(IIF-BIF+1) * 1.745329E-2)
                  SPCOR = SIN (PCOR(IIF-BIF+1) * 1.745329E-2)
C                                       Right hand (or X) parameters
                  POLP1 = POLCA(INDEX)
                  POLP2 = POLCA(INDEX+1)
                  POLCA(INDEX) = POLP1 * CPCOR - POLP2 * SPCOR
                  POLCA(INDEX+1) = POLP2 * CPCOR + POLP1 * SPCOR
C                                       Left hand (or Y) parameters
                  POLP1 = POLCB(INDEX)
                  POLP2 = POLCB(INDEX+1)
                  POLCB(INDEX) = POLP1 * CPCOR + POLP2 * SPCOR
                  POLCB(INDEX+1) = POLP2 * CPCOR - POLP1 * SPCOR
C                                       Nothing for now
               ELSE
                  CONTINUE
                  END IF
               INDEX = INDEX + 2
 30            CONTINUE
C                                       Write record
            IANRNO = IANT
            CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR, 'WRIT'
               GO TO 990
               END IF
 50         CONTINUE
         END IF
C                                      Close AN extension files
 90   IF (ISOPEN) THEN
         CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CLOSING AN FILE'
            GO TO 990
            END IF
         END IF
C                                       SU table
C                                       Open for READ first to set
C                                       all variables
C                                       Open SU table
      CALL SOUINI ('READ', BUFFER, DISK, CNO, 1, CATBLK, LUN, NUMIF,
     *   VELDEF, VELTYP, SUFQID, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING SU TABLE FOR READ'
         GO TO 990
         END IF
C                                       then close
      CALL TABIO ('CLOS', 0, 1, BUFFER, BUFFER, IERR)
C                                       Open for write
      CALL SOUINI ('WRIT', BUFFER, DISK, CNO, 1, CATBLK, LUN, NUMIF,
     *   VELDEF, VELTYP, SUFQID, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING SU TABLE FOR WRITE'
         GO TO 990
         END IF
C                                       Loop through SU rows modifing
C                                       the selected source position
      NSOURC = BUFFER(5)
      DO 150 IANT = 1,NSOURC
         ISURNO = IANT
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING SU TABLE'
            GO TO 990
            END IF
         DO 120 IIF = BIF,EIF
            CPCOR = COS (PCOR(IIF-BIF+1) * 1.745329E-2)
            SPCOR = SIN (PCOR(IIF-BIF+1) * 1.745329E-2)
            POLP1 = FLUX(2,IIF)
            POLP2 = FLUX(3,IIF)
            FLUX(2,IIF) = POLP1 * CPCOR - POLP2 * SPCOR
            FLUX(3,IIF) = POLP2 * CPCOR + POLP1 * SPCOR
 120        CONTINUE
         ISURNO = IANT
         CALL TABSOU ('WRIT', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITING SU TABLE'
            GO TO 990
            END IF
 150     CONTINUE
      CALL TABIO ('CLOS', 0, 1, BUFFER, BUFFER, IANT)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANTCOR: ERROR',I3,' ON ',A)
 1030 FORMAT ('   previously been modified for FQID ',I3)
 1040 FORMAT ('   You are now changing them with FREQID =',I3)
 1070 FORMAT ('ANTCOR: UNKNOWN POLN. SOLN. TYPE = ',A8)
 1100 FORMAT ('ANTCOR: ERROR',I3,1X,A4,'ING AN FILE')
 1200 FORMAT ('ANTCOR: ERROR',I3,' CLOSING AN FILE')
      END
      SUBROUTINE AXCOR (DISK, CNO, INVER, CATBLK, BUFFER, NANTS, ANS,
     *   PAR, IERR)
C-----------------------------------------------------------------------
C   AXCOR corrects the axis offsets STAXOF in accordance of input
C   CLCORPRM
C   Inputs:
C      DISK      I      Volume number
C      CNO       I      Catalog slot number
C      INVER     I      Input version number (subarray number)
C      CATBLK(*) I      Catalog header block
C      NANTS     I      Number of selected antennas
C      ANS(*)    I      Array of selected anntennas' numbers
C      PAR(*)    R      Array of input parameters
C      BUFFER(*) I      I/O Buffer
C   Output:
C      IERR      I      Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, INVER, CATBLK(256), BUFFER(8), NANTS, ANS(*),
     *   IERR
      REAL      PAR(*)
C
      INTEGER   LUN, IANT, NUMREC, I
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN /28/
C-----------------------------------------------------------------------
C                                      Open AN extension file.
      CALL ANTINI ('WRIT', BUFFER, DISK, CNO, INVER, CATBLK, LUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRIT'
         GO TO 990
         END IF
      NUMREC = BUFFER(5)
C                                       Read AN records
      DO 100 IANT = 1,NUMREC
         IANRNO = IANT
         CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'READ'
            GO TO 990
            END IF
C                                       Make appropriate correction
         DO 50 I = 1, NANTS
            IF (ANS(I).EQ.NOSTA) STAXOF = STAXOF + PAR(I)
   50       CONTINUE
C                                       Write record
         IANRNO = IANT
         CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'WRIT'
            GO TO 990
            END IF
 100     CONTINUE
C                                      Close AN extension files
      CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1200) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AXCOR: ERROR',I3,' OPEN-FOR-',A4,'ING AN FILE')
 1100 FORMAT ('AXCOR: ERROR',I3,1X,A4,'ING AN FILE')
 1200 FORMAT ('AXCOR: ERROR',I3,' CLOSING AN FILE')
      END
      SUBROUTINE ATMOS (IERR)
C-----------------------------------------------------------------------
C   Routine to determine and correct neutral atmospheric corrections.
C   Corrections are applied to the CL record in Common.
C   Control info from common:
C      ICODE    I    2 => Opacity, 3 => delay.
C      PARM(*)  R    (1) = Opacity (ICODE=2) or Atmos. press.(mbar)
C                        (ICODE=3)
C                    (2) = partial pressure of H2O (mbar).
C                    (3) = Temperature (deg C)
C                    (4) = Tropospheric lapse rate (K/km) (negative)
C                    (5) = Height of the tropopause (km).
C                    (6) = Scale height of water vapor (km)
C                    (7) = Last source ID number
C      ISTOK    I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C   Output:
C      IERR     I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, IANT, I, ITEMP
      REAL      XT, YT, FACTOR, PDLY, DPDLY, CFAC, SFAC, FQFAC, ZA, ARG,
     *   ELV, HA, AZ
      DOUBLE PRECISION HRANG, DARG, FREQS
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN /30/
C-----------------------------------------------------------------------
      LSTSOU = PARM(7) + 0.5
      THSOU = CLRECI(SOUCL)
      IF (LSTSOU.NE.THSOU) THEN
C                                       Get source info
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
         PARM(7) = THSOU
         FREQS = FREQ + FREQO(BIF)
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
         END IF
      IANT = CLRECI(ANTCL)
      CALL SOUELV (IANT, CLRECD(TIMCL), HA, ELV, AZ)
      HRANG = HA
      ZA = (1.570796327 - ELV)
      DARG = SIN (ELV)
      IF (ICODE.EQ.3) GO TO 500
C                                       Transmission factor:
C                                       Modified cosecant law from
C                                       Chopo Ma's thesis:
      ARG = PARM(1) / (DARG + (0.00143 / (TAN(ELV) + 0.0045)))
C                                       Need square root for
C                                       calibration table factor
      FACTOR = SQRT (EXP (ARG))
C
      IF (ISTOK.LE.1) THEN
         DO 100 I = BIF,EIF
            XT = CLRECR(RE1CL+I-1)
            YT = CLRECR(IM1CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XT * FACTOR
               CLRECR(IM1CL+I-1) = YT * FACTOR
               END IF
 100        CONTINUE
         END IF
      IF (ABS(ISTOK).NE.1) THEN
         DO 200 I = BIF,EIF
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * FACTOR
               CLRECR(IM2CL+I-1) = YT * FACTOR
               END IF
 200        CONTINUE
         END IF
      GO TO 999
C                                       Phase delay
C                                       Get delay and rate
 500  CALL ATMFAZ (ELV, HA, STNLAT(IANT), DECAPP, STNRAD(IANT), PARM(3),
     *   PARM(1), PARM(2), PARM(4), PARM(5), PARM(6), PDLY, DPDLY)
C                                       Atmospheric group delay
      IF (CLRECR(ATMCL).NE.FBLANK) CLRECR(ATMCL) = CLRECR(ATMCL) - PDLY
C                                       Atmospheric group delay rate
      IF (CLRECR(DATMCL).NE.FBLANK) CLRECR(DATMCL) = CLRECR(DATMCL) -
     *   DPDLY
      IF (ISTOK.NE.2) THEN
         DO 600 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE1CL+I-1)
            YT = CLRECR(IM1CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM1CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE1CL+I-1).NE.FBLANK)
     *         CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *         CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600        CONTINUE
            END IF
      IF (ABS(ISTOK).NE.1) THEN
         DO 700 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM2CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE2CL+I-1).NE.FBLANK)
     *         CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *         CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
 700        CONTINUE
         END IF
C
 999  RETURN
      END
      REAL FUNCTION POLYN (NTERMS, ARG, COEF)
C-----------------------------------------------------------------------
C   Evaluates a polynomial function.
C    Inputs:
C     NTERMS    I    Number of terms (coefficients).
C     ARG       R    Argument of polynomial expansion.
C     COEF(*)   R    Coefficients.
C-----------------------------------------------------------------------
      INTEGER   NTERMS
      REAL      ARG, COEF(*)
C
      INTEGER   LOOP
      REAL      TEMP, SUM
C-----------------------------------------------------------------------
      SUM = COEF(1)
      TEMP = 1.0
      DO 100 LOOP = 2,NTERMS
         TEMP = TEMP * ARG
         SUM = SUM + COEF(LOOP) * TEMP
 100     CONTINUE
      POLYN = SUM
C
 999  RETURN
      END
      SUBROUTINE CLOKER (IERR)
C-----------------------------------------------------------------------
C   Routine to correct effects of clock error.
C   Corrections are applied to the CL record in Common.
C   Control info from common:
C    PARM(*)  R    (1) = Rate error (sec/sec)
C                  (2) = Clock error at t0 (sec)
C                  (3) = t0 (days)
C                  (4) = correction mode
C                        0 = rate correction added
C                        1 = rate + offset correction added
C                        2 = rate and offset replace table values.
C    ISTOK    I    Polarization to correct, 1=first, 2=second, 0 = both
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, I
      REAL      XT, YT, CFAC, SFAC, FQFAC, GDELAY, DGDELY, OLDDEL
      DOUBLE PRECISION    FREQS, CLTIME
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN /30/
C-----------------------------------------------------------------------
      CONTINUE
      LSTSOU = PARM(12) + 0.5
      THSOU = CLRECI(SOUCL)
C                                       Get source info
      IF (LSTSOU.NE.THSOU) THEN
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
         PARM(12) = THSOU
         FREQS = FREQ + FREQO(BIF)
         END IF
      CLTIME = CLRECD(TIMCL)
      GDELAY = ((CLTIME - PARM(3)) * 86400.0 * PARM(1)) + PARM(2)
      DGDELY = PARM(1)
C                                       Clock
      IF (ISTOK.NE.2) THEN
         IF (CLRECR(CLK1CL).NE.FBLANK) THEN
            IF (BPARM(7).NE.2) THEN
               CLRECR(CLK1CL) = CLRECR(CLK1CL) - GDELAY
               CLRECR(DCK1CL) = CLRECR(DCK1CL) - DGDELY
            ELSE IF (BPARM(7).EQ.2) THEN
               CLRECR(CLK1CL) = -GDELAY
               CLRECR(DCK1CL) = -DGDELY
               END IF
            END IF
         DO 100 I = BIF,EIF
C         FQFAC = -TWOPI * (FREQS+FRQOFF(I)) * GDELAY
C        The sign has been changed by LK Jan 15, 2004
            FQFAC = TWOPI * (FREQS + FRQOFF(I)) * GDELAY
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE1CL+I-1)
            YT = CLRECR(IM1CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM1CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE1CL+I-1).NE.FBLANK) THEN
               OLDDEL = CLRECR(DE1CL+I-1)
               IF (BPARM(7).NE.2) THEN
                  CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + GDELAY
               ELSE IF (BPARM(7).EQ.2) THEN
                  CLRECR(DE1CL+I-1) = GDELAY
                  END IF
               END IF
C                                       Rate
            IF (CLRECR(RA1CL+I-1).NE.FBLANK) THEN
               CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DGDELY
               END IF
 100        CONTINUE
         END IF
      IF (ABS(ISTOK).NE.1) THEN
C                                       Clock error.
         IF (CLRECR(CLK2CL).NE.FBLANK) THEN
            IF (BPARM(7).NE.2) THEN
               CLRECR(CLK2CL) = CLRECR(CLK2CL) - GDELAY
               CLRECR(DCK2CL) = CLRECR(DCK2CL) - DGDELY
            ELSE IF (BPARM(7).EQ.2) THEN
               CLRECR(CLK2CL) = -GDELAY
               CLRECR(DCK2CL) = -DGDELY
               END IF
            END IF
         DO 200 I = BIF,EIF
C         FQFAC = -TWOPI * (FREQS + FRQOFF(I)) * GDELAY
C        The sign has been changed by LK Jan 15, 2004
            FQFAC = TWOPI * (FREQS + FRQOFF(I)) * GDELAY
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM2CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE2CL+I-1).NE.FBLANK) THEN
               OLDDEL = CLRECR(DE2CL+I-1)
               IF (BPARM(7).NE.2) THEN
                  CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + GDELAY
               ELSE IF (BPARM(7).EQ.2) THEN
                  CLRECR(DE2CL+I-1) = GDELAY
                  END IF
               END IF
C                                       Rate
            IF (CLRECR(RA2CL+I-1).NE.FBLANK) THEN
               CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DGDELY
               END IF
 200        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE FARADA (IERR)
C-----------------------------------------------------------------------
C   Routine to determine and correct ionospheric Faraday rotation.
C   Corrections are applied to the CL record in Common.
C   Control info from common:
C    PARM(*)  R    (1) = Electron density model type.
C                        1 = Chiu, PARM(3) = Zurich Sunspot number,
C                                  PARM(4) = annual time (months).
C                  (2) = Last source ID number
C   Output:
C      IERR   I    Return error code, 0=OK
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, IANT, I
      REAL      XT, YT, XXT, YYT, PDLY(2), DPDLY(2), ZA, ARG, HA, ELV,
     *   GDLY(2), DGDLY(2), GDLYIF, PDLYIF, DGDLYI, DPDLYI, AZ, FR,
     *   DELDT
      DOUBLE PRECISION HRANG, DARG, FREQS, SINLAT, COSLAT
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN /30/
C-----------------------------------------------------------------------
      LSTSOU = PARM(2) + 0.5
      THSOU = CLRECI(SOUCL)
      IF (LSTSOU.NE.THSOU) THEN
C                                       Get source info
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
         PARM(2) = THSOU
         FREQS = FREQ + FREQO(BIF)
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
         END IF
      IANT = CLRECI(ANTCL)
C                                       Local apparent position and
C                                       derivitive.
      CALL SOUELV (IANT, CLRECD(TIMCL), HA, ELV, AZ)
      HRANG = HA
      ZA = (1.570796327 - ELV)
      COSLAT = COS (STNLAT(IANT))
      SINLAT = SIN (STNLAT(IANT))
      DARG = SINLAT * SINDEC + COSLAT * COSDEC * COS (HRANG)
      DELDT = -(1.0 / SQRT (1.0 - DARG*DARG)) * COSLAT * COSDEC * SIN
     *   (HRANG) * TWOPI / 86400.0
C                                       Compute Faraday rotation
      CALL FAROT (CLRECD(TIMCL), STNLAT(IANT), STNLON(IANT),
     *   STNRAD(IANT), AZ, ELV, DELDT, PARM, FR, PDLY, GDLY, DPDLY,
     *   DGDLY)
C                                       Apply corrections
C                                       Polarization 1 corrections:
      IF (ISTOK.NE.2) THEN
C                                       Dispersive delay=phase delay at
C                                       1 m wavelength
         IF (CLRECR(DIS1CL).NE.FBLANK) CLRECR(DIS1CL) = CLRECR(DIS1CL) -
     *      PDLY(1) / (2.997925E8 ** 2)
         IF (CLRECR(DDS1CL).NE.FBLANK) CLRECR(DDS1CL) = CLRECR(DDS1CL) -
     *      DPDLY(1) / (2.997925E8 ** 2)
         DO 100 I = BIF,EIF
C                                       Following for RCP
            GDLYIF = GDLY(1) / ((FREQS + FRQOFF(I)) ** 2)
            PDLYIF = PDLY(1) / ((FREQS + FRQOFF(I)) ** 2)
            DGDLYI = DGDLY(1) / ((FREQS + FRQOFF(I)) ** 2)
            DPDLYI = DPDLY(1) / ((FREQS + FRQOFF(I)) ** 2)
            CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLYI
            CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + GDLYIF
C***??? SIGN???
            ARG = -FR * ((VELITE / (FREQS + FRQOFF(I))) ** 2) +
     *         PDLYIF * TWOPI * (FREQS + FRQOFF(I))
            XT = COS (ARG)
            YT = SIN (ARG)
            XXT = CLRECR(RE1CL+I-1)
            YYT = CLRECR(IM1CL+I-1)
            IF ((XXT.NE.FBLANK) .AND. (YYT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XXT*XT - YYT*YT
               CLRECR(IM1CL+I-1) = XXT*YT + YYT*XT
               END IF
 100        CONTINUE
         END IF
C                                       Polarization 2 corrections:
      IF (ABS(ISTOK).NE.1) THEN
C                                       Dispersive delay=phase delay at
C                                       1 m wavelength
         IF (CLRECR(DIS2CL).NE.FBLANK) CLRECR(DIS2CL) = CLRECR(DIS2CL) -
     *      PDLY(2) / (2.997925E8 ** 2)
         IF (CLRECR(DDS2CL).NE.FBLANK) CLRECR(DDS2CL) = CLRECR(DDS2CL) -
     *      DPDLY(2) / (2.997925E8 ** 2)
         DO 200 I = BIF,EIF
C                                       Following for LCP
            GDLYIF = GDLY(2) / ((FREQS + FRQOFF(I)) ** 2)
            PDLYIF = PDLY(2) / ((FREQS + FRQOFF(I)) ** 2)
            DGDLYI = DGDLY(2) / ((FREQS + FRQOFF(I)) ** 2)
            DPDLYI = DPDLY(2) / ((FREQS + FRQOFF(I)) ** 2)
            CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLYI
            CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + GDLYIF
C***??? SIGN???
            ARG = FR * ((VELITE / (FREQS + FRQOFF(I))) ** 2) +
     *         PDLYIF * TWOPI * (FREQS + FRQOFF(I))
            XT = COS (ARG)
            YT = SIN (ARG)
            XXT = CLRECR(RE2CL+I-1)
            YYT = CLRECR(IM2CL+I-1)
            IF ((XXT.NE.FBLANK) .AND. (YYT.NE.FBLANK)) THEN
C                                       Opposite phase for Pol. 2.
               CLRECR(RE2CL+I-1) = XXT*XT + YYT*YT
               CLRECR(IM2CL+I-1) = -XXT*YT + YYT*XT
               END IF
 200        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE FAROT (TIME, LAT, LONG, RHO, AZ, EL, DELDT, PARM,
     *   FR, PDLY, GDLY, DPDLY, DGDLY)
C-----------------------------------------------------------------------
C   Routine to compute Faraday rotation values from one of various
C   ionospheric models.  Also returns Group and phase delay and
C   derivatives.
C    Inputs:
C     TIME        D    Time in days since 0 iat on reference date
C     LAT         D    Latitude of station (rad)
C     LONG        D    Longitude of station (rad)
C     RHO         D    Distance of station from earth center (m).
C     AZ          R    Azimuth (radians)
C     EL          R    Elevation (radians)
C     DELDT       R    Time derivative of EL (rad/sec)
C     PARM(*)     R    Model dependent parameters
C                      PARM(1) = model type
C                       1.0 = Chiu model
C                             BPARM(3) = Zurich sunspot number
C                             BPARM(4) = Time of year (mo)
C    Output:
C     FR          R    Faraday rotation in rad/m**2
C     PDLY(2)     R    Phase delay correction (poln 1 and 2) in sec *
C                      frequency**2
C     GDLY(2)     R    Group delay correction (poln 1 and 2) in sec *
C                      frequency**2
C     DPDLY(2)    R    Phase delay derivitive correction (sec/sec) *
C                      frequency**2. Assumes constant ionosphere.
C     DGDLY(2)    R    Group delay derivitive correction (sec/sec) *
C                      frequency**2. Assumes constant ionosphere.
C-----------------------------------------------------------------------
      DOUBLE PRECISION    TIME, LAT, LONG, RHO
      REAL      AZ, EL, DELDT, PARM(*), FR, PDLY(2), GDLY(2), DPDLY(2),
     *   DGDLY(2)
C
      REAL      TWOPI, VELITE
      PARAMETER (TWOPI = 6.2831853)
      PARAMETER (VELITE = 2.997925E8)
      INTEGER   ITYPE
      REAL      PEAKF2, MLAT, MLONG, GLAT, GLONG, ANNTIM, HEIOLD,
     *   LOCTIM, SUNSPT, ZCD, FACT, HEIGHT, MAGFLD, ZAFACT, RAD,
     *   H(3), THICK, PIO2, DZAFDT
      EXTERNAL PEAKF2
C                                       HEIGHT = height of F2 slab in m
C                                       This is modeled in SLABF2
      DATA HEIGHT /0.0/
      DATA PIO2 /1.570796327/
C-----------------------------------------------------------------------
C                                       path length factor
C***??? can do better
      ZAFACT = 1.0 / COS (PIO2 - EL)
C                                       Time derivative
      DZAFDT = -ZAFACT * ZAFACT * SIN (PIO2 - EL) * DELDT
C                                       Initial values
      FR = 0.0
      PDLY(1) = 0.0
      PDLY(2) = 0.0
      GDLY(1) = 0.0
      GDLY(2) = 0.0
      DPDLY(1) = 0.0
      DPDLY(2) = 0.0
      DGDLY(1) = 0.0
      DGDLY(2) = 0.0
C                                       Restart here if need to iterate
C                                       height.
 10   HEIOLD = HEIGHT
C                                       Get lat, long for sub
C                                       ionispheric location.
      FACT = (PIO2 - EL - ASIN ((RHO/(RHO+HEIGHT)) *
     *   COS (EL)))
      GLAT = LAT + COS (AZ) * FACT
      GLONG = LONG + SIN (AZ) * FACT
C                                       Convert to magnetic coordinates.
      CALL MAGCRD (GLAT, GLONG, MLAT, MLONG)
C                                       Branch on model type
      ITYPE = PARM(1) + 0.1
      IF ((ITYPE.LT.1) .OR. (ITYPE.GT.1)) ITYPE = 2
      GO TO (100,750), ITYPE
C                                       Chiu model
C                                       PARM(3) = Sunspot number
C                                       PARM(4) = annual time.
 100     ANNTIM = PARM(4)
         LOCTIM = (TIME * 6.283185308D0) - GLONG
         SUNSPT = PARM(3)
C                                       Find peak height and integral
         CALL SLABF2 (MLAT, ANNTIM, LOCTIM, SUNSPT, THICK, HEIGHT)
C                                       If HEIGHT disagrees with older
C                                       value then redo.
         IF (ABS (HEIGHT-HEIOLD).GT.1.0E4) GO TO 10
C                                       Zenith column density:
C                                       Units = m**-2
         ZCD = PEAKF2 (MLAT, MLONG, ANNTIM, LOCTIM, SUNSPT) * THICK
         GO TO 800
C                                       Unknown model (shouldn't get
C                                       here).
 750     ZCD = 0.0
         GO TO 800
C                                       Magnetic field model
 800  RAD = RHO + HEIGHT
      CALL MAGDIP (GLAT, GLONG, RAD, H)
C                                       Project along line of sight.
      MAGFLD = H(3) * COS (AZ) * COS (EL) +
     *         H(2) * SIN (AZ) * COS (EL) +
     *         H(1) * SIN (EL)
C                                       Faraday rotation, from
C                                       Pacholczyk, Radio Astrophysics,
C                                       1970, p 57 (eq 2.81).
C                                       in Rad/m**2
      FR = 0.93E6 * ZCD * MAGFLD * ZAFACT * 1.0E-4 /
     *   (TWOPI * TWOPI * VELITE * VELITE)
C                                       Group and Phase delay
C                                       From: Hagfors, Methods of
C                                       Experimental Physics,
C                                       Vol 12 B, Meeks, ed
C***??? is this right???
      PDLY(1) = -40.28 * ZCD * ZAFACT / VELITE
      PDLY(2) = PDLY(1)
      DPDLY(1) = -40.28 * ZCD * DZAFDT / VELITE
      DPDLY(2) = DPDLY(1)
      GDLY(1) = 40.28 * ZCD * ZAFACT / VELITE
      GDLY(2) = GDLY(1)
      DGDLY(1) = 40.28 * ZCD * DZAFDT / VELITE
      DGDLY(2) = DPDLY(1)
C
 999  RETURN
      END
      SUBROUTINE MAGCRD (GLAT, GLONG, MLAT, MLONG)
C-----------------------------------------------------------------------
C   MAGCRD converts geographic latitude and longitude into magnetic
C   latitude and longitude. Note that geographic longitude increases to
C   the West while magnetic longitude increases to the East.
C   Inputs:   GLAT     R     geographic latitude (radians)
C             GLONG    R     geographic east-longitude (radians)
C   Outputs:  MLAT     R     magnetic latitude (radians)
C             MLONG    R     magnetic east-longitude (radians)
C   Programmer: C. Flatters, Oct. 1987
C-----------------------------------------------------------------------
      REAL   GLAT, GLONG, MLAT, MLONG
C
      REAL   PI, CMLONG, GLATMP, GLONMP, SMLONG
      PARAMETER (PI = 3.141 592 65)
C                                       Geographic coordinates of
C                                       North magnetic pole.
      PARAMETER (GLATMP = 78.63 * PI / 180)
      PARAMETER (GLONMP = 289.85 * PI / 180)
C-----------------------------------------------------------------------
      MLAT = ASIN (SIN(GLAT) * SIN(GLATMP)
     *             + COS(GLAT) * COS(GLATMP) * COS(GLONG - GLONMP))
      CMLONG = (SIN(GLATMP) * SIN(MLAT) - SIN(GLAT))
     *         / (COS(GLATMP) * COS(MLAT))
      SMLONG = SIN(GLONG - GLONMP) * COS(GLAT) / COS(MLAT)
      MLONG = ATAN2 (SMLONG, CMLONG)
  999 RETURN
C-----------------------------------------------------------------------
      END
      REAL FUNCTION PEAKF2 (MLAT, MLONG, ANNTIM, LOCTIM, SUNSPT)
C-----------------------------------------------------------------------
C   PEAKF2 returns the peak free electron density of the F2-layer in
C   electrons per cubic meter. This is derived from a phenomenological
C   model of the ionosphere (Chiu, J. At. Terr. Phys. 37, 1563; 1975).
C   Some formulae have been corrected according to the code fragment
C   IONDEM published as part of the International Reference Ionosphere
C   IRI-79 (Report UAG-82, 1981).
C   Inputs:
C      MLAT    R    magnetic latitude (radians)
C      MLONG   R    magnetic east-longitude (radians)
C      ANNTIM  R    annual time (months), beginning Dec 15th
C      LOCTIM  R    local time (radians)
C      SUNSPT  R    monthly smoothed Zurich relative sunspot
C                   number
C   Programmer: C. Flatters, Oct. 1987
C-----------------------------------------------------------------------
      REAL MLAT, MLONG, ANNTIM, LOCTIM, SUNSPT
C
      REAL   ANNFN, BETA, BIGG, DIPFN, DIURNL, EQUATR, FOLD, G, GAMMA,
     *   KAPPA, LATFN, LONGFN, LPRIME, MAGDIP, NONPLR, PI, POLAR, PSI,
     *   Q, RHO, SIGMA, SOLAR, SOLDEC, TILT, W, X, XLONG, Y, ZETA
      EXTERNAL GAMMA, PSI
      PARAMETER (PI = 3.141 592 65)
      PARAMETER (TILT = -23.5 * PI / 180)
C-----------------------------------------------------------------------
C                                     Calculate magnetic dip angle.
      MAGDIP = ATAN (2 * SIN(MLAT) / COS(MLAT))
C                                     Calculate solar declination angle
      SOLDEC = ASIN (0.39795 * SIN (PI/6 * (ANNTIM - 3.167)))
C                                     Calculate the seasonal anomoly
C                                     parameter zeta.
      ZETA = SIN(SOLDEC) * SIN(MLAT)
C                                     Normalize sunspot number.
      RHO = SUNSPT / 100
C                                     Calculate the layer peak function.
C                                     This consists of a polar function
C                                     folded with a non-polar function.
C                                     The polar function dominates at
C                                     high latitudes while the non-polar
C                                     function dominates for lower
C                                     latitudes.
C                                     First calculate the folding factor
      FOLD = EXP (-1 * ((2.4 + (0.4 + 0.1*RHO) * SIN(MLAT)) ** 6)
     *   * COS(MLAT) ** 6)
C                                     Now the polar function. This is
C                                     omitted from Chiu's paper and has
C                                     been reconstructed from a program
C                                     fragment printed in Report UAG-82
C                                     (IRI-79).
      IF (MLAT.GE.0.0) THEN
         POLAR = (2 + 1.2 * RHO) * (1 + 0.3 * SIN (PI/12 * ANNTIM))
     *      * EXP(-1.2*(COS(MLAT + TILT * COS(LOCTIM)) - COS(MLAT)))
      ELSE
         XLONG = SIN(PI/12 * ANNTIM) * (0.5 * SIN(MLONG/2)
     *      - 0.5 * SIN(MLONG) - (MLONG/2)**8) - (1 + SIN(PI/12 *
     *      ANNTIM)) * COS(PI/6 * ANNTIM) * SIN(MLONG) /
     *      SQRT(ABS(SIN(MLONG))) * EXP (-4 * SIN(MLONG/2)**2)
         POLAR = (1 + 0.4 * (1 - SIN(PI/12 * ANNTIM)**2)
     *      * EXP(-1 * COS(MLONG/2 - PI/20)**4 * SIN(PI/12 * ANNTIM)))
     *      * (2.5 + 2 * RHO + COS(PI/6 * ANNTIM) * (0.5 + (1.3 + 0.2 *
     *      RHO) * COS(MLONG/2 - PI/20)**4) + (1.3 + 0.5 * RHO) *
     *      COS(LOCTIM - PI * (1 + XLONG)))
         END IF
C                                     The non-polar function is the
C                                     product of a solar cycle function,
C                                     a diurnal function, a latitudinal
C                                     function, an annual function, an
C                                     equatorial anomoly function, a
C                                     longitudinal function and a
C                                     magnetic dip function.
C                                     First the solar cycle function.
      SIGMA = 1 + RHO + 0.204 * RHO**2 + 0.03 * RHO**3
      IF (RHO.LE.1.1) THEN
         SOLAR = SIGMA
      ELSE
         SOLAR = 2.39 + 1.53 * (SIGMA - 2.39) * SIN(MLAT)**2
         END IF
C                                     The diurnal function.
      DIURNL = (0.9 + 0.32 * ZETA)
     *      * (1 + ZETA * COS(LOCTIM - PI/4)**2)
     *      * EXP(-1.1 * (1 + COS (LOCTIM - 0.873)))
C                                     The latitudinal function.
      LPRIME = EXP(3.0 * COS(MLAT/2 * (SIN(LOCTIM) - 1)))
      Q = 1 - 0.15 * EXP(-1 * SQRT((12 * MLAT + 4*PI/3) ** 2
     *   + (ANNTIM/2 - 3) ** 2))
      LATFN = (1.2 - 0.5 * COS(MLAT)**2)
     *   * (1 + 0.05 * RHO * SIN(MLAT)**3
     *   * COS(PI/6 * ANNTIM)) * LPRIME * Q
C                                     The annual function.
      BETA = 1.3 + 0.278 * RHO ** 2 * COS(0.5 * (MLAT - PI/4)) ** 2
     *   + 0.051 * RHO ** 3
      W = EXP (-BETA * (COS(PSI(MLAT, LOCTIM, SOLDEC)) - COS(MLAT)))
      KAPPA = 1 + 0.085 * (COS(MLAT - PI/6)
     *   * COS(PI/12 * (ANNTIM - 2)) ** 3
     *   + COS (MLAT + PI/4) * COS(PI/12 * (ANNTIM - 8)) ** 2)
      X = 0.7 * (KAPPA + 0.178 * RHO**2
     *   * COS(PI/3 * (ANNTIM - 4.3)) / SOLAR) * W
      Y = 0.2 * (1 - SIN(ABS(MLAT) - PI/6))
     *   * (1 + 0.6 * COS(PI/3 * (ANNTIM - 3.94)))
     *   * COS(PI/6 * (ANNTIM - 1))
     *   + (0.13 - 0.06 * SIN(ABS(ABS(MLAT) - PI/9)))
     *   * COS(PI/3 * (ANNTIM - 4.5))
     *   - (0.15 + 0.3 * SIN(ABS(MLAT)))
     *   * (1 - COS(LOCTIM)) ** 0.25
     *   * COS(PSI(MLAT, 0.0, SOLDEC)) ** 3
      ANNFN = X + Y/SOLAR
C                                       The equatorial function.
      BIGG = (1 + 0.6 * SQRT(RHO) - 0.2 * RHO)
     *   * EXP (0.25 * (1 + COS(LOCTIM - 0.873)))
     *   * COS(MLAT)**8 * COS(ABS(MLAT) - 0.2618)**12
      EQUATR = GAMMA(0.05, 0.5, ANNTIM)
     *   * (1 + BIGG) * (1 - 0.4 * COS(MLAT) ** 10)
     *   * (1 + 0.6 * COS(MLAT)**10 * COS(ANNTIM - PI/4)**2)
C                                       The longitudinal function.
      LONGFN = 1 + 0.1 * COS(MLAT)**3
     *   * COS(2 * (MLONG - 7*PI/18))
      G = 0.15 - (1 + RHO) * SIN(MLAT/2)**2
     *   * EXP (-0.33 * (ANNTIM - 6)**2)
C                                       The dip function.
      DIPFN = GAMMA(0.03, 0.5, ANNTIM)
     *   * (1 + G * EXP (-18 * (ABS(MAGDIP) - 2*PI/9)**2))
C                                       Now everything can be put
C                                       together.
      NONPLR = SOLAR * DIURNL * LATFN * ANNFN * EQUATR * LONGFN
     *   * DIPFN
      PEAKF2 = 0.66E11 * (FOLD * POLAR + (1 - FOLD) * NONPLR)
  999 RETURN
C-----------------------------------------------------------------------
      END
      REAL FUNCTION PSI (XI, ETA, SDEC)
C-----------------------------------------------------------------------
C PSI is the seasonal anomoly parameter psi(xi,eta). Psi(lat, pi) is
C the solar zenith angle at noon.
C
C Inputs:   XI       R     dummy variable
C           ETA      R     dummy variable
C           SDEC     R     solar declination angle (radians)
C   Programmer: C. Flatters, Oct. 1987
C-----------------------------------------------------------------------
      REAL   XI, ETA, SDEC
C-----------------------------------------------------------------------
      PSI = XI + SDEC * COS (ETA)
  999 RETURN
C
      END
      SUBROUTINE SLABF2 (MLAT, ANNTIM, LOCTIM, SUNSPT, THICK, HEIGHT)
C-----------------------------------------------------------------------
C   SLABF2 returns the slab thickness of the F2-layer according to a
C   phenomenological model of the ionosphere (Chiu, J. At. Terr. Phys.
C   37, 1563; 1975) in meters and the effective height of the slab.
C   Inputs:
C    MLAT    R    magnetic latitude (radians)
C    ANNTIM  R    annual time (months), beginning Dec 15th
C    LOCTIM  R    local time (radians)
C    SUNSPT  R    monthly smoothed Zurich relative sunspot
C                 number
C   Outputs:
C    THICK   R    Effective thickness of slab (m).
C    HEIGHT  R    Effective height of the slab (m).
C   Programmer: C. Flatters, Oct. 1987
C-----------------------------------------------------------------------
      REAL      MLAT, ANNTIM, LOCTIM, SUNSPT, THICK, HEIGHT
C
      INTEGER   ALT, I, UPLIM
      REAL      STEP
C                                     Integration step (km).
      PARAMETER (STEP = 1.0)
C                                     Upper limit for integration
C                                     (units of STEP)
      PARAMETER (UPLIM = 1024)
      REAL   ALTPK, PI, PROF, R, RHO, SOLDEC, ZETA
      PARAMETER (PI = 3.141 592 65)
C-----------------------------------------------------------------------
C                                     Calculate the solar declination
C                                     angle.
      SOLDEC = ASIN (0.39795 * SIN (PI/6 * (ANNTIM - 3.167)))
C                                     Calculate the seasonal anomoly
C                                     parameter zeta.
      ZETA = SIN(SOLDEC) * SIN(MLAT)
C                                     Normalize sunspot number.
      RHO = SUNSPT / 100
C                                     The altitude of the layer peak
C                                     must be calculated before
C                                     integrating the layer profile
      ALTPK = 240 + 75 * RHO + 83 * RHO * ZETA * COS(MLAT)
     *   + COS(LOCTIM - 4.5 * ABS(MLAT) - PI)
     *   + 10 * COS(MLAT) * COS(PI/3 * (ANNTIM - 4.5))
C                                       Now integrate over the profile.
C                                       Simpson's method is used. Most
C                                       vector compilers should
C                                       vectorize this loop.
      THICK = 0.0
      DO 10 I = 1, UPLIM
         ALT = I * STEP
         IF (ALT.GE.ALTPK) THEN
            R = (ALT - ALTPK) / (40 + 0.2 * ALTPK)
         ELSE
            R = (ALT - ALTPK) / (40 + 0.2 * ALT)
            END IF
         IF ((I.EQ.1) .OR. (I.EQ.UPLIM)) THEN
            PROF = 2.0/3.0 * EXP(1 - R - EXP(-R)) * 1000
         ELSE IF (MOD(I, 2).EQ.0) THEN
            PROF = 2.0 * 2.0/3.0 * EXP(1 - R - EXP(-R)) * 1000
         ELSE
            PROF = 4.0 * 2.0/3.0 * EXP(1 - R - EXP(-R)) * 1000
            END IF
         THICK = THICK + PROF
   10 CONTINUE
C                                       Effective height
      HEIGHT = ALTPK * 1000.0
  999 RETURN
C-----------------------------------------------------------------------
      END
      REAL FUNCTION GAMMA (A, B, ANNTIM)
C-----------------------------------------------------------------------
C   GAMMA returns the gamma function used in the Chiu ionosphere model.
C    Inputs:
C          A        R       dummy parameter
C          B        R       dummy parameter
C          ANNTIM   R       annual time (months), beginning Dec 15th
C   Programmer: C. Flatters, Oct. 1987
C-----------------------------------------------------------------------
      REAL   A, B, ANNTIM
C
      REAL   PI
      PARAMETER (PI = 3.141 592 65)
C-----------------------------------------------------------------------
      GAMMA = 1 + A * (B - COS (PI/3 * ANNTIM) + COS (PI/6 * ANNTIM))
  999 RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE MAGDIP (GLAT, GLONG, RADIUS, H)
C-----------------------------------------------------------------------
C   Routine to compute the earth's magnetic field strength from an
C   offset dipole model.  Adapted from Handbook of Geophysics and Space
C   Envirnoments (circa 1965) S. L. Valley ed. Air Force Cambridge
C   Research Laboratories and Chapman and Bartels, 1940, GEOPHYSICS,
C   Oxford)
C      NOTE: The Gaussian coefficients from Chapman and Bartels give
C   a slightly better representation of the field than Valley so these
C   values are used here.
C      Values of H returned are probably good to better than 20%.
C   At the VLA the model is 6% low in total intensity and 11 deg W in
C   magnetic declination.
C    Inputs:
C     GLAT    R    Geocentric latitude (radians)
C     GLONG   R    Geocentric EAST longitude (radians)
C     RADIUS  R    Distance from the center of the earth (m)
C    Output:
C     H(3)    R    Magnetic field vector (gauss),
C                  (1) = positive away from earth center,
C                  (2) = positive east,
C                  (3) = positive north.
C-----------------------------------------------------------------------
      REAL    GLAT, GLONG, RADIUS, H(3)
C
      REAL    RE, FACT, GLATMP, GLONMP, PI,H02, L0, L1, L2, E, SQRT3,
     *   G10, G11, G20, G21, G22, H11, H21, H22, X0, Y0, Z0
      PARAMETER (PI = 3.14159265)
C                                       Geographic coordinates of
C                                       North magnetic pole.
      PARAMETER (GLATMP = 78.63 * PI / 180)
      PARAMETER (GLONMP = 289.85 * PI / 180)
C                                       Gaussian coefficients(gauss):
C                                       From Handbook of Geophysics...
C                                       Epoch 1960.
C                                       Modified??????
C      PARAMETER (G10 = -0.30509)
C      PARAMETER (G11 = -0.02181/2.0)
C      PARAMETER (G20 = -0.02196/2.0)
C      PARAMETER (G21 =  0.05145/3.0)
C      PARAMETER (G22 =  0.01448/4.0)
C      PARAMETER (H11 =  0.05841/2.0)
C      PARAMETER (H21 = -0.03443/3.0)
C      PARAMETER (H22 =  0.00172/4.0)
C                                       Chapman values Epoch 1922
      PARAMETER (G10 = -.3095)
      PARAMETER (G11 = -.0226)
      PARAMETER (G20 = -.0067)
      PARAMETER (G21 = 0.0292)
      PARAMETER (G22 = 0.0143)
      PARAMETER (H11 = 0.0592)
      PARAMETER (H21 = -.0122)
      PARAMETER (H22 = 0.0113)
C                                       SQRT3 = sqrt (3.0)
      PARAMETER (SQRT3 = 1.732050808)
C                                       Compute dipole center in units
C                                       of earth radius.
      PARAMETER (H02 = G10*G10 + G11*G11 + H11*H11)
      PARAMETER (L0  = 2.0*G10*G20 + (G11*G21 + H11*H21) * SQRT3)
      PARAMETER (L1  = -G11*G20 + (G10*G21+G11*G22+H11*H22) * SQRT3)
      PARAMETER (L2 = -H11*G20 + (G10*H21-H11*G22+G11*H22) * SQRT3)
      PARAMETER (E = (L0*G10 + L1*G11 + L2*H11) / (4.0*H02))
      PARAMETER (X0 = (L1 - G11*E) / (3.0*H02))
      PARAMETER (Y0 = (L2 - H11*E) / (3.0*H02))
      PARAMETER (Z0 = (L0 - G10*E) / (3.0*H02))
      REAL   X0M, Y0M, Z0M, HMAG, HD(3), CLA, SLA, CLO, SLO
      DOUBLE PRECISION POS0(3), POS1(3), POSTMP(3), POST2(3), RADDIP,
     *   COLAT, LONDIP, CA, SA, CB, SB
C                                       RE = Radius of earth (avg polar
C                                       and equitorial)
      DATA RE /6367650.0/
C-----------------------------------------------------------------------
C                                       Center of dipole
      X0M = X0 * RE
      Y0M = Y0 * RE
      Z0M = Z0 * RE
C                                       Convert to earth center x,y,z
C                                       Here y=> 90 e long.
      POS0(1) = RADIUS * COS (GLAT) * COS (GLONG)
      POS0(2) = RADIUS * COS (GLAT) * SIN (GLONG)
      POS0(3) = RADIUS * SIN (GLAT)
C                                       Translate
      POSTMP(1) = POS0(1) - X0M
      POSTMP(2) = POS0(2) - Y0M
      POSTMP(3) = POS0(3) - Z0M
C                                       Rotate to dipole coord.
      CA = COS (GLONMP)
      SA = SIN (GLONMP)
      CB = SIN (GLATMP)
      SB = -COS (GLATMP)
      POST2(1) = (POSTMP(1)*CA + POSTMP(2)*SA) * CB +
     *   POSTMP(3) * SB
      POST2(2) = POSTMP(2) * CA - POSTMP(1) * SA
      POST2(3) = POSTMP(3) * CB - SB * (POSTMP(1)*CA + POSTMP(2)*SA)
C                                       Polar coordinates in dipole.
      RADDIP = SQRT (POST2(1)*POST2(1) + POST2(2)*POST2(2) +
     *   POST2(3)*POST2(3))
      COLAT = ACOS (POST2(3) / RADDIP)
      LONDIP = ATAN2 (POST2(2), POST2(1))
      CLA = SIN (COLAT)
      SLA = COS (COLAT)
      CLO = COS (LONDIP)
      SLO = SIN (LONDIP)
C                                       Terms of dipole, local
      FACT = SQRT (H02) * ((RE / RADDIP) ** 3)
      H(1) = -2.0 * FACT * COS (COLAT)
      H(2) = 0.0
      H(3) = FACT * SIN (COLAT)
C                                       Rotate to dipole centered
      HD(1) = (H(1)*CLA - H(3)*SLA) * CLO - H(2) * SLO
      HD(2) = H(2) * CLO + (H(1)*CLA - H(3)*SLA) * SLO
      HD(3) = H(3) * CLA + H(1) * SLA
C                                       Modulus of HD
      HMAG = SQRT (HD(1)*HD(1) + HD(2)*HD(2) + HD(3)*HD(3))
C                                       Find position 1 km from
C                                       position in the direction of HD.
      POST2(1) = POST2(1) + 1000.0 * HD(1) / HMAG
      POST2(2) = POST2(2) + 1000.0 * HD(2) / HMAG
      POST2(3) = POST2(3) + 1000.0 * HD(3) / HMAG
C                                       Rotate new position to earth
C                                       system.
      POSTMP(1) = (POST2(1)*CB - POST2(3)*SB) * CA - POST2(2) * SA
      POSTMP(2) = POST2(2) * CA + (POST2(1)*CB - POST2(3)*SB) * SA
      POSTMP(3) = POST2(3) * CB + POST2(1) * SB
C                                       Translate to earth center
      POS1(1) = POSTMP(1) + X0M
      POS1(2) = POSTMP(2) + Y0M
      POS1(3) = POSTMP(3) + Z0M
C                                       Earth centered field
      HD(1) = (POS1(1) - POS0(1)) * 0.001 * HMAG
      HD(2) = (POS1(2) - POS0(2)) * 0.001 * HMAG
      HD(3) = (POS1(3) - POS0(3)) * 0.001 * HMAG
C                                       Earth local field
      CLA = COS (GLAT)
      SLA = SIN (GLAT)
      CLO = COS (GLONG)
      SLO = SIN (GLONG)
      H(1) = (HD(1)*CLO + HD(2)*SLO) * CLA + HD(3) * SLA
      H(2) = HD(2) * CLO - HD(1) * SLO
      H(3) = HD(3) * CLA - (HD(1)*CLO + HD(2)*SLO) * SLA
C
 999  RETURN
      END
      SUBROUTINE ANTPOS (IERR)
C-----------------------------------------------------------------------
C   Routine to correct for errors in antenna and source position.
C   Corrections are applied to the CL record in Common.
C   Control info from common:
C      PARM(*)  R    (1) = "X" correction (m)
C                    (2) = "Y" correction (m)
C                    (3) = "Z" correction (m)
C                    (4) = 1 if RH, -1 if LH coordinates
C                    (5) = Last source ID number
C                    (6) = RA correction (radians)
C                    (7) = Declination correction (radians)
C                    (8) = 0 for VLBI; > 0 for VLA
C                    (9) = Right ascension drift in rad/day
C                    (10) = declination  drift in rad/day
C      ISTOK    I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C   Output:
C      IERR     I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, I, ITEMP, IANT
      DOUBLE PRECISION XT, YT, CFAC, SFAC, FQFAC, EL, RAL, RAR, DECL,
     *   DECR, RAINT, DECINT
      DOUBLE PRECISION TCLT, TMESA, TLEFT, TRIGHT, CHAD, SHAD, HA, HAD,
     *   FREQS, CIR, X, Y, Z, DELAYC, RATEC,DDEC, DRA, CI, RADSEC,
     *   DELAY, RATE, PDLY, DPDLY, HRANG,PII, TWOPII, SIND, COSD, COEF
C                                       CI = 1/speed of light
      PARAMETER (CI = 1.0D0 / 2.997925D8)
C                                       COEF: mas => degree
      PARAMETER (COEF = 3.141592653589793D0 /180.0 / 3.6D6)
C                                       RADSEC = earth rot rate in
C                                       rad/sec.
      PARAMETER (RADSEC = 3.1415926535897932384D0 / 43200.0D0)
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      SAVE FREQS
      DATA LUN /30/
      DATA PII /3.141592653589793D0/, TWOPII /6.283185307179586D0/
C-----------------------------------------------------------------------
      LSTSOU = PARM(5) + 0.5
      THSOU = CLRECI(SOUCL)
      IF (LSTSOU.NE.THSOU) THEN
C                                       Get source info
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
         PARM(5) = THSOU
         FREQS = FREQ + FREQO(BIF)
C                                       Declination: correct back
C                                       (SU table already updated)
         DECAPP = DECAPP - PARM(7)
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
         RAAPP = RAAPP - PARM(6) / COSDEC
         END IF
C                                       Get local Hour angle
      IANT = CLRECI(ANTCL)
C                                       HA is double precision im the
C                                       SOUEL output
      CALL SOUEL (IANT, CLRECD(TIMCL), HA, EL)
C                                       Get Greenwich hour angle
C                                       for VLBI and stay local one
C                                       for VLA
      IF ((ABS(ARRAYC(1)).LT.1.D2) .AND. (ABS(ARRAYC(2)).LT.1.D2) .AND.
     *   (ABS(ARRAYC(3)).LT.1.D2)) THEN
         HRANG = HA - STNLON(IANT)
         HRANG = DMOD (HRANG, TWOPII)
         IF (HRANG.GT. PII) HRANG = HRANG - TWOPII
         IF (HRANG.LT.-PII) HRANG = HRANG + TWOPII
         HA = HRANG
         END IF
      HAD = HA
      CHAD = COS (HAD)
      SHAD = SIN (HAD)
      IF ((REFANT.LE.0) .OR. (STNX(REFANT).EQ.0.0D0)) THEN
         IF (RF1CL.GT.0) REFANT = CLRECI(RF1CL)
         IF ((REFANT.LE.0) .AND. (RF2CL.GT.0)) REFANT = CLRECI(RF2CL)
         END IF
      IF ((REFANT.LE.0) .OR. (STNX(REFANT).EQ.0.0D0)) THEN
         DO 10 I = 1,MAXANT
            IF ((STNX(I).NE.0.0D0) .AND. (STNY(I).NE.0.0D0) .AND.
     *         (STNZ(I).NE.0.0D0)) THEN
               REFANT = I
               GO TO 20
               END IF
 10         CONTINUE
         REFANT = 1
         END IF
C                                       Antenna coordinates:
C                                       correct for handedness of
C                                       coordinates.
 20   X = STNX(IANT) - STNX(REFANT)
      Y = STNY(IANT) - STNY(REFANT)
      Z = STNZ(IANT) - STNZ(REFANT)
      CIR = CI * RADSEC
C                                       Delay and rate in sec and
C                                       sec/sec. (want corrections).
C                                       The formulae are written
C                                       at the Right Hand coordinate
C                                       system.
C                                       not corrected delay and rate
      DELAY = CI * ((X * CHAD - Y * SHAD) * COSDEC + Z * SINDEC)
      RATE = CIR * (-X * SHAD - Y * CHAD) * COSDEC
      IF (ANAME.EQ.'GMRT') THEN
         DELAY = -DELAY
         RATE = -RATE
         END IF
C                                       correcte antenna coordinates
      X = X + PARM(1)
      Y = Y + PARM(2)
      Z = Z + PARM(3)
C                                       time at the CL table line,
C                                       in days
      TCLT = CLRECD(TIMCL)
C                                       start of variable source
C                                       position correction
      IF (INFILE.EQ.' ') THEN
C                                       TCLT is time relatively OBSTIM
C                                       given at the IMH
C
C                                       PARM(9) is source drift at RA
C                                       No COSDEC!
C                                       PARM(10) is source drift at DEC
         RAINT =  PARM(9) * TCLT
         DECINT = PARM(10) * TCLT
      ELSE
C                                       Interpolate the variable RA,DEC
C                                       correction to the TCLT using
C                                       the INFILE data
         TLEFT = 0.0D0
         TRIGHT = 100.0D0
         DO 100 I = 1, NLINES
C                                       find the nearest point at left
C                                       and right side of TCLT
            TMESA = TMES(I)
            IF ((TMESA.GT.TCLT) .AND. (TMESA.LT.TRIGHT)) THEN
               TRIGHT = TMESA
               RAR = RADR(I)
               DECR = DECDR(I)
               END IF
            IF ((TMESA.LT.TCLT) .AND. (TMESA.GT.TLEFT)) THEN
               TLEFT = TMESA
               RAL = RADR(I)
               DECL= DECDR(I)
               END IF
  100       CONTINUE
            IF (TRIGHT.EQ.100.0D0) THEN
               IF (TLEFT.EQ.0.0D0) THEN
                  RAINT = 0
                  DECINT = 0
               ELSE
                  RAINT = RAL
                  DECINT = DECL
                  END IF
            ELSE
               IF (TLEFT.EQ.0.0D0) THEN
                  RAINT = RAR
                  DECINT = DECR
               ELSE
C                                       make the interpolation itself
                  RAINT = RAL + (RAR-RAL) * (TCLT-TLEFT)/(TRIGHT-TLEFT)
                  DECINT =
     *               DECL+ (DECR-DECL) * (TCLT-TLEFT)/(TRIGHT-TLEFT)
                  END IF
               END IF
C                                       convert RAINT, DECINT from mas
C                                       to radians
         RAINT = COEF * RAINT
C                                       NO COSDEC in RAINT!!
         DECINT = COEF * DECINT
         END IF
C                                       end of variable source position
C                                       correction
C
C                                       Source position error in RA
C                                       direction at the picture plane
      DRA = 0.0
      IF (COSDEC.NE.0.0) DRA = PARM(6) / COSDEC
C                                       Source position error in
C                                       declination direction
      DDEC = PARM(7)
C                                       add the variable part of RA
C                                       correction
      DRA = DRA + RAINT
      DDEC = DDEC + DECINT
C                                       correct hour angle
      CHAD = COS (HAD - DRA)
      SHAD = SIN (HAD - DRA)
C                                       correct declination
      SIND = SIN (DECAPP + DDEC)
      COSD = COS (DECAPP + DDEC)
C                                       corrected delay and rate
      DELAYC = CI * ((X * CHAD - Y * SHAD) * COSD + Z * SIND)
      RATEC = CIR * (-X * SHAD - Y * CHAD) * COSD
      IF (ANAME.EQ.'GMRT') THEN
         DELAYC = -DELAYC
         RATEC = -RATEC
         END IF
C                                       correction of delay and rate
      PDLY = DELAYC - DELAY
      DPDLY = RATEC - RATE
C                                       exclude correct geom. delay
C                                       LK Sep 8,2005
C                                       Correct CL record
C                                       Geometric delay (2 terms)
C      IF (CLRECD(GDLCL).NE.DBLANK) CLRECD(GDLCL) = CLRECD(GDLCL) - PDLY
C                                       Second term
C      IF ((CLNUMV(CLDDEL).GE.2) .AND. (CLRECD(GDLCL+1).NE.DBLANK))
C     *   CLRECD(GDLCL+1) = CLRECD(GDLCL+1) - DPDLY
C
      IF (ISTOK.NE.2) THEN
         IF (CLRECR(MBD1CL).NE.FBLANK) CLRECR(MBD1CL) = CLRECR(MBD1CL)
     *      + PDLY
         DO 600 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE1CL+I-1)
            YT = CLRECR(IM1CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM1CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE1CL+I-1).NE.FBLANK)
     *         CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *         CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600        CONTINUE
         END IF
      IF (ABS(ISTOK).NE.1) THEN
         IF (CLRECR(MBD2CL).NE.FBLANK) CLRECR(MBD2CL) = CLRECR(MBD2CL)
     *      + PDLY
         DO 700 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM2CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE2CL+I-1).NE.FBLANK)
     *         CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *         CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
 700        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE REPGAI
C-----------------------------------------------------------------------
C   Routine to replace the complex gains with unit vectors of user
C   specified phase.  New values are written into the CL record in
C   Common.
C   Control info from common:
C      PARM(*)  R    (1,2) real,imaginary part of IF=BIF
C                    (3,4) real,imaginary part of IF=BIF+1 etc.
C      ISTOK    I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C-----------------------------------------------------------------------
C
      INTEGER   I, IP
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       First polarization
      IF (ISTOK.NE.2) THEN
         IP = 1
         DO 100 I = BIF,EIF
            CLRECR(RE1CL+I-1) = PARM(IP)
            CLRECR(IM1CL+I-1) = PARM(IP+1)
            IP = IP + 2
 100        CONTINUE
         END IF
C                                       Second polarization
      IF (ABS(ISTOK).NE.1) THEN
         IP = 1
         DO 200 I = BIF,EIF
            CLRECR(RE2CL+I-1) = PARM(IP)
            CLRECR(IM2CL+I-1) = PARM(IP+1)
            IP = IP + 2
 200        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CORSBD
C-----------------------------------------------------------------------
C   Routine to make an additive correction to the IF delay residuals in
C   the CL record in common.
C   Control info from common:
C      PARM(*)  R    The corrections, 1 per IF from BIF to EIF.
C      ISTOK    I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C      FRQOFF   D(*) IF frequency offset table (Hz)
C-----------------------------------------------------------------------
C
      INTEGER   I, IP
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       First polarization
      IF (ISTOK.NE.2) THEN
         IP = 1
         DO 100 I = BIF,EIF
            IF (CLRECR(DE1CL+I-1).NE.FBLANK) CLRECR(DE1CL+I-1) =
     *         CLRECR(DE1CL+I-1) + PARM(IP)
            IP = IP + 1
 100        CONTINUE
         END IF
C                                       Second polarization
      IF (ABS(ISTOK).NE.1) THEN
         IP = 1
         DO 200 I = BIF,EIF
            IF (CLRECR(DE2CL+I-1).NE.FBLANK) CLRECR(DE2CL+I-1) =
     *         CLRECR(DE2CL+I-1) + PARM(IP)
            IP = IP + 1
 200        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CORMBD
C-----------------------------------------------------------------------
C   Routine to make an correction to the IF phase corresponded
C   to a given multiband delay
C   Control info from common:
C      PARM(*)  R    The corrections, 1 per IF from BIF to EIF.
C      ISTOK    I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C      FRQOFF   D(*) IF frequency offset table (Hz)
C-----------------------------------------------------------------------
C
      INCLUDE 'CLCOR.INC'
      INTEGER   I, IP
      REAL      PCOR, CPCOR(MAXIF), SPCOR(MAXIF), GR, GI
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Phase corrections
      IP = 1
      DO 50 I = BIF,EIF
         PCOR = -6.283185308 * FRQOFF(I) * PARM(IP)
         CPCOR(I) = COS (PCOR)
         SPCOR(I) = SIN (PCOR)
         IP = IP + 1
 50      CONTINUE
C                                       First polarization
      IF (ISTOK.NE.2) THEN
         IP = 1
         IF (CLRECR(MBD1CL).NE.FBLANK) CLRECR(MBD1CL) =
     *         CLRECR(MBD1CL) + PARM(IP)
         DO 100 I = BIF,EIF
C                                       Correct phase at each IF
            GR = CLRECR(RE1CL+I-1) * CPCOR(I) -
     *           CLRECR(IM1CL+I-1) * SPCOR(I)
            GI = CLRECR(RE1CL+I-1) * SPCOR(I) +
     *           CLRECR(IM1CL+I-1) * CPCOR(I)
            CLRECR(RE1CL+I-1) = GR
            CLRECR(IM1CL+I-1) = GI
            IP = IP + 1
 100        CONTINUE
         END IF
C                                       Second polarization
      IF (ABS(ISTOK).NE.1) THEN
         IP = 1
         IF (CLRECR(MBD2CL).NE.FBLANK) CLRECR(MBD2CL) =
     *         CLRECR(MBD2CL) + PARM(IP)
         DO 200 I = BIF,EIF
C                                       Correct phase at each IF
            GR = CLRECR(RE2CL+I-1) * CPCOR(I) -
     *           CLRECR(IM2CL+I-1) * SPCOR(I)
            GI = CLRECR(RE2CL+I-1) * SPCOR(I) +
     *           CLRECR(IM2CL+I-1) * CPCOR(I)
            CLRECR(RE2CL+I-1) = GR
            CLRECR(IM2CL+I-1) = GI
            IP = IP + 1
 200        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CORRFQ (IERR)
C-----------------------------------------------------------------------
C   Routine to correct for a phase error caused by an error in the
C   Signed Sum of the LOs.
C   Corrections are applied to the CL record in Common.
C   Control info from common:
C      PARM(*)  R    (1) = Frequency error
C                    (4) = 1 if RH, -1 if LH coordinates
C                    (5) = Last source ID number
C      ISTOK    I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C   Output:
C      IERR     I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, I, ITEMP, IANT
      REAL      XT, YT, CFAC, SFAC, FQFAC, HA, EL, COSHA, SINHA, AZ
      DOUBLE PRECISION FREQS, D(3), S(3), CI, RADSEC, PDLY
C                                       CI = 1/speed of light
      PARAMETER (CI = 1.0D0 / 2.997925D8)
C                                       RADSEC = earth rot rate in
C                                       rad/sec.
      PARAMETER (RADSEC = 3.1415926535897932384D0 / 43200.0D0)
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C
      SAVE LSTSOU, THSOU, FREQS
      DATA LUN /30/
C-----------------------------------------------------------------------
      LSTSOU = PARM(5) + 0.5
      THSOU = CLRECI(SOUCL)
      IF (LSTSOU.NE.THSOU) THEN
C                                       Get source info
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
         PARM(5) = THSOU
         FREQS = FREQ + FREQO(BIF)
C                                       Declination
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
         END IF
C                                       Get Hour angle
      IANT = CLRECI(ANTCL)
      CALL SOUELV (IANT, CLRECD(TIMCL), HA, EL, AZ)
      COSHA = COS (HA)
      SINHA = SIN (HA)
C                                       Antenna coordinates:
C                                       correct for handedness of
C                                       coordinates.
      D(1) = STNX(IANT)
      D(2) = STNY(IANT)
      D(3) = STNZ(IANT)
C                                       Source position
      S(1) =  COSDEC * COSHA
      S(2) = -COSDEC * SINHA
      S(3) =  SINDEC
C                                       Delay in sec.
      PDLY = CI * (S(1)*D(1) + S(2)*D(2) + S(3)*D(3))
C                                       Correct CL record
      IF (ISTOK.NE.2) THEN
         DO 600 I = BIF,EIF
            FQFAC = PARM(1) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE1CL+I-1)
            YT = CLRECR(IM1CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM1CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delays and derivatives were
C                                       done correctly.
 600        CONTINUE
         END IF
C                                       2nd polarization
      IF (ABS(ISTOK).NE.1) THEN
         DO 700 I = BIF,EIF
            FQFAC = PARM(1) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM2CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delays and derivatives were
C                                       done correctly.
 700        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE PTCHPC
C-----------------------------------------------------------------------
C   Routine to patch phase cal values.
C   The expected relationship between the phase in the different IF is
C   given in PARM.  Any valid phases are used to estimate the blanked
C   phases.  If there are no blanked phases then the values in PARM are
C   used.   New values are written into the CL record in
C   Common.
C   Control info from common:
C      PARM(*)  R    Expected phase relationship of IF BIF-EIF (rad).
C      ISTOK    I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C-----------------------------------------------------------------------
C
      INCLUDE 'CLCOR.INC'
      INTEGER   I, J, IP, COUNT
      REAL      FAZ(MAXIF), CSUM, SSUM
      LOGICAL   SOMGOD, ALGOOD, GOOD(MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       First polarization
      IF (ISTOK.NE.2) THEN
C                                       Get existing phases
         SOMGOD = .FALSE.
         ALGOOD = .TRUE.
         IP = 1
         DO 20 I = BIF,EIF
            IF ((CLRECR(RE1CL+I-1).NE.FBLANK) .AND.
     *         (CLRECR(IM1CL+I-1).NE.FBLANK)) THEN
               FAZ(IP) = ATAN2 (CLRECR(IM1CL+I-1),
     *            CLRECR(RE1CL+I-1)+1.0E-20)
               SOMGOD = .TRUE.
               GOOD(IP) = .TRUE.
            ELSE
               FAZ(IP) = 0.0
               GOOD(IP) = .FALSE.
               ALGOOD = .FALSE.
               END IF
            IP = IP + 1
 20         CONTINUE
C                                       If all valid skip
         IF (ALGOOD) GO TO 500
C                                       If any good values use them to
C                                       estimate the others.
         IP = 1
         IF (SOMGOD) THEN
            DO 60 I = BIF,EIF
               IF (.NOT.GOOD(IP)) THEN
C                                       Average good phases
                  CSUM = 0.0
                  SSUM = 0.0
                  COUNT = 0
                  DO 40 J = BIF,EIF
                     IF (GOOD(J-BIF+1)) THEN
                        COUNT = COUNT + 1
                        CSUM =  CSUM + COS (FAZ(J-BIF+1) - PARM(J-BIF+1)
     *                     + PARM(IP))
                        SSUM =  SSUM + SIN (FAZ(J-BIF+1) - PARM(J-BIF+1)
     *                     + PARM(IP))
                        END IF
 40                  CONTINUE
                  IF (COUNT.GT.0) THEN
                     FAZ(IP) = ATAN2 (SSUM/COUNT, (CSUM/COUNT)+1.0E-20)
                  ELSE
                     FAZ(IP) = PARM(IP)
                     END IF
                  END IF
               IP = IP + 1
 60            CONTINUE
         ELSE
C                                       All blanked - use PARM phases
            DO 80 I = BIF,EIF
               FAZ(IP) = PARM(IP)
               IP = IP + 1
 80            CONTINUE
            END IF
C                                       Replace blanked values
         IP = 1
         DO 100 I = BIF,EIF
            IF (.NOT.GOOD(IP)) THEN
               CLRECR(RE1CL+I-1) = COS (FAZ(IP))
               CLRECR(IM1CL+I-1) = SIN (FAZ(IP))
               END IF
            IP = IP + 1
 100        CONTINUE
         END IF
C                                       Second polarization
 500  IF (ABS(ISTOK).NE.1) THEN
C                                       Get existing phases
         SOMGOD = .FALSE.
         ALGOOD = .TRUE.
         IP = 1
         DO 520 I = BIF,EIF
            IF ((CLRECR(RE2CL+I-1).NE.FBLANK) .AND.
     *         (CLRECR(IM2CL+I-1).NE.FBLANK)) THEN
               FAZ(IP) = ATAN2 (CLRECR(IM2CL+I-1),
     *            CLRECR(RE2CL+I-1)+1.0E-20)
               SOMGOD = .TRUE.
               GOOD(IP) = .TRUE.
            ELSE
               FAZ(IP) = 0.0
               GOOD(IP) = .FALSE.
               ALGOOD = .FALSE.
               END IF
            IP = IP + 1
 520        CONTINUE
C                                       If all valid skip
         IF (ALGOOD) GO TO 999
C                                       If any good values use them to
C                                       estimate the others.
         IP = 1
         IF (SOMGOD) THEN
            DO 560 I = BIF,EIF
               IF (.NOT.GOOD(IP)) THEN
C                                       Average good phases
                  CSUM = 0.0
                  SSUM = 0.0
                  COUNT = 0
                  DO 540 J = BIF,EIF
                     IF (GOOD(J-BIF+1)) THEN
                        COUNT = COUNT + 1
                        CSUM =  CSUM + COS (FAZ(J-BIF+1) - PARM(J-BIF+1)
     *                     + PARM(IP))
                        SSUM =  SSUM + SIN (FAZ(J-BIF+1) - PARM(J-BIF+1)
     *                     + PARM(IP))
                        END IF
 540                 CONTINUE
                  IF (COUNT.GT.0) THEN
                     FAZ(IP) = ATAN2 (SSUM/COUNT, (CSUM/COUNT)+1.0E-20)
                  ELSE
                     FAZ(IP) = PARM(IP)
                     END IF
                  END IF
               IP = IP + 1
 560           CONTINUE
         ELSE
C                                       All blanked - use PARM phases
            DO 580 I = BIF,EIF
               FAZ(IP) = PARM(IP)
               IP = IP + 1
 580           CONTINUE
            END IF
C                                       Replace blanked values
         IP = 1
         DO 600 I = BIF,EIF
            IF (.NOT.GOOD(IP)) THEN
               CLRECR(RE2CL+I-1) = COS (FAZ(IP))
               CLRECR(IM2CL+I-1) = SIN (FAZ(IP))
               END IF
            IP = IP + 1
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE ANAXIS (IERR)
C-----------------------------------------------------------------------
C   Routine to correct for errors in antenna axis offset
C   Corrections are applied to the CL record in Common.
C   Control info from common:
C      PARM(1)  R    (1) = axis offset (m)
C   Output:
C      IERR     I    Return error code , 0=OK else failed.
C   All IFs and all polarizations are corrected
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, I, ITEMP, IANT, MOUNT
      REAL      XT, YT, CFAC, SFAC, FQFAC, HA, EL, CHA, SHA, AZ
      DOUBLE PRECISION FREQS, LAT, SL, CL, BRACK, SQR, CI, RADSEC,
     *           PDLY, DPDLY
C                                       CI = 1/speed of light
      PARAMETER (CI = 1.0D0 / 2.997925D8)
C                                       RADSEC = earth rot rate in
C                                       rad/sec.
      PARAMETER (RADSEC = 3.1415926535897932384D0 / 43082.0D0)
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      SAVE  FREQS
      DATA LUN /30/
C-----------------------------------------------------------------------
      LSTSOU = PARM(NANTSL + 1) + 0.5
      THSOU = CLRECI(SOUCL)
      IF (LSTSOU.NE.THSOU) THEN
C                                       Get source info
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
         PARM(NANTSL + 1) = THSOU
         FREQS = FREQ + FREQO(BIF)
C                                       Declination
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
         END IF
      IANT = CLRECI(ANTCL)
C                                       Antenna mount
      MOUNT = MNTYP(IANT)
C                                       Altaz or Nasmyth mount
C                       (folded cassegrain is a varient of Nasmyth)
      IF (MOUNT.EQ.0) THEN
C                                       Get Hour angle
         CALL SOUELV (IANT, CLRECD(TIMCL), HA, EL, AZ)
         CHA = COS (HA)
         SHA = SIN (HA)
C                                       Antenna latitude
         LAT = STNLAT(IANT)
         SL = DSIN(LAT)
         CL = DCOS(LAT)
         BRACK = SL*SINDEC + CL*COSDEC*CHA
         SQR = DSQRT(1.D0 - BRACK*BRACK)
C                                       Delay and rate in sec and
C                                       sec/sec. (want corrections).
         PDLY = (AXOFF*CI) * SQR
         DPDLY = ((AXOFF*CI)*BRACK/SQR) * CL * COSDEC * SHA * RADSEC
C                                       XY-EW mount
      ELSE IF (MOUNT.EQ.3) THEN
C                                       Get Hour angle
         CALL SOUELV (IANT, CLRECD(TIMCL), HA, EL, AZ)
         CHA = COS (HA)
         SHA = SIN (HA)
C                                       Antenna latitude
         LAT = STNLAT(IANT)
         SL = DSIN(LAT)
         CL = DCOS(LAT)
         BRACK = SHA*COSDEC
         SQR = DSQRT(1.D0 - BRACK*BRACK)
C                                       Delay and rate in sec and
C                                       sec/sec. (want corrections).
         PDLY = (AXOFF*CI) * SQR
         DPDLY = ((AXOFF*CI)*BRACK/SQR) * CL * COSDEC * SHA * RADSEC

C                                       Equatorial mount
      ELSE IF (MOUNT.EQ.1) THEN
         PDLY = AXOFF*CI*COSDEC
         DPDLY = 0.0
         END IF
C                                       exclude correction of geom.
C                                       delay
C                                       LK Sep 8, 2005
C                                       Correct CL record
C                                       Geometric delay (2 terms)
C      IF (CLRECD(GDLCL).NE.DBLANK) CLRECD(GDLCL) = CLRECD(GDLCL) - PDLY
C                                       Second term
C      IF ((CLNUMV(CLDDEL).GE.2) .AND. (CLRECD(GDLCL+1).NE.DBLANK))
C     *   CLRECD(GDLCL+1) = CLRECD(GDLCL+1) - DPDLY
C                                       Correct the first polarization
      IF (ISTOK.NE.2) THEN
         DO 600 I = 1,NUMIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE1CL+I-1)
            YT = CLRECR(IM1CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM1CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE1CL+I-1).NE.FBLANK)
     *         CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *         CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600        CONTINUE
         END IF
C                                       Correct the second polarization
C                                       if it exists
      IF (ABS(ISTOK).NE.1) THEN
         DO 700 I = 1, NUMIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM2CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE2CL+I-1).NE.FBLANK)
     *         CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *         CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
  700       CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE SOUEL (ANTNO, TIME, HA, EL)
C-----------------------------------------------------------------------
C   Subroutine to compute the apparent source elevations based on source
C   and antenna coordinates in common.  The routines GETANT and GETSOU
C   should be called before this routine to but the correct values in
C   the relevant commons.
C   Inputs:
C      ANTNO      I    Antenna number
C      TIME       D    Current data time (days).
C   Input from common:
C      RAAPP      D    Apparent RA of source
C      DECAPP     D    Apparent Declination of source.
C      STNLAT     D(*) Antenna latitude (rad).
C      STNLON     D(*) Antenna east longitudes (rad).
C      GSTIAT     D    GST at IAT=0 of reference day (rad).
C      ROTIAT     D    Rotation of the earth rate in IAT.
C   Output:
C      HA         D    Source hour angle (rad) wrt to telescope
C      EL         R    Source elevation (rad) at telescope
C-----------------------------------------------------------------------
      INTEGER   ANTNO
      DOUBLE PRECISION TIME
      DOUBLE PRECISION    EL
      DOUBLE PRECISION    HRANG, ANTLST, DARG, DRA, DDEC, PI, TWOPI, HA
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA PI /3.141592653589793/, TWOPI/6.283185307179586/
C-----------------------------------------------------------------------
C                                       Antenna LST
      ANTLST = GSTIAT + STNLON(ANTNO) + TIME * ROTIAT
C                                       Source position; if apparent
C                                       position missing use mean
C                                       position.
      DRA = RAAPP
      DDEC = DECAPP
      IF ((DRA.LE.1.0D-5) .AND. (DDEC.LE.1.0D-5)) THEN
         DRA = RAEPO
         DDEC = DECEPO
         END IF
C                                       Hour angle
      HRANG = ANTLST - DRA
C                                       Limit to between 0 and 2pi
      HRANG = DMOD (HRANG, TWOPI)
C                                       translate to between -pi and pi
      IF (HRANG.GT. PI) HRANG = HRANG - TWOPI
      IF (HRANG.LT.-PI) HRANG = HRANG + TWOPI
      HA = HRANG
C                                       Elevation angle
      DARG = SIN (STNLAT(ANTNO)) * SIN (DDEC) + COS (STNLAT(ANTNO))
     *   * COS (DDEC) * COS (HRANG)
      EL = (1.570796327 - ACOS (DARG))
C
 999  RETURN
      END
      SUBROUTINE ANTMOD (DISK, CNO, SUBA, ANTNO, PARM, IERR)
C-----------------------------------------------------------------------
C   Subroutine to make the correction of the selected antenna position
C   if OPCODE ='ANTP' and the antenna correction parameters are not
C   equal zero.
C   Inputs:
C      DISK       I    The file disk number
C      CNO        I    The file catalog slot number.
C      ANTNO      I    The antenna number
C      SUBA       I    Subarray number (AN table number)
C      PARM(*)    R    Array of corrections
C   Input from common:
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, SUBA, ANTNO, IERR
      REAL      PARM(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, LUN1, BUFFAN(512), NENTRY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA LUN1 /28/
C-----------------------------------------------------------------------
C                                       Open for write: existing file
C                                       so parameters need not be set
      CALL ANTINI ('WRIT', BUFFAN, DISK, CNO, SUBA, CATBLK, LUN1,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Loop through AN rows correcting
C                                       the selected antenna position
      NENTRY = BUFFAN(5)
      DO 300 I = 1, NENTRY
         IANRNO = I
         CALL TABAN ('READ', BUFFAN, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1200) IERR, I
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Put the correction here
         IF (NOSTA.EQ.ANTNO) THEN
            STAXYZ(1) = STAXYZ(1) + PARM(1)
            STAXYZ(2) = STAXYZ(2) + PARM(2)
            STAXYZ(3) = STAXYZ(3) + PARM(3)
            END IF
         IANRNO = I
         CALL TABAN ('WRIT', BUFFAN, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1300) IERR, I
            CALL MSGWRT (8)
            GO TO 999
            END IF
 300     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 1, IANRNO, BUFFAN, BUFFAN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1400) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ANTMOD: ERROR',I3,' READING OUTPUT AN TABLE')
 1200 FORMAT ('ANTMOD: ERROR',I3,' READING AN ROW ',I4)
 1300 FORMAT ('ANTMOD: ERROR',I3,' WRITING AN ROW ',I4)
 1400 FORMAT ('ANTMOD: ERROR',I3,' CLOSING OUTPUT AN TABLE')
      END
      SUBROUTINE SOUMOD (IDIR, DISK, CNO, SOUS, DX, DY, IERR)
C-----------------------------------------------------------------------
C   Subroutine to make the correction of the selected source position
C   if OPCODE ='ANTP' and the source correction parameters are not
C   equal zero.
C   Inputs:
C      IDIR    I        Direction: 0 -> add to apparent, find mean
C                                  1 -> add to mean, find apparent
C      DISK    I        The file disk number
C      CNO     I        The file catalog slot number.
C      SOUS    I        The source ID number
C   In/out: out = correction in apparent coord
C      DX      D        Correction at the picture plane in RA direction,
C                       in degrees.
C      DY      D        Correction at DEC, in degrees.
C   Input from common /MAPHDR/
C      CATBLK  I(256)   Catalog header record.
C-----------------------------------------------------------------------
      INTEGER   IDIR, DISK, CNO, SOUS, IERR
      DOUBLE PRECISION DX, DY
C                                       Declarations for recalculation
C                                       aparent coordinates to epoch
      INTEGER   DIR
      REAL      POLAR(2)
      CHARACTER OBSDAT*8
      DOUBLE PRECISION JD, DELDAT, OBSPOS(3), RAATT, DECTT, DRA, COSDEC
      LOGICAL   GR
C
      INTEGER   BUFFER(512)
C                                       Declarations for SOUINI
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELDEF*8, VELTYP*8, CALCOD*4, SOUNAM*16
      INTEGER   JERR, IDSOU, SUKOLS(MAXSUC), LUN, SUNUMV(MAXSUC),
     *   QUAL, NUMIF, NSOURC, I, ISURNO, SUFQID
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, TRA, TDEC, LDX, LDY, RAOBS, DECOBS, TEPOCH, TAAPP,
     *   TECAPP
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION    LSRVEL(MAXIF), FREQO(MAXIF), LRESTF(MAXIF)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA DELDAT, OBSPOS, POLAR /1.D-6, 0.D0, 0.D0, 0.D0, 0., 0./
      DATA LUN /28/
C-----------------------------------------------------------------------
C                                       take RA, DEC(EPOC) from the
C                                       catalog
      RA = CATD(KDCRV+JLOCR)
      DEC = CATD(KDCRV+JLOCD)
      LDX = DX * 3.6D3
      LDY = DY * 3.6D3
C                                       Open for READ first to set
C                                       all variables
C                                       Open SU table
      CALL SOUINI ('READ', BUFFER, DISK, CNO, 1, CATBLK, LUN,
     *   NUMIF, VELDEF, VELTYP, SUFQID, ISURNO, SUKOLS, SUNUMV, JERR)
C                                       Go to modify the source
C                                       position at the header
C                                       if there is no SU table
      IF (JERR.NE.0) GO TO 400
C                                       then close
      CALL TABIO ('CLOS', 0, 1, BUFFER, BUFFER, JERR)
C                                       Open for write
      CALL SOUINI ('WRIT', BUFFER, DISK, CNO, 1, CATBLK, LUN,
     *   NUMIF, VELDEF, VELTYP, SUFQID, ISURNO, SUKOLS, SUNUMV, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1100)
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Loop through SU rows modifing
C                                       the selected source position
      NSOURC = BUFFER(5)
C                                       Find time of observation
      CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
C                                       Find Julian day, JD
      CALL JULDAY(OBSDAT, JD)
      EPOCH = CATR(KREPO)
      GR = .TRUE.
      DO 300 I = 1, NSOURC
C                                       Read record
         ISURNO = I
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, TEPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL,
     *      LRESTF, PMRA, PMDEC, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1200) I
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Do not trust apparent coords
C                                       recompute and compare
         RAATT = RAEPO * DG2RAD
         DECTT = DECEPO * DG2RAD
         DIR = 1
         CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *      RAATT, DECTT, TAAPP, TECAPP)
         TAAPP = RAD2DG * TAAPP
         TECAPP = RAD2DG * TECAPP
         IF (RAEPO.LT.0.0) RAEPO = RAEPO + 360.0D0
         IF (TAAPP.LT.0.0) TAAPP = TAAPP + 360.0D0
         IF (RAAPP.LT.0.0) RAAPP = RAAPP + 360.0D0
         IF ((ABS(TAAPP-RAAPP).GT.1.0D-2) .OR.
     *      (ABS(DECAPP-TECAPP).GT.1.0D-2)) THEN
            MSGTXT = 'CORRECTING BAD APPARENT COORDINATES FOR ' //
     *         SOUNAM
            CALL MSGWRT (7)
            END IF
         RAAPP = TAAPP
         DECAPP = TECAPP
C                                       Put the correction here
         IF (IDSOU.EQ.SOUS) THEN
C                                       add to apparent, find epoch
            IF (IDIR.EQ.0) THEN
C                                       Recalculate the source position
C                                       error in RA from the error at
C                                       the picture plane
               COSDEC = COS (DECAPP * DG2RAD)
               DRA = 0.0
               IF (COSDEC.NE.0.0) DRA = DX / COSDEC
               TRA = RAEPO
               TDEC = DECEPO
               RAAPP  = RAAPP + DRA
               DECAPP = DECAPP + DY
C                                       Find RAEPO, DECEPO for
C                                       the given epoch
               RAATT = RAAPP * DG2RAD
               DECTT = DECAPP * DG2RAD
               DIR = -1
               CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *            RAEPO, DECEPO, RAATT, DECTT)
               COSDEC = COS (DECEPO)
               RAEPO = RAEPO * RAD2DG
               DECEPO = DECEPO * RAD2DG
               TRA = (RAEPO - TRA) * 3.6D3 * COSDEC
               TDEC = (DECEPO - TDEC) * 3.6D3
               WRITE (MSGTXT,1000) 'Apparent', LDX, LDY
               CALL MSGWRT (3)
               WRITE (MSGTXT,1000) 'Epoch   ', TRA, TDEC
               CALL MSGWRT (3)
C                                       add to epoch, find apparent
            ELSE
               COSDEC = COS (DECEPO * DG2RAD)
               DRA = 0.0
               IF (COSDEC.NE.0.0) DRA = DX / COSDEC
               TRA = RAAPP
               TDEC = DECAPP
               RAEPO  = RAEPO + DRA
               DECEPO = DECEPO + DY
C                                       Find RAEPO, DECEPO for
C                                       the given epoch
               RAATT = RAEPO * DG2RAD
               DECTT = DECEPO * DG2RAD
               DIR = 1
               CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *            RAATT, DECTT, RAAPP, DECAPP)
               COSDEC = COS (TDEC*DG2RAD)
               RAAPP = RAAPP * RAD2DG
               DECAPP = DECAPP * RAD2DG
               TRA = (RAAPP - TRA) * 3.6D3 * COSDEC
               TDEC = (DECAPP - TDEC) * 3.6D3
               WRITE (MSGTXT,1000) 'Epoch   ', LDX, LDY
               CALL MSGWRT (3)
               WRITE (MSGTXT,1000) 'Apparent', TRA, TDEC
               CALL MSGWRT (3)
               DX = TRA / 3.6D3
               DY = TDEC / 3.6D3
               END IF
            END IF
         ISURNO = I
         CALL TABSOU ('WRIT', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1300) I
            CALL MSGWRT (8)
            GO TO 999
            END IF
 300     CONTINUE
C
C                                       Close table
      CALL TABIO ('CLOS', 0, 1, BUFFER, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1400)
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Modify the epoc RA, DEC at the
C                                       header if there is only one
C                                       source at the SU table and
C                                       RA.NE.0 or DEC.NE.0 at the header
      IF ((NSOURC.EQ.1 .AND.
     *      (ABS(RA).GT.0.0001) .OR. ABS(DEC).GT.0.0001)) THEN
         CATD(KDCRV+JLOCR) = RAEPO
         CATD(KDCRV+JLOCD) = DECEPO
         CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFFER, IERR)
         END IF
      GO TO 999
C                                       Modify the epoc RA, DEC at the
C                                       header if there is no SU table
 400  WRITE (MSGTXT,1400)
      CALL MSGWRT (4)
C                                       Find time of observation
      CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
C                                       Find Julian day, JD
      CALL JULDAY(OBSDAT, JD)
C                                       Find RAAPP, DECAP for
C                                       observation date.
      EPOCH = CATR(KREPO)
      RAEPO = RA * DG2RAD
      DECEPO = DEC * DG2RAD
      GR = .TRUE.
      DIR = 1
      CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *   RAEPO, DECEPO, RAAPP, DECAPP)
      IERR = 0
C                                       add to apparent
      IF (IDIR.EQ.0) THEN
         TRA = RAEPO
         TDEC = DECEPO
C                                       Recalculate the source position
C                                       error in RA from the error at
C                                       the picture plane
         COSDEC = COS (DECAPP)
         DRA = 0.0
         IF (COSDEC.NE.0.0) DRA = DX / COSDEC
         RAAPP  = RAAPP + DRA*DG2RAD
         DECAPP = DECAPP + DY*DG2RAD
C                                       Find RAEPO, DECEPO for the
C                                       corrected RAAPP, DECAPP
         GR = .TRUE.
         DIR = -1
         CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *      RAEPO, DECEPO, RAAPP, DECAPP)
         COSDEC = COS (DECEPO)
         RAEPO = RAEPO * RAD2DG
         DECEPO = DECEPO * RAD2DG
         TRA = (RAEPO - TRA) * 3.6D3 * COSDEC
         TDEC = (DECEPO - TDEC) * 3.6D3
         WRITE (MSGTXT,1000) 'Epoch', TRA, TDEC
         CALL MSGWRT (3)
C                                       add to EPOCH
      ELSE
         TRA = RAAPP * RAD2DG
         TDEC = DECAPP * RAD2DG
         RAEPO = RA * DG2RAD
         DECEPO = DEC * DG2RAD
         COSDEC = COS (DECEPO)
         DRA = 0.0
         IF (COSDEC.NE.0.0) DRA = DX / COSDEC
         RAEPO  = RAEPO + DRA*DG2RAD
         DECEPO = DECEPO + DY*DG2RAD
         DIR = 1
         CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *      RAEPO, DECEPO, RAAPP, DECAPP)
         COSDEC = COS (DECAPP)
         RAAPP = RAAPP * RAD2DG
         DECAPP = DECAPP * RAD2DG
         TRA = (RAAPP - TRA) * 3.6D3 * COSDEC
         TDEC = (DECAPP - TDEC) * 3.6D3
         WRITE (MSGTXT,1000) 'Apparent', TRA, TDEC
         CALL MSGWRT (3)
         DX = TRA / 3.6D3
         DY = TDEC / 3.6D3
         END IF
      CATD(KDCRV+JLOCR) = RAEPO * RAD2DG
      CATD(KDCRV+JLOCD) = DECEPO * RAD2DG
      RA = CATD(KDCRV+JLOCR)
      DEC = CATD(KDCRV+JLOCD)
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOUMOD: change in ',A,' RA,DEC',2(1PE13.5),' asec')
 1100 FORMAT ('SOUMOD: SU table is not modified.',
     *   ' Error opening SU table')
 1200 FORMAT ('SOUMOD: SU table is not modified. Error reading row',I4)
 1300 FORMAT ('SOUMOD: SU table is not modified. Error writing row',I4)
 1400 FORMAT ('SOUMOD: Error closing SU table')
      END
      SUBROUTINE GETINP (FILE, IERR)
C-----------------------------------------------------------------------
C  This subroutine reads, from an input file specified by name "file",
C  the list of the vertical atmosphere delays for the given antenna
C  and time.
C
C   Inputs:
C    FILE     C*48  File name
C   Outputs in common:
C    NLINES    I     Number of data lines
C    IAN(*)    I     The antenna number
C    TMES(*)   D     The measurement time, in days
C    VERDEL(*) R     The atmosphere vertical delay, in cm
C    CLKDEL(*) R     The clock delay, in cm
C    DVERDE(*) R     Zenith atmos. delay derivative, in sec/sec*1.0E14
C    DCLKDE(*) R     Clock drift in sec/sec*1.0E14
C   Outputs:
C    IERR     I     Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      INTEGER   I, IERR
      INTEGER   LUN, FIND, NBYTES, KBP, IA, IIANT, NANSEL
      LOGICAL   F
      CHARACTER   CNUMBS(11)
      LOGICAL DONUM, DOCHA
      INTEGER IND
C
      INTEGER  DD, HH, MM, JT, JTRIM
      REAL SS
      CHARACTER LINE*80
      CHARACTER CX*3
      DOUBLE PRECISION X
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA F /.FALSE./
      DATA CNUMBS /' ','0','1','2','3','4','5','6','7','8','9'/
C-----------------------------------------------------------------------
C                                       Open text file
      LUN = 10
      CALL ZTXOPN ('READ', LUN, FIND, FILE, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001)
         GO TO 990
         END IF
C                                       Get number of lines
      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       Get value
      KBP = 1
      NBYTES = 80
      CALL GETNUM (LINE, NBYTES, KBP, X)
      NLINES = X + 0.1
C                                       Tell user
      WRITE (MSGTXT,2000) NLINES
      CALL MSGWRT (6)
C                                       antennas are in numbers or
C                                       in two characters?
C                                       Look the first line only!
      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       Get antenna value
      READ (LINE, 1111) CX
C                                       Antenna in characters if at
C                                       least one char (in 3 chars)
C                                       is not number
C      DOCHA = .FALSE.
C      DONUM = .TRUE.
      DO 10 IND = 1, 11
         IF (CX(1:1).EQ.CNUMBS(IND)) THEN
C                                       the first symbol is number
            DONUM = .TRUE.
            DOCHA = .FALSE.
            GO TO 15
            END IF
   10    CONTINUE
C                                       The first of 3 is char
      DOCHA = .TRUE.
      DONUM = .FALSE.
      GO TO 40
   15 CONTINUE
C
      DO 20 IND = 1, 11
         IF (CX(2:2).EQ.CNUMBS(IND)) THEN
C                                       the second symbol is number
            DONUM = .TRUE.
            DOCHA = .FALSE.
            GO TO 25
            END IF
   20    CONTINUE
C                                       The second of 3 is char
      DOCHA = .TRUE.
      DONUM = .FALSE.
      GO TO 40
   25 CONTINUE
      DO 30 IND = 1, 11
         IF (CX(3:3).EQ.CNUMBS(IND)) THEN
C                                       the third symbol is number
            DONUM = .TRUE.
            DOCHA = .FALSE.
            GO TO 40
            END IF
   30    CONTINUE
C                                       The third of 3 is char
      DOCHA = .TRUE.
      DONUM = .FALSE.
   40    CONTINUE
C
      NANSEL =  NANTSL
C                                       all antennas selected
C                                       (ANTENNAS = 0)
      IF (NANTSL.EQ.0) NANSEL = NSTNS
C                                       Read measurment info
      DO 100 I = 1,NLINES
         IF (I.GT.1) THEN
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.NE.0) GO TO 980
            JT = JTRIM (LINE)
            END IF
C                                       Get values
         IF (DONUM) THEN
C                                       Numbers stand for antennas
            KBP = 1
            CALL GETNUM (LINE, NBYTES, KBP, X)
            IAN(I) = X
         ELSE
C                                       characters stand for antennas
            READ (LINE, 1111) CX
C                                       identify IAN(I) to zero
            IAN(I) = 0
C                                       find the antenna number
C                                       corresponed to the name CX
            DO 60 IA = 1, NANSEL
               IF (NANTSL.EQ.0) THEN
                  IIANT = IA
               ELSE
                  IIANT = ANTENS(IA)
                  END IF
               IF (STNNAM(IIANT)(1:2).EQ.CX(1:2)) THEN
                  IAN(I) = IIANT
                  GO TO 80
                  END IF
   60          CONTINUE
   80       CONTINUE
C
            KBP = 4
            END IF
C
         CALL GETNUM (LINE, NBYTES, KBP, X)
         DD = X
         CALL GETNUM (LINE, NBYTES, KBP, X)
         HH = X
         CALL GETNUM (LINE, NBYTES, KBP, X)
         MM = X
         CALL GETNUM (LINE, NBYTES, KBP, X)
         SS = X
C                                       Tell user
         TMES(I) = DD + HH/24.0 + MM/(24.*60.) + SS/(24.*3600.)
         CALL GETNUM (LINE, NBYTES, KBP, X)
         VERDEL(I) = X
         CALL GETNUM (LINE, NBYTES, KBP, X)
         CLKDEL(I) = X
         CALL GETNUM (LINE, NBYTES, KBP, X)
         DVERDE(I) = X
         CALL GETNUM (LINE, NBYTES, KBP, X)
         DCLKDE(I) = X
C         WRITE (MSGTXT,2001) I, IAN(I), DD, HH, MM, SS, TMES(I),
C     *      VERDEL(I), CLKDEL(I), DVERDE(I), DCLKDE(I)
C         CALL MSGWRT (6)
 100     CONTINUE
C                                       close input file
      CALL ZTXCLS (LUN, FIND, IERR)
C
      GO TO 999
C                                       Read error
 980  WRITE (MSGTXT,1980) IERR
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ERROR ',I3,' OPENING  INFO TEXT FILE')
 1111 FORMAT (A3)
 1980 FORMAT ('ERROR ',I3,' READING ANTENNA INFO TEXT FILE')
 2000 FORMAT ('Number of lines = ',I4)
C2001 FORMAT (I4,I3,1X,3(I2,':'),F4.1,F12.8,F6.2,F6.2,F12.5,F12.5)
      END
      SUBROUTINE GETRAD (FILE, IERR)
C-----------------------------------------------------------------------
C  This subroutine reads, the list of the RA, DEC corrections and
C  the relevant times from an input file specified by name "file",
C
C   Inputs:
C    FILE     C*48  File name
C   Outputs in common:
C    NLINES    I     Number of data lines
C    TMES(*)   D     The measurement time, in days
C    RADR(*)   R     RA corrections, in mas
C    DECDR(*)  R     DEC corrections, in mas
C   Outputs:
C    IERR     I     Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      CHARACTER OBSDAT*8
      INTEGER   I, IERR
      INTEGER   LUN, FIND, NBYTES, KBP, JT, JTRIM
      LOGICAL   F
C
      CHARACTER LINE*80
      DOUBLE PRECISION X
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Find time of observation
      CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
C                                       Find Julian day, JD
      CALL JULDAY(OBSDAT, JD)
C                                       Open text file
      LUN = 10
      CALL ZTXOPN ('READ', LUN, FIND, FILE, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001)
         GO TO 990
         END IF
C                                       Get number of lines
      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       Get value
      KBP = 1
      NBYTES = 80
      CALL GETNUM (LINE, NBYTES, KBP, X)
      NLINES = X + 0.1
C                                       Tell user
      WRITE (MSGTXT,2000) NLINES
      CALL MSGWRT (6)
C                                       Read measurment info
      DO 100 I = 1,NLINES
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.NE.0) GO TO 980
         JT = JTRIM (LINE)
C                                       Get values
         KBP = 1
C
         CALL GETNUM (LINE, NBYTES, KBP, X)
         TMES(I) = X - JD
         CALL GETNUM (LINE, NBYTES, KBP, X)
         RADR(I) = X
         CALL GETNUM (LINE, NBYTES, KBP, X)
         DECDR(I) = X
 100     CONTINUE
C                                       close input file
      CALL ZTXCLS (LUN, FIND, IERR)
C
      GO TO 999
C                                       Read error
 980  WRITE (MSGTXT,1980) IERR
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ERROR ',I3,' OPENING  INFO TEXT FILE')
 1980 FORMAT ('ERROR ',I3,' READING ANTENNA INFO TEXT FILE')
 2000 FORMAT ('Number of lines = ',I4)
C2001 FORMAT (I4,I3,1X,3(I2,':'),F4.1,F12.8,F6.2,F6.2,F12.5,F12.5)
      END
      SUBROUTINE USNORE (FILE, IERR)
C-----------------------------------------------------------------------
C  This subroutine reads from an input file specified by name "file",
C  the list of the UT1-TAI, POLX, and POLY for the given JDs set
C  at the vicinity of the observation date.
C  The input file can be picked up from
C  http://gemini.gsfc.nasa.gov/solve_save/usno_finals.erp
C
C   Inputs:
C    FILE       C*48  File name
C   Input in common:
C    JD         D     Julian day of the observation
C    BCOUNT     I     Number of days preceeded JD of observation
C    NCOUNT     I     Number of day used starting JD-BCOUNT
C    Example BCOUNT=1, ECOUNT=5 => JD is the second day; and 3 days
C                                     follow the JD
C   Outputs in common:
C    UT1TAI(*)  D     UT1-TAI,  in sec
C    XYWOB(2,*) D     X,Y pole wobble, in milliarcsec
C   Outputs:
C    IERR     I     Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER FILE*48, LASDAT*8
      DOUBLE PRECISION JDLAS, JDOBS, DDDAY, NNDAY
      INTEGER   IERR
      INTEGER   LUN, FIND, NBYTES, KBP
      INTEGER   YLAST, MLAST, DLAST
      LOGICAL   F
C
      INTEGER  ILINE, JT, JTRIM
      DOUBLE PRECISION  JDFILE
      CHARACTER LINE*80
      DOUBLE PRECISION X
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Open text file
      LUN = 10
      CALL ZTXOPN ('READ', LUN, FIND, FILE, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001)
         GO TO 990
         END IF

         MSGTXT = 'UT1-UTC(sec), POLX, POLY(milliasec) picked up ' //
     *      'from the USNO file'
         CALL MSGWRT (4)
C                                       Read the file lines
      ILINE = 0
  10  CONTINUE
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.NE.0) GO TO 980
         JT = JTRIM (LINE)
C                                       pick up the last real data
         IF (LINE(18:21).EQ.'real') THEN
            READ (LINE(30:39),1010) YLAST, MLAST, DLAST
            WRITE (LASDAT, 1020) YLAST, MLAST, DLAST
            CALL JULDAY(LASDAT, JDLAS)
            JDOBS = JD
            DDDAY = JDLAS - JDOBS
            NNDAY = NCOUNT - BCOUNT - 1
            IF (DDDAY.LT.NNDAY) THEN
C                                       there is no enough days
C                                       with real data after OBSDAT
               IERR = 1


      MSGTXT = '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1030) DDDAY
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1040) NNDAY
         CALL MSGWRT (8)
      MSGTXT = '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
               GO TO 990
               END IF
            END IF
C                                       skip the comment lines
         IF (LINE(1:1).EQ.'#' .OR. LINE(1:1).EQ.'E') GO TO 10
C
C                                       Get values
         KBP = 1
         NBYTES = 80
C
         CALL GETNUM (LINE, NBYTES, KBP, X)
         JDFILE = X
C                                       look for the beginning line
C                                       JDFILE = JD-1
         IF (JDFILE.LT. JD-BCOUNT) GO TO 10
         ILINE = ILINE + 1
C                                       XPOLE in milli arcsec
         CALL GETNUM (LINE, NBYTES, KBP, X)
         XYWOB(1,ILINE) = X*100
C                                       YPOLE in milli arcsec
         CALL GETNUM (LINE, NBYTES, KBP, X)
         XYWOB(2,ILINE) = X*100
C                                       UT1TAI in sec
         CALL GETNUM (LINE, NBYTES, KBP, X)
C         UT1TAI(ILINE) = MOD(X/1.D6, 1.D0)
         UT1TAI(ILINE) = X/1.D6
C                                       print out
C                                       the picked up values
         WRITE (MSGTXT, 1050) ILINE, JDFILE, UT1TAI(ILINE)+LEAPS(2),
     *            XYWOB(1,ILINE), XYWOB(2,ILINE)
         CALL MSGWRT (8)


C                                       the required number of days
         IF (ILINE.EQ.NCOUNT) GO TO 20
         GO TO 10
 20      CONTINUE
C                                       close input file
      CALL ZTXCLS (LUN, FIND, IERR)
C
      GO TO 999
C                                       Read error
 980  WRITE (MSGTXT,1980) IERR
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ERROR ',I3,' OPENING  INFO TEXT FILE')
 1010 FORMAT (I4, 1X, I2, 1X, I2)
 1020 FORMAT (I4, I2, I2)
 1030 FORMAT ('! Last day with real data in INFILE minus OBSDAT '
     *   ,F5.0,'!')
 1040 FORMAT ('! is smaller than requred one after OBSDAT       ',
     *   F5.0,'!')
 1050 FORMAT (I5, F10.1, 2X, F12.6, 2F10.3)
 1980 FORMAT ('ERROR ',I3,' READING ANTENNA INFO TEXT FILE')
      END
C
      SUBROUTINE PLLIST (FILE, IERR)
C-----------------------------------------------------------------------
C  This subroutine reads the list of the planets from an input file
C  specified by name "file"
C
C   Inputs:
C    FILE     C*48  File name
C   Outputs in common:
C    NPLANE    I     Number of planets in the list
C    PLANET    C*12  The planet list
C    PLMASS    D(12) The planet masses
C   Outputs:
C    IERR     I     Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      CHARACTER PLALL(10)*12
      INTEGER   I, IPL, KPL, IPLA, ISTA, IEND, IERR
      INTEGER   LUN, FIND, NBYTES, KBP, NFLINE, JT, JTRIM
      DOUBLE PRECISION PLM(10)
      LOGICAL   F
C
      CHARACTER LINE*80
      DOUBLE PRECISION X
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
C      INCLUDE 'INCS:DDCH.INC'
      DATA PLALL / 'MERCURY     ', 'VENUS       ', 'MOON        ',
     *             'MARS        ', 'JUPITER     ', 'SATURN      ',
     *             'URANUS      ', 'NEPTUNE     ', 'PLUTO       ',
     *             'SUN         ' /
      DATA PLM   / 3.18D23,        4.88D24,        7.36D22,
     *             6.42D23,        1.90D27,        5.68D26,
     *             8.68D25,        1.03D26,        1.40D22,
     *             1.99D30 /
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IF (FILE.EQ.' ') THEN
         NPLANE = 1
         PLANET(1)=  'SUN         '
         PLMASS(1) = 1.99D30
         GO TO 999
         END IF
C                                       Open text file
      LUN = 10
      CALL ZTXOPN ('READ', LUN, FIND, FILE, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001)
         GO TO 990
         END IF
C                                       Get number of lines
      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       Get value
      KBP = 1
      NBYTES = 80
      CALL GETNUM (LINE, NBYTES, KBP, X)
      NFLINE = X + 0.1
C                                       Tell user
C     WRITE (MSGTXT,2000) NFLINE
C     CALL MSGWRT (6)
C                                       Read measurment info
C     IPLA = 0
      DO 100 I = 1,NFLINE
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.NE.0) GO TO 980
         JT = JTRIM (LINE)
C
         DO 80 IPL = 1, 60, 12
            IPLA = IPLA + 1
            ISTA = IPL
            IEND = IPL + 11
            PLANET(IPLA) = LINE(ISTA:IEND)
            IF (PLANET(IPLA).EQ.' ') THEN
               GO TO 90
            ELSE
C                                       associate the mass of the planet
               DO 60 KPL = 1, 10
                  IF (PLANET(IPLA).EQ.PLALL(KPL))
     *               PLMASS(IPLA) = PLM(KPL)
   60             CONTINUE
               END IF
   80       CONTINUE
C         WRITE (MSGTXT,2001) I, IAN(I), DD, HH, MM, SS, TMES(I),
C     *      VERDEL(I), CLKDEL(I), DVERDE(I), DCLKDE(I)
C         CALL MSGWRT (6)
            GO TO 100
   90       IPLA = IPLA - 1
 100     CONTINUE
      NPLANE = IPLA
C                                       close input file
      CALL ZTXCLS (LUN, FIND, IERR)
C
      GO TO 999
C                                       Read error
 980  WRITE (MSGTXT,1980) IERR
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ERROR ',I3,' OPENING  INFO TEXT FILE')
 1980 FORMAT ('ERROR ',I3,' READING ANTENNA INFO TEXT FILE')
C 2000 FORMAT ('Number of lines = ',I4)
C 2001 FORMAT (I4, I3, 1X, 3(I2,':'), F4.1, 2X, F10.8, F6.2, F6.2,
C     *   2X, F10.5, 2X, F10.5)
      END
      SUBROUTINE ATMOV (IERR)
C-----------------------------------------------------------------------
C   Routine to apply atmospheric corrections to the given antenna
C   and time based on array given at INFILE
C   Corrections are applied to the CL record in Common.
C   Control info from common:
C      NLINES    I    number of elements at the following arrays
C      IAN(*)    I    array of antenna numbers
C      TMES(*)   D    array of measurement times, in days
C      VERDEL(*) R    array of atmosphere vertical delay, in cm
C      CLKDEL(*) R     The clock delay, in cm
C      ISTOK     I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C   Output:
C      IERR      I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, IANT, I, ITEMP, PERR
      LOGICAL   DOCLAT
      REAL      XT, YT, PDLY, DPDLY, CFAC, SFAC, FQFAC, ZA, ELV, HA,
     *   VERINT, CLKINT, DATINT, DCLINT, DELL, DELR, CLKL, CLKR, DATL,
     *   DATR, DCLL, DCLR, DELDT, AZ, PDLYAT, PDLYCL, DPDATZ, DPDLAT,
     *   DPDLCL, PRESS, MDRY, MWET, COSZI
      DOUBLE PRECISION FREQS, SINLAT, COSLAT, TCLT, TMESA, TLEFT,
     *   TRIGHT, STNHT, DRYDEL
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN /30/
C-----------------------------------------------------------------------
      IERR = 0
C                                       time at the CL table line,
C                                       in days
      TCLT = CLRECD(TIMCL)
C                                       antenna number at the CL table
C                                       line
      IANT = CLRECI(ANTCL)
C                                       If TROP then calulate dry zenith
C                                       delay using the pressure, station
C                                       lat. and station height.
C
      IF (ICODE.EQ.19) THEN
         CALL GETPRE(TCLT,IANT,PRESS,PERR)
         IF(PERR.EQ.0)
     *   DRYDEL= (2.2768D-03 * PRESS) / (1 - 0.00266 *
     *      COS(2.0D0 * STNLAT(IANT)) - 2.8D-07 * STNHT(IANT))
         ENDIF

C                                       Interpolate the vertical
C                                       atmosphere delay given at the
C                                       array VERDEL(*) and clock delay
C                                       CLKDEL(*) read from the
C                                       INFILE
      TLEFT = 0
      TRIGHT = 100
      DO 100 I = 1, NLINES
         TMESA = TMES(I)
         IF (IANT.EQ.IAN(I)) THEN
            IF ((TMESA.GT.TCLT) .AND. (TMESA.LT.TRIGHT)) THEN
               TRIGHT = TMESA
               DELR = VERDEL(I)
               CLKR = CLKDEL(I)
               DATR = DVERDE(I)
               DCLR = DCLKDE(I)
               END IF
            IF ((TMESA.LT.TCLT) .AND. (TMESA.GT.TLEFT)) THEN
               TLEFT = TMESA
               DELL = VERDEL(I)
               CLKL = CLKDEL(I)
               DATL = DVERDE(I)
               DCLL = DCLKDE(I)
               END IF
            END IF
  100    CONTINUE
      IF (TRIGHT.EQ.100) THEN
         IF (TLEFT.EQ.0) THEN
            VERINT = 0
            CLKINT = 0
            DATINT = 0
            DCLINT = 0
         ELSE
            VERINT = DELL
            CLKINT = CLKL
            DATINT = DATL
            DCLINT = DCLL
            END IF
      ELSE
         IF (TLEFT.EQ.0) THEN
            VERINT = DELR
            CLKINT = CLKR
            DATINT = DATR
            DCLINT = DCLR
         ELSE
C                                       make the interpolation itself
            VERINT = DELL + (DELR-DELL) * (TCLT-TLEFT)/(TRIGHT-TLEFT)
            CLKINT = CLKL + (CLKR-CLKL) * (TCLT-TLEFT)/(TRIGHT-TLEFT)
            DATINT = DATL + (DATR-DATL) * (TCLT-TLEFT)/(TRIGHT-TLEFT)
            DCLINT = DCLL + (DCLR-DCLL) * (TCLT-TLEFT)/(TRIGHT-TLEFT)
            END IF
         END IF
C
C                                       VERINT, CLKINT are
C                                       the interpolated vertical
C                                       atmosphere and clock delays
C                                       at time of the CL table line
C
C                                       previous source ID
      LSTSOU = PARM(1) + 0.5
C                                       Will only atmosphere or sum
C                                       of atmosphere and clock
C                                       be corrected at CL table
      DOCLAT = (PARM(2).LT.0.1)
C                                       source ID at the CL table line
      THSOU = CLRECI(SOUCL)
      IF (LSTSOU.NE.THSOU) THEN
C                                       Get source info
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
C
         PARM(1) = THSOU
         FREQS = FREQ + FREQO(BIF)
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
         END IF
C
      CALL SOUELV (IANT, CLRECD(TIMCL), HA, ELV, AZ)
C                                       zenith angle of the source
C                                       from the antenna
      ZA = (PI/2.0D0 - ELV)
      COSLAT = COS (STNLAT(IANT))
      SINLAT = SIN (STNLAT(IANT))
C
      PDLYCL = CLKINT/100./VELITE
      DPDATZ = DATINT * 1.0E-14
      DPDLCL = DCLINT * 1.0E-14
C
      IF (ELV.GT.0.05) THEN
         IF (ICODE.EQ.18) THEN
C                                       'ATMO'
            PDLYAT = VERINT/100./SIN(ELV)/VELITE
         ELSE
            IF (ICODE.EQ.23) THEN
C                                       'IONO'
               COSZI = SQRT(1 - (RATIO*COS(ELV))**2)
               PDLYAT = VERINT/100./COSZI/VELITE
               END IF
            END IF

C                                       If TROP then subtract the input
C                                       total atmos. delay from the corr.
C                                       model to get the difference.
C                                       Then treat as OPCODE='ATMO'
         IF (ICODE.EQ.19) THEN
             IF(PERR.EQ.0)
     *          PDLYAT = (MDRY(ELV) * DRYDEL +
     *             MWET(ELV) * (VERINT/100 - DRYDEL)) / VELITE
             PDLYAT = CLRECR(ATMCL) - PDLYAT
         ENDIF
         IF (DOCLAT) THEN
            PDLY = PDLYAT
         ELSE
            PDLY = PDLYAT + PDLYCL
            END IF
      ELSE
         PDLY = 0
         END IF
C                                       Now evaluate derivative of delay
C
C                                       DELDT is derivative of SIN(ELV)
C                                       (by time) = COS(EL)* D(EL)/DT
C                                       COS(ELV)
      DELDT = -7.29211E-5 * SIN (HA) * COSLAT * COSDEC
C                                       derivative of atmosphere
C                                       in sec/sec without /SIN(ELV)
      DPDLAT = -PDLYAT * DELDT
C
      IF (ELV.GT.0.05) THEN
         IF (ICODE.EQ.23) THEN
C                                       'IONO'
            DPDLAT = (DPDLAT*(RATIO**2)/COSZI*SIN(ELV) + DPDATZ) /
     *         COSZI
         ELSE
            DPDLAT = (DPDLAT + DPDATZ) / SIN(ELV)
            END IF
      ELSE
         DPDLAT = 0
         END IF
C
C                                       Correct only atmosphere?
      IF (DOCLAT) THEN
         DPDLY = DPDLAT
      ELSE
         DPDLY = DPDLAT + DPDLCL
         END IF
C                                       Atmospheric group delay
      IF (CLRECR(ATMCL).NE.FBLANK) CLRECR(ATMCL) = CLRECR(ATMCL) -
     *   PDLYAT
C                                       Atmospheric group delay rate
      IF (CLRECR(DATMCL).NE.FBLANK) CLRECR(DATMCL) = CLRECR(DATMCL) -
     *   DPDLAT
      IF (ISTOK.NE.2) THEN
C                                       clock shift for first stokes
         IF (.NOT.DOCLAT .AND. CLRECR(CLK1CL).NE.FBLANK)
     *      CLRECR(CLK1CL) = CLRECR(CLK1CL) - PDLYCL
C                                       clock drift for first stokes
         IF (.NOT.DOCLAT .AND. CLRECR(DCK1CL).NE.FBLANK)
     *      CLRECR(DCK1CL) = CLRECR(DCK1CL) - DPDLCL
C                                       multiband delay for both stokes
         IF (CLRECR(MBD1CL).NE.FBLANK) CLRECR(MBD1CL) = CLRECR(MBD1CL)
     *      + PDLY
         DO 600 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
C                                       change the sign (ISFAC=-1)
C                                       for 'IONO'
            SFAC = ISFAC*SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE1CL+I-1)
            YT = CLRECR(IM1CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM1CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE1CL+I-1).NE.FBLANK)
     *         CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *         CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600        CONTINUE
         END IF
C                                       2nd polarization
      IF (ABS(ISTOK).NE.1) THEN
C                                       clock shift for second stokes
         IF (.NOT.DOCLAT .AND. CLRECR(CLK2CL).NE.FBLANK)
     *      CLRECR(CLK2CL) = CLRECR(CLK2CL) - PDLYCL
C                                       clock drift for second stokes
         IF (.NOT.DOCLAT .AND. CLRECR(DCK2CL).NE.FBLANK)
     *      CLRECR(DCK2CL) = CLRECR(DCK2CL) - DPDLCL
C                                       multiband delay for both stokes
         IF (CLRECR(MBD2CL).NE.FBLANK) CLRECR(MBD2CL) = CLRECR(MBD2CL)
     *      + PDLY
         DO 700 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
C                                       change the sign (ISFAC=-1)
C                                       for 'IONO'
            SFAC = ISFAC*SIN (FQFAC)
C
C                                       Phase
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM2CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE2CL+I-1).NE.FBLANK)
     *         CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *         CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
 700        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE DISPV (IERR)
C-----------------------------------------------------------------------
C   Routine to apply dispersion corrections versus time based on array
C   given at INFILE.  Corrections are applied to the CL record in Common
C   Control info from common:
C      NLINES    I    number of elements at the following arrays
C      IAN(*)    I    array of antenna numbers
C      TMES(*)   D    array of measurement times, in days
C      VERDEL(*) R    array of atmosphere vertical dispersion, in 1/cm
C      CLKDEL(*) R    rate of change of VERDEL * 1.E14
C      ISTOK     I    Polarization to correct, 1=first, 2=second,
C                     0 = both
C   Output:
C      IERR      I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, IANT, I
      REAL      PDLY, DPDLY, ZA, ELV, HA, VERINT, DATINT, DELL, DELR,
     *   DATL, DATR, DELDT, AZ, PDLYAT, DPDATZ, DPDLAT
      DOUBLE PRECISION FREQS, SINLAT, COSLAT, TCLT, TMESA, TLEFT,
     *   TRIGHT
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN /30/
C-----------------------------------------------------------------------
      IERR = 0
C                                       time at the CL table line,
C                                       in days
      TCLT = CLRECD(TIMCL)
C                                       antenna number at the CL table
C                                       line
      IANT = CLRECI(ANTCL)
C                                       Interpolate the vertical
C                                       atmosphere delay given at the
C                                       array VERDEL(*) and clock delay
C                                       CLKDEL(*) read from the
C                                       INFILE
      TLEFT = 0
      TRIGHT = 100
      DO 100 I = 1, NLINES
         TMESA = TMES(I)
         IF (IANT.EQ.IAN(I)) THEN
            IF ((TMESA.GT.TCLT) .AND. (TMESA.LT.TRIGHT)) THEN
               TRIGHT = TMESA
               DELR = VERDEL(I)
               DATR = CLKDEL(I)
               END IF
            IF ((TMESA.LT.TCLT) .AND. (TMESA.GT.TLEFT)) THEN
               TLEFT = TMESA
               DELL = VERDEL(I)
               DATL = CLKDEL(I)
               END IF
            END IF
  100    CONTINUE
      IF (TRIGHT.EQ.100) THEN
         IF (TLEFT.EQ.0) THEN
            VERINT = 0
            DATINT = 0
         ELSE
            VERINT = DELL
            DATINT = DATL
            END IF
      ELSE
         IF (TLEFT.EQ.0) THEN
            VERINT = DELR
            DATINT = DATR
         ELSE
C                                       make the interpolation itself
            VERINT = DELL + (DELR-DELL) * (TCLT-TLEFT)/(TRIGHT-TLEFT)
            DATINT = DATL + (DATR-DATL) * (TCLT-TLEFT)/(TRIGHT-TLEFT)
            END IF
         END IF
C
C                                       VERINT, CLKINT are
C                                       the interpolated vertical
C                                       atmosphere and clock delays
C                                       at time of the CL table line
C
C                                       previous source ID
      LSTSOU = PARM(1) + 0.5
C                                       source ID at the CL table line
      THSOU = CLRECI(SOUCL)
C                                       Get source info
      IF (LSTSOU.NE.THSOU) THEN
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
C
         PARM(1) = THSOU
         FREQS = FREQ + FREQO(BIF)
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
         END IF
C
      CALL SOUELV (IANT, CLRECD(TIMCL), HA, ELV, AZ)
C                                       zenith angle of the source
C                                       from the antenna
      ZA = (PI/2.0D0 - ELV)
      COSLAT = COS (STNLAT(IANT))
      SINLAT = SIN (STNLAT(IANT))
C
      DPDATZ = DATINT * 1.0E-14
C                                       'DISP'
      IF (ELV.GT.0.05) THEN
         PDLYAT = VERINT/100./SIN(ELV)/VELITE
         PDLY = PDLYAT
      ELSE
         PDLY = 0
         END IF
C                                       Now evaluate derivative of delay
C
C                                       DELDT is derivative of SIN(ELV)
C                                       (by time) = COS(EL)* D(EL)/DT
C                                       COS(ELV)
      DELDT = -7.29211E-5 * SIN (HA) * COSLAT * COSDEC
C                                       derivative of atmosphere
C                                       in sec/sec without /SIN(ELV)
      DPDLAT = -PDLYAT * DELDT
C
      IF (ELV.GT.0.05) THEN
         DPDLAT = (DPDLAT + DPDATZ) / SIN(ELV)
      ELSE
         DPDLAT = 0
         END IF
C
C                                       Correct only atmosphere?
      DPDLY = DPDLAT
C                                       Dispersion
      IF (CLRECR(DIS1CL).NE.FBLANK) CLRECR(DIS1CL) = CLRECR(DIS1CL) +
     *   PDLY
C                                       dispersion rate
      IF (CLRECR(DDS1CL).NE.FBLANK) CLRECR(DDS1CL) = CLRECR(DDS1CL) +
     *   DPDLAT
C                                       2nd polarization
      IF (ABS(ISTOK).NE.1) THEN
C                                       Dispersion
         IF (CLRECR(DIS2CL).NE.FBLANK) CLRECR(DIS2CL) = CLRECR(DIS2CL) +
     *      PDLY
C                                       dispersion rate
         IF (CLRECR(DDS2CL).NE.FBLANK) CLRECR(DDS2CL) = CLRECR(DDS2CL) +
     *      DPDLAT
         END IF
C
 999  RETURN
      END
      SUBROUTINE SUNDEL (IERR)
C-----------------------------------------------------------------------
C   Routine to apply the time delay correction caused by the binding
C   of the light ray passing through the gravitational field of the
C   Sun.
C
C   Control info from common:
C      NLINES    I    number of elements at the following arrays
C      TMES(*)   D    array of measurement times, in days
C      SPACEX(*) R    array of the spacecraft X, in meters
C      SPACEY(*) R    array of the spacecraft Y, in meters
C      SPACEZ(*) R    array of the spacecraft Z, in meters
C      ISTOK     I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C   Output:
C      IERR      I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, IANT, I, ITEMP, YEAR, DAY, MONTH, ITEST
      LOGICAL   LINUN
      CHARACTER NAME*12
      REAL      XT, YT, PDLY, DPDLY, CFAC, SFAC, FQFAC, FDAY,
     *   TCLNEW, TCLOLD
      DOUBLE PRECISION FREQS, TCLT, TMESA, TLEFT, TRIGHT, FDDAY,
     *   SUNCV(6), PLANX(10), PLANY(10), PLANZ(10)
      DOUBLE PRECISION SPACXR, SPACYR, SPACZR, SPACXL, SPACYL, SPACZL,
     *   SPX, SPY, SPZ, ANTX, ANTY, ANTZ, ANTRX, ANTRY, ANTRZ
      DOUBLE PRECISION  GCONST, COEFF(10), GSTRA,
     *   R1, R2, RS, RS1, RS2, RSIND, RS1IND, RS2IND, CIND,
     *   DR21X, DR21Y, DR21Z, R1X, R1Y, R1Z, RSX, RSY, RSZ,
     *   PDINF1, PDINF2
C
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA GCONST, CIND /6.672D-11, 1.0D3/
C-----------------------------------------------------------------------
      IERR = 0
C                                       antenna number at the CL table
C                                       line
      IANT = CLRECI(ANTCL)
C                                       time at the CL table line,
C                                       in days
      TCLT = CLRECD(TIMCL)
C
      TCLNEW = TCLT
C                                       previous time at the CL table
C                                       line
      TCLOLD = PARM(2)

      IF (TCLNEW.EQ.TCLOLD) GO TO 200
C                                       calculate spacecraft and Sun
C                                       only for the another CL time
      PARM(2) = TCLNEW


C                                       Interpolate the spacecraft
C                                       coordinates read from the
C                                       OB table
      TLEFT = -100
      TRIGHT = 100
      DO 100 I = 1, NLINES
         TMESA = TMES(I)
         IF ((TMESA.GT.TCLT) .AND. (TMESA.LT.TRIGHT)) THEN
C                                       find the nearest (from right)
C                                       OB table time to the CL time
            TRIGHT = TMESA
            SPACXR = SPACEX(I)
            SPACYR = SPACEY(I)
            SPACZR = SPACEZ(I)
            END IF
         IF ((TMESA.LT.TCLT) .AND. (TMESA.GT.TLEFT)) THEN
C                                       find the nearest (from left)
C                                       OB table time to the CL time
            TLEFT = TMESA
            SPACXL = SPACEX(I)
            SPACYL = SPACEY(I)
            SPACZL = SPACEZ(I)
            END IF
  100    CONTINUE

      IF (TRIGHT.EQ.100) THEN
C                                       all OB times are lefter CL time
         SPX = SPACXL
         SPY = SPACYL
         SPZ = SPACZL
      ELSE
         IF (TLEFT.EQ.-100) THEN
C                                       all OB times are righter CL time
            SPX = SPACXR
            SPY = SPACYR
            SPZ = SPACZR
         ELSE
C                                       make the interpolation itself
            SPX = SPACXL + (SPACXR-SPACXL) *
     *         (TCLT-TLEFT)/(TRIGHT-TLEFT)
            SPY = SPACYL + (SPACYR-SPACYL) *
     *         (TCLT-TLEFT)/(TRIGHT-TLEFT)
            SPZ = SPACZL + (SPACZR-SPACZL) *
     *         (TCLT-TLEFT)/(TRIGHT-TLEFT)
            END IF
         END IF
C                                       Position of the space craft
C                                       SPX, SPY, SPZ are
C                                       in the RH equatorial coordinate
C                                       system as it given in OB table.
C                                       X at RA=0
C
C                                       FDAY(Real) Fraction of day
      FDAY = TCLT
C                                       FDDAY (DP) Fraction of day
      FDDAY = TCLT
C                                       Year, for example 2004
C      YEAR = PARM(3)
C                                       Day in year (1 = Jan 1st)
C      DAY = PARM(4)
C      CALL SUN (YEAR, DAY, FDAY, SUNCV)
C                                       Bryan's (JPL) software to
C                                       calculate the SUN position
      YEAR = PARM(3)
      MONTH = PARM(4)
      DAY = PARM(5)
C                                       UNIX/LINUX

      LINUN = BYTFLP.GT.0
      DO 120 I = 1, NPLANE
         NAME = PLANET(I)
C                                       calculate position of the planet
         CALL SUNJPL (YEAR, MONTH, DAY, FDDAY, NAME, LINUN, SUNCV)
C                                       Planet coordinates relatively
C                                       Earth center, in meters
C                                       in the RH equatorial coordinate
C                                       system. X at RA=0.
         PLANX(I) = SUNCV(1) * 1000
         PLANY(I) = SUNCV(2) * 1000
         PLANZ(I) = SUNCV(3) * 1000
C                                       COEFF before Logarithm [sec]
         COEFF(I) = GCONST * PLMASS(I) / VELITE/VELITE/VELITE
  120    CONTINUE
C
  200 CONTINUE

C                                       Antenna coordinates at the LH
C                                       coordinate system fixed in EARTH
C                                       X to Greenwich
      ANTX = STNX(IANT)
C                                       Make LH so following works
      ANTY = -STNY(IANT)
      ANTZ = STNZ(IANT)

C
Ctemoprally GSTIA0 from almonach
C      GSTIA0 = 201.327D0
      GSTRA = (GSTIA0 + 1.00273790265D0*TCLT*360) * DG2RAD
C                                       Rotate to the RH sky equatorial
C                                       coordinate system X at RA=0
      ANTRX = ANTX*COS(GSTRA) + ANTY*SIN(GSTRA)
      ANTRY = ANTX*SIN(GSTRA) - ANTY*COS(GSTRA)
      ANTRZ = ANTZ
C
C                                       gravitational delay of all
C                                       listed planets
      PDLY = 0
      PDINF1 = 0
      PDINF2 = 0
      DO 140 I = 1, NPLANE
C                                       R2 distance ANT-SUN
         R2 = SQRT ((ANTRX-PLANX(I))**2 + (ANTRY-PLANY(I))**2 +
     *      (ANTRZ-PLANZ(I))**2)
C                                       R1 distance
C                                       EARTH center-SUN center
         R1 = SQRT (PLANX(I)**2 + PLANY(I)**2 + PLANZ(I)**2)
C                                       RS distance Spacecraft-SUN
         RS = SQRT ((SPX-PLANX(I))**2 + (SPY-PLANY(I))**2 +
     *      (SPZ-PLANZ(I))**2)
C                                       space craft is CIND time
C                                       further
         RSIND = SQRT ((SPX*CIND-PLANX(I))**2 + (SPY*CIND-PLANY(I))**2
     *      +(SPZ*CIND-PLANZ(I))**2)
C                                       RS2 distance Spacecraft-ANT
         RS2 = SQRT ((SPX-ANTRX)**2 + (SPY-ANTRY)**2 +
     *      (SPZ-ANTRZ)**2)
C                                       space craft is CIND time
C                                       further
         RS2IND = SQRT ((SPX*CIND-ANTRX)**2 +
     *      (SPY*CIND-ANTRY)**2 + (SPZ*CIND-ANTRZ)**2)
C                                       RS1 distance Spacecraft-
C                                       EARTH center
         RS1 = SQRT (SPX**2 + SPY**2 + SPZ**2)
C                                       space craft is CIND time
C                                       further
         RS1IND = SQRT ((SPX*CIND)**2 + (SPY*CIND)**2 +
     *      (SPZ*CIND)**2)
C                                       calculate the extra delay
C                                       because of the bending at the
C                                       gravity field of the Sun
C                                       spacecraft is at the given
C                                       position
         PDLY = PDLY +
     *      2*COEFF(I)* LOG(   ((RS+R2+RS2)/(RS+R1+RS1)) *
     *                      ((RS+R1-RS1)/(RS+R2-RS2))  )
C                                       Delay for infinite position
C                                       of the spacecraft
         PDINF1 = PDINF1 +
     *      2*COEFF(I)* LOG(   ((RSIND+R2+RS2IND)/(RSIND+R1+RS1IND))
     *    * ((RSIND+R1-RS1IND)/(RSIND+R2-RS2IND))  )
C                                       vector R2-R1
         DR21X = ANTRX
         DR21Y = ANTRY
         DR21Z = ANTRZ
C                                       unit vector along SUN->ANT1
C                                       ANT1 is at the EARTH center
         R1X = -PLANX(I) / R1
         R1Y = -PLANY(I) / R1
         R1Z = -PLANZ(I) / R1
C                                       unit vector along SUN->SPC
         RSX = (SPX*CIND-PLANX(I)) / RSIND
         RSY = (SPY*CIND-PLANY(I)) / RSIND
         RSZ = (SPZ*CIND-PLANZ(I)) / RSIND
C                                       Delay for infinite position
C                                       of the spacecraft (simpler
C                                       equation)
         PDINF2 = PDINF2 -
     *      2*COEFF(I) * (DR21X*(R1X+RSX) + DR21Y*(R1Y+RSY) +
     *                     DR21Z*(R1Z+RSZ)) / R1 /
     *                     (1 + R1X*RSX + R1Y*RSY + R1Z*RSZ)
  140    CONTINUE
C------------------------TEST PRINT-------------------------
         IF (PARM(7).EQ.0) THEN
            IF (PARM(10).EQ.-10) THEN
               WRITE (MSGTXT,1075)
 1075          FORMAT ('IANT', 2X, 'AN', 2X, 'TIME', 7X, 'DEL,psec',
     *            2X, 'DINF1,ps',2X, 'DINF2,ps',2X, 'DINF2-DEL,ps')
               CALL MSGWRT (8)
               END IF
C
            ITEST = PARM(10)
            IF (MOD(ITEST,10).EQ.0) THEN
               WRITE (MSGTXT,1100) IANT, STNNAM(IANT)(1:2), FDDAY,
     *            PDLY*1.0E12,
     *            PDINF1*1.0E12, PDINF2*1.0E12, (PDINF2-PDLY)*1.0E12
 1100          FORMAT (I3, 3X, A2, 2X, F7.5, 5X, F7.2, 1X, F7.2, 3X,
     *             F7.2, 7X, F7.2)
               CALL MSGWRT (8)
C                                       print positions of the
C                                       spacecraft, sun, baseline
C                                       print the test data only for SUN
               IF (NPLANE.EQ.1) THEN
                  IF (PARM(8).GT.0) THEN
                     WRITE (MSGTXT,1110) PLANET(1)(1:3),
     *                  PLANX(1)/1000, PLANY(1)/1000, PLANZ(1)/1000
 1110                FORMAT (A3, ' X,Y,Z, KM  ', 3F12.0)
                     CALL MSGWRT (8)
C
                     WRITE (MSGTXT,1120) SPX/1000, SPY/1000, SPZ/1000
 1120                FORMAT ('SPA X,Y,Z, KM  ', 3F12.0)
                     CALL MSGWRT (8)
C
                     WRITE (MSGTXT,1130) ANTRX/1000, ANTRY/1000, ANTRZ
     *                  /1000
 1130                FORMAT ('ANT X,Y,Z, KM  ', 3F12.0)
                     CALL MSGWRT (8)
                     END IF
                  END IF
               END IF
            END IF
C
      PARM(10) = PARM(10) + 1
C                                       source ID at the CL table line
      THSOU = CLRECI(SOUCL)
C                                       previous source ID
      LSTSOU = PARM(1) + 0.5
      IF (LSTSOU.NE.THSOU) THEN
C
         PARM(1) = THSOU
         FREQS = FREQ + FREQO(BIF)
         END IF
C                                       calculate the actual space craft
C                                       position correction minus
C                                       infinite space craft
      IF (PARM(6).EQ.0) THEN
C                                       -PDINF2  has been added by
C                                       correlator
C                                       put -PDLY instead
         PDLY = PDINF2 - PDLY
      ELSE
         PDLY = -PDLY
         END IF
      IF (ANAME.EQ.'GMRT') PDLY = -PDLY
C                                       calculate the derivative of
C                                       the extra delay because of
C                                       the bending at the gravity field
C                                       of the Sun
C      DPDLAT = ??????????????
C
      IF (ISTOK.NE.2) THEN
         IF (CLRECR(MBD1CL).NE.FBLANK) CLRECR(MBD1CL) = CLRECR(MBD1CL)
     *      + PDLY
         DO 600 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE1CL+I-1)
            YT = CLRECR(IM1CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM1CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE1CL+I-1).NE.FBLANK)
     *         CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *         CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600        CONTINUE
         END IF
      IF (ABS(ISTOK).NE.1) THEN
         IF (CLRECR(MBD2CL).NE.FBLANK) CLRECR(MBD2CL) = CLRECR(MBD2CL)
     *      + PDLY
         DO 700 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM2CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE2CL+I-1).NE.FBLANK)
     *         CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *         CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
 700        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE UTPOL (IERR)
C-----------------------------------------------------------------------
C   Routine to correct for errors in UT1-UTC and the Earth pole position
C   Corrections are applied to the CL record in Common.
C   Control info from common:
C      PARM(*)  R    (1) = "UT1-UTC" correction (millisec)
C                    (2) = "X pole " correction (arcsec)
C                    (3) = "Y pole " correction (arcsec)
C      ISTOK    I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C      JD       D    Julian day of the OBSDAT
C
C      INDAY    D(*)       Day numbers
C      UT1C     D(*)       UT1-UTC values (CT tab), in sec
C      WOBX     D(*)       Polar wobble (CT tab) at X, in milliarcsec
C      WOBY     D(*)       Polar wobble (CT tab) at Y, in milliarcsec
C      TBEG     D(*)       Begin time of each group
C      TEND     D(*)       End time of each group
C      NATGRO   I(*)       Number of rows at the group
C      NACUMU   I(*)       Number of rows accumulated
C      NGROUP   I          Number of groups at the CT table
C      ICASE    I          1,2,3 depending on the CT table content
C      UT1TAI   D(*)       UT1-UTC values (USNO file), in sec
C      XYWOB    D(2,*)     Polar wobble (USNO file) in milliarcsec

C      BCOUNT   I    Number of days preceeded JD of observation
C      NCOUNT   I    Number of day used starting JD-BCOUNT
C   Output:
C      IERR     I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, I, ITEMP, IANT, KGROUP, INDEX
      REAL      XT, YT, CFAC, SFAC, FQFAC
C                                       for UT1UTC
      DOUBLE PRECISION UT1IFX(4), UT1PTX(20), TAIUTC(20), XJDTIM,
     *   UT1VAL, XYWOBX(2,20), WOBVAL(2), LEAPS0
C
      DOUBLE PRECISION CHAD, SHAD, HAD, DRA, DDEC
      DOUBLE PRECISION FREQS, X, Y, Z, DELAYC, RATEC,
     *   RADSEC, DELAY, RATE, PDLY, DPDLY,
     *   XR, YR, ZR, DTCOR, XPCOR, YPCOR
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
C                                       RADSEC = earth rot rate in
C                                       rad/sec.
      PARAMETER (RADSEC = PI / 43200.0D0)
      SAVE  FREQS, DRA, DDEC
      DATA LUN /30/
C-----------------------------------------------------------------------
      LSTSOU = PARM(7) + 0.5
      THSOU = CLRECI(SOUCL)
      IF (LSTSOU.NE.THSOU) THEN
C                                       Get source info
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
         PARM(7) = THSOU
         FREQS = FREQ + FREQO(BIF)
C                                       Get Greenwich hour angle
C                                       for VLBI
C                                       Source position; if apparent
C                                       position missing use mean
C                                       position.
         DRA = RAAPP
         DDEC = DECAPP
         IF ((DRA.LE.1.0D-5) .AND. (DDEC.LE.1.0D-5)) THEN
            DRA = RAEPO
            DDEC = DECEPO
            END IF
C                                       Declination
         SINDEC = SIN (DDEC)
         COSDEC = COS (DDEC)

         END IF
C
C                                       Get antenna number
      IANT = CLRECI(ANTCL)
C

C                                       Everything now RH no sign change
C                                       needed
      X = STNX(IANT)
      Y = STNY(IANT)
      Z = STNZ(IANT)
C
C                                       Inputs to the John's subroutine
C                                       to estimate UT1-UTC, XPOLE,
C                                       YPOLE, in sec, milli arcsec
C                                       Fix the first line = JD-1
C                                       and number of line = 5
C
      IF (ICASE.EQ.0) THEN
C                                       all CT table gropes are
C                                       identical, so the first group
C                                       is used
         UT1IFX(1) = JD + INDAY(1)
         UT1IFX(2) = 1.0
         UT1IFX(3) = 5
         UT1IFX(4) = 0
C                                       NCTROW =5 is tetermined in
C                                       CLCCOR before call CTTAB
         DO 10 I = 1, NCTROW
            UT1PTX(I) = UT1C(I)
            XYWOBX(1,I) = WOBX(I)
            XYWOBX(2,I) = WOBY(I)
            TAIUTC(I) = LEAPS(I)
   10       CONTINUE
      ELSE
C                                       CT table groups are not
C                                       identical; both
C                                       ICASE=1, and ICASE=2

         KGROUP = 1
         DO 20 I = 1, NGROUP
            IF (CLRECD(TIMCL).GE.TBEG(I) .AND.
     *         CLRECD(TIMCL).LT.TEND(I)) THEN
               KGROUP = I
               GO TO 30
               END IF
   20       CONTINUE
   30    CONTINUE
C
         UT1IFX(1) = JD + INDAY(NACUMU(KGROUP) + 1)
         UT1IFX(2) = 1.0
         UT1IFX(3) = 5
         UT1IFX(4) = 0
C                                       NCTROW =5 is determined in
C                                       CLCCOR before call CTTAB
         DO 40 I = 1, NCTROW
            INDEX = NACUMU(KGROUP) + I
            UT1PTX(I) = UT1C(INDEX)
            XYWOBX(1,I) = WOBX(INDEX)
            XYWOBX(2,I) = WOBY(INDEX)
            TAIUTC(I) = LEAPS(INDEX)
   40          CONTINUE
         END IF
C                                       store the leaps second of
C                                       the observation day
C                                       (it is the second one!!?)
      LEAPS0 = LEAPS(2)


C                                       TAI-UTC (leap sec) for the
C                                       observation time
C                                       TAIUTC = 0 because CT table
C                                       has UT1-UTC column already
C      TAIUTC = 0
C                                       time to interpolate to:
C                                       the CL table row time
      XJDTIM = JD + CLRECD(TIMCL)
C                                       John's subroutine to
C                                       estimate UT1-UTC
      CALL UT1COR (UT1IFX, UT1PTX, TAIUTC, XJDTIM, UT1VAL)
C                                       interpolated UT1-UTC as it
C                                       used in correlator, radians
C
C                                       UT1-UTC in days
      DTCOR = UT1VAL /3.6D3/24.D0
C
      CALL PINTER (UT1IFX, XYWOBX, XJDTIM, WOBVAL)
C                                       interpolated X pole wobble
C                                       as it used in correlator,
C                                       radians
      XPCOR = WOBVAL(1) / 1000.0 * AS2RAD
C                                       interpolated Y pole wobble
C                                       as it used in correlator,
C                                       radians
      YPCOR = WOBVAL(2) / 1000.0 * AS2RAD
C
      HAD = GSTIAT + (CLRECD(TIMCL)+ DTCOR)* ROTIAT
      IF (HAD.GT. PI) HAD = HAD - TWOPI
      IF (HAD.GT. PI) HAD = HAD - TWOPI
      IF (HAD.GT. PI) HAD = HAD - TWOPI
      IF (HAD.LT.-PI) HAD = HAD + TWOPI
      IF (HAD.LT.-PI) HAD = HAD + TWOPI
      CHAD = COS(HAD)
      SHAD = SIN(HAD)
C                                       correcte antenna coordinates
C                                       using the matrix Q=U*X*Y
C                                       JPL publication 83-39, Rev.6
C                                       by O.J. Sovers, C.S. Lacobs
      XR = X*CHAD - Y*SHAD + Z*(-SIN(XPCOR)*CHAD - SIN(YPCOR)*SHAD)
      YR = X*SHAD + Y*CHAD + Z*(-SIN(XPCOR)*SHAD + SIN(YPCOR)*CHAD)
      ZR = X*SIN(XPCOR) - Y*SIN(YPCOR) + Z
C                                       delay using appar coordinates
      DELAY = ((XR*COS(DRA) + YR*SIN(DRA))*COSDEC +
     *   ZR*SINDEC) / VELITE
C
C                                       Calculate RATE taking into
C                                       account only rotation of the
C                                       Earth (not changing of the
C                                       poles and UT1-UTC in time)
      RATE = RADSEC/VELITE *(
     *   ((-X * SHAD - Y * CHAD)*COS(DRA)) +
     *   ((+X * CHAD - Y * SHAD)*SIN(DRA)) )
     *   * COSDEC
      IF (ANAME.EQ.'GMRT') THEN
         DELAY = -DELAY
         RATE = -RATE
         END IF
C
C------Finish calculating DELAY, RATE used by VLBA correlator-------
C
C                                       Inputs to the John's subroutine
C                                       to estimate UT1-UTC, XPOLE,
C                                       YPOLE, in sec, milli arcsec
C                                       Inputs taken from USNO file
C                                       Default BCOUNT=1, NCOUNT=5
      UT1IFX(1) = JD - BCOUNT
      UT1IFX(2) = 1.0
      UT1IFX(3) = NCOUNT
      UT1IFX(4) = 0
C
      DO 90 I = 1, NCOUNT
         UT1PTX(I) = UT1TAI(I)
C                                       we are at the UT1-TAI.
C                                       So leap seconds are not required
         TAIUTC(I) = 0
         XYWOBX(1,I) = XYWOB(1,I)
         XYWOBX(2,I) = XYWOB(2,I)
   90    CONTINUE
C
C                                       John's subroutine to
C                                       estimate UT1-UTC
      CALL UT1COR (UT1IFX, UT1PTX, TAIUTC, XJDTIM, UT1VAL)
C                                       back to UT1-UTC
      UT1VAL = UT1VAL + LEAPS0
C                                       interpolated UT1-UTC as it
C                                       taken from USNO, in days
      DTCOR = UT1VAL /3.6D3/24.D0
C                                       John's subroutine to
C                                       estimate pole wobble
      CALL PINTER (UT1IFX, XYWOBX, XJDTIM, WOBVAL)

C                                       interpolated X,Y pole wobble
C                                       as it taken from USNO, radians
      XPCOR = WOBVAL(1) / 1000.0 * AS2RAD
      YPCOR = WOBVAL(2) / 1000.0 * AS2RAD
C
      HAD = GSTIAT + (CLRECD(TIMCL)+ DTCOR)* ROTIAT
      IF (HAD.GT. PI) HAD = HAD - TWOPI
      IF (HAD.GT. PI) HAD = HAD - TWOPI
      IF (HAD.GT. PI) HAD = HAD - TWOPI
      IF (HAD.LT.-PI) HAD = HAD + TWOPI
      IF (HAD.LT.-PI) HAD = HAD + TWOPI
      CHAD = COS(HAD)
      SHAD = SIN(HAD)
C                                       correcte antenna coordinates
C                                       using the matrix Q=U*X*Y
C                                       JPL publication 83-39, Rev.6
C                                       by O.J. Sovers, C.S. Lacobs
      XR = X*CHAD - Y*SHAD + Z*(-SIN(XPCOR)*CHAD - SIN(YPCOR)*SHAD)
      YR = X*SHAD + Y*CHAD + Z*(-SIN(XPCOR)*SHAD + SIN(YPCOR)*CHAD)
      ZR = X*SIN(XPCOR) - Y*SIN(YPCOR) + Z
C                                       calculate delay projecting
C                                       XR,YR, ZR on the W axis
C                                       use apparent coordinates of
C                                       the source
      DELAYC = ((XR*COS(DRA) + YR*SIN(DRA))*COSDEC +
     *   ZR*SINDEC) / VELITE
C                                       Calculate RATE taking into
C                                       account only rotation of the
C                                       Earth (not changing of the
C                                       poles and UT1-UTC in time)
      RATEC = RADSEC/VELITE *(
     *   ((-X * SHAD - Y * CHAD)*COS(DRA)) +
     *   ((+X * CHAD - Y * SHAD)*SIN(DRA)) )
     *   * COSDEC
      IF (ANAME.EQ.'GMRT') THEN
         DELAYC = -DELAYC
         RATEC = -RATEC
         END IF
C-------finish calculating corected values of DELAY, RATE-----------
C                                       correction of delay and rate
      PDLY = DELAYC - DELAY
      DPDLY = RATEC - RATE
C
      IF (ISTOK.NE.2) THEN
         IF (CLRECR(MBD1CL).NE.FBLANK) CLRECR(MBD1CL) = CLRECR(MBD1CL)
     *      + PDLY
         DO 600 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE1CL+I-1)
            YT = CLRECR(IM1CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE1CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM1CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE1CL+I-1).NE.FBLANK)
     *         CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *         CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600        CONTINUE
         END IF
      IF (ABS(ISTOK).NE.1) THEN
         IF (CLRECR(MBD2CL).NE.FBLANK) CLRECR(MBD2CL) = CLRECR(MBD2CL)
     *      + PDLY
         DO 700 I = BIF,EIF
            FQFAC = (FREQS+FRQOFF(I)) * PDLY
            ITEMP = FQFAC
            FQFAC = TWOPI * (FQFAC - ITEMP)
            CFAC = COS (FQFAC)
            SFAC = SIN (FQFAC)
C                                       Phase
            XT = CLRECR(RE2CL+I-1)
            YT = CLRECR(IM2CL+I-1)
            IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
               CLRECR(RE2CL+I-1) = XT * CFAC - YT * SFAC
               CLRECR(IM2CL+I-1) = XT * SFAC + YT * CFAC
               END IF
C                                       Delay
            IF (CLRECR(DE2CL+I-1).NE.FBLANK)
     *         CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
            IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *         CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
 700        CONTINUE
         END IF
C
 999  RETURN
      END
      REAL FUNCTION MDRY (ELV)
C-----------------------------------------------------------------------
C   Function used in ATMOV for a mapping function which takes
C   the wet and dry delay into account.  The formula is based on
C   Chao (1974) and recently published in Sovers, Fanselow and
C   Jacobs, 1998, Reviews of Modern Physics, 70, 1393.
C
C   Input    ELV   R   Elevation in radians
C-----------------------------------------------------------------------
      REAL      ELV, ADRY, BDRY
C-----------------------------------------------------------------------
      ADRY = 0.00143
      BDRY = 0.0445
      MDRY = 1.0E0 / (SIN(ELV) + ADRY / (TAN(ELV) + BDRY))
      RETURN
      END
      REAL FUNCTION MWET(ELV)
C-----------------------------------------------------------------------
C   Function used in ATMOV for a mapping function which takes
C   the wet and dry delay into account.  The formula is based on
C   Chao (1974) and recently published in Sovers, Fanselow and
C   Jacobs, 1998, Reviews of Modern Physics, 70, 1393.
C
C   Input    ELV   R   Elevation in radians
C-----------------------------------------------------------------------
      REAL ELV, AWET, BWET
      AWET = 0.00035
      BWET = 0.017
      MWET = 1.0E0 / (SIN(ELV) + AWET / (TAN(ELV) + BWET))
      RETURN
      END
      SUBROUTINE GETPRE (TIME, IANT, PRESS, IERR)
C-----------------------------------------------------------------------
C   GETPRE gets the linearly interpolated pressure at a station from the
C   WX table at a specific time
C   INPUTS: TIME   D     Time in CL table
C           IANT   I     Antenna number
C   OUTPUT: PRESS  R     Pressure
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DWXV.INC'
      DOUBLE PRECISION TIME, TIMEWX, PRETIM, POSTIM, FIRTIM, LASTIM
      REAL PRESS, DTIME, TEMP, DEWPT, WVEL, WDIR, WGUST, PRECIP, H2OCOL,
     *   IONCOL, PREPRE, POSPRE, FIRPRE, LASPRE, AVEPRE, TOTPRE
      INTEGER   WXBUFF(512), VER, LUNWX, IWXRNO, I, NROW,
     *   WXKOLS(MAXWXC), WXNUMV(MAXWXC), TABVER, IERR, IANT, ANT,
     *   SEQIN, SUBA, DISKIN, CNOIN, CLVER, JSUB, CLUSE
      CHARACTER OBSCOD*8, OBSDAT*8
      REAL XSIN, XDISIN, XFQID, XBAND, XFREQ, XBIF, XEIF, XTIME(8),
     *   XANT(50), XSUBA, XGVER, BPARM(20), XBAD(10), SELBAN, XGUSE
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSTOK, XOPCOD,
     *   XINFIL(12)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XXSTOK,
     *   XBAND, XFREQ, XFQID, XBIF, XEIF, XTIME, XANT, XSUBA, XGVER,
     *   XGUSE, XOPCOD, BPARM, XBAD, XINFIL, SELBAN, SEQIN, DISKIN,
     *   CNOIN, SUBA, CLVER, CLUSE
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNWX /30/
C-----------------------------------------------------------------------
C                                                    Open WX table
      CALL WXINI('READ', WXBUFF, DISKIN, CNOIN, VER, CATBLK, LUNWX,
     *   IWXRNO, WXKOLS, WXNUMV, OBSCOD, OBSDAT, TABVER, IERR)
      NROW = WXBUFF(5)
      IF (IERR.EQ.0) THEN
         PRETIM=1.D10
         POSTIM=1.D10
         PREPRE=1.D10
         POSPRE=1.D10
         AVEPRE=0.
         TOTPRE=0
         DO 100 I=2, NROW
C                                                   Read all lines
            CALL TABWX ('READ', WXBUFF, I-1, WXKOLS, WXNUMV, TIMEWX,
     *         DTIME, ANT, JSUB, TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST,
     *         PRECIP, H2OCOL, IONCOL, IERR)
            IF (IERR.EQ.0) THEN
               IF(ANT.EQ.IANT) THEN
                  AVEPRE=AVEPRE+PRESS
                  TOTPRE=TOTPRE+1
                  IF (TOTPRE.EQ.1) THEN
                     FIRPRE = PRESS
                     FIRTIM = TIMEWX
                  END IF
                  LASTIM = TIMEWX
                  LASPRE = PRESS
C                                                   Get pressure at times
C                                                   before and after TIME
                  IF (TIME.GT.TIMEWX) THEN
                     PRETIM = TIMEWX
                     PREPRE = PRESS
                  END IF
                  IF (TIME.LT.TIMEWX) THEN
                     POSTIM = TIMEWX
                     POSPRE = PRESS
                     GO TO 110
                  END IF
               END IF
            ELSE
              WRITE(MSGTXT,1000)
              CALL MSGWRT (8)
              WRITE(MSGTXT,1100)
              CALL MSGWRT (8)
              WRITE(MSGTXT,1200)
              CALL MSGWRT (8)
              GOTO 990
            END IF
 100     CONTINUE
C                                                  Calc average pressure
C                                                  to be used if TIME is more
C                                                  than an hour outside the
C                                                  timerange in the WX table.
 110     AVEPRE = AVEPRE / TOTPRE
         PRESS = AVEPRE
         IF (PRETIM.GT.9.D9) THEN
            IF ((FIRTIM - TIME).LT.0.0417) THEN
               PRESS = FIRPRE
            END IF
            GO TO 130
         END IF
         IF (POSTIM.GT.9.D9) THEN
            IF ((TIME - LASTIM).LT.0.0417) THEN
               PRESS = LASPRE
            END IF
            GO TO 130
         END IF
         PRESS = PREPRE + (TIME - PRETIM) * (POSPRE - PREPRE) /
     *      (POSTIM - PRETIM)
 130     CALL TABIO('CLOS',0,1,IWXRNO,WXBUFF,IERR)
      ELSE
         WRITE(MSGTXT,1300)
         CALL MSGWRT (8)
         WRITE(MSGTXT,1100)
         CALL MSGWRT (8)
         WRITE(MSGTXT,1200)
         CALL MSGWRT (8)
         END IF
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETPRE: ERROR READING WX TABLE, WILL NOT USE PRESSURE')
 1100 FORMAT ('TO ESTIMATE THE DRY DELAY, THIS MEANS THE MAPPING')
 1200 FORMAT ('FUNCTION AT LOW ELEVATIONS WILL BE BAD')
 1300 FORMAT ('GETPRE: ERROR OPENING WX TABLE, WILL NOT USE PRESSURE')
      END
      DOUBLE PRECISION FUNCTION STNHT(IANT)
C-----------------------------------------------------------------------
C     STNHT returns the station height of an antenna
C     Inputs: IANT   I    Antenna number
C     Output: STNHT  D    Station height in meters
C-----------------------------------------------------------------------
      INTEGER IANT
      DOUBLE PRECISION  B, R, E, F, P, Q, D, NU, G, T
      DOUBLE PRECISION  X, Y, Z
      DOUBLE PRECISION  SMAXIS, FLATEN
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'INCS:DANS.INC'
      PARAMETER         (SMAXIS=6378137.D0)
      PARAMETER         (FLATEN=1.D0/298.257223563D0)
C-----------------------------------------------------------------------
      X = STNX(IANT)
      Y = STNY(IANT)
      Z = STNZ(IANT)
      R = SQRT( X**2 + Y**2 )
C
C           Reasonable range.
C
      B = DSIGN (SMAXIS * (1.D0 - FLATEN ), Z)
      E = (B * Z - (SMAXIS**2 - B**2)) / (SMAXIS * R)
      F = (B * Z + (SMAXIS**2 - B**2)) / (SMAXIS * R)
      P = (4.0D0 / 3.0D0) * (E * F + 1.D0)
      Q = 2.D0 * (E**2 - F**2)
      D = P**3 + Q**2
      IF( D.LT.0.D0) THEN
         NU = 2.D0 * SQRT (-P) * COS ((1.D0/3.D0) *
     *              ACOS( Q / (P * SQRT (-P))))
      ELSE
         NU = (SQRT (D) - Q )**(1.D0/3.D0) -
     *              (SQRT (D) + Q )**(1.D0/3.D0)
         END IF
C
C           Deal with cases near singularities as per Almanac.
C           The criteria are a wild guess.
C
      IF (Z.LT.SMAXIS / 1.D5 .OR. R.LT.SMAXIS / 1.D5) THEN
         NU = (-1.D0) * (NU**3 + 2.D0 * Q) / (3.D0 * P)
      END IF
      G = 0.5D0 * (SQRT (E**2 + NU) + E)
      T = SQRT (G**2 + (F - NU * G) / (2.D0 * G - E)) - G
      STNHT = (R - SMAXIS * T) * COS (STNLAT(IANT)) +
     *           (Z - B) * SIN (STNLAT(IANT))
      RETURN
      END
      SUBROUTINE SUN (YEAR, DAY, FDAY, SUNCV)
C-----------------------------------------------------------------------
C   Calculate three coordinates of the Sun and its velocity at
C   the equatorial system of coordinates originating at the Earth
C   center.
C   Acuracy for the time interval 1950-2050:
C                      10000 km in position
C                      0.004 km/s in speed
C
C   Inputs:
C      YEAR      I       Year, for example 1996
C      DAY       I       Day in year (1 = Jan 1st)
C      FDAY      R       Fraction of day
C
C   Outputs:
C      SUNCV     R(6)    Sun position & velocity vector
C                        Position part, SUNCV(1-3), is in km;
C                        Velocity part, SUNCV(4-6), is in km/sec.
C-----------------------------------------------------------------------
      REAL FDAY, SUNCV(6)
      INTEGER YEAR, DAY
C
      INCLUDE 'INCS:PSTD.INC'
      INTEGER YEAR4
      REAL SPEED, REMB, SEMB, Y1900, YF, T, ELM, GAMMA, EM, ELT, EPS0,
     *   ECCEN, ESQ, V, R, ELMM, COSELT, SINEPS, COSEPS, W1, W2, SELMM,
     *   CELMM, AU
C                                       Astronomical unit, km
      DATA AU /1.495979E8/
C-----------------------------------------------------------------------
      REMB = 3.12E-5
      SEMB = 8.31E-11
C                                       Mean orbital speed of Earth,
C                                       AU/s
      SPEED = 1.9913E-7
      Y1900 = YEAR-1900.0
      YEAR4 = MOD (MOD (YEAR, 4) +4, 4)
      YF=(FLOAT( 4 * (DAY - 1/(YEAR4 + 1)) - YEAR4 - 2) + 4.0 * FDAY)
     *   / 1461.0
      T=Y1900 + YF
C                                       Geometric mean longitude of Sun
      ELM = DMOD (4.881628 + TWOPI * YF + 0.00013420 * T, TWOPI)
C                                       Mean longitude of perihelion
      GAMMA = 4.908230 + 3.0005E - 4 * T
C                                       Mean anomaly
      EM = ELM - GAMMA
C                                       Mean obliquity
      EPS0 = 0.40931975 - 2.27E-6 * T
C                                       Eccentricity
      ECCEN = 0.016751 - 4.2E-7 * T
      ESQ = ECCEN * ECCEN
C                                       True anomaly
      V = EM + 2.0 * ECCEN * SIN(EM) + 1.25 * ESQ * SIN (2.0 * EM)

C                                       True ecliptic longitude
      ELT = V + GAMMA
C                                       True distance
      R = (1.0 - ESQ) / (1.0 + ECCEN * COS(V))

C                                       Moon's mean longitude
      ELMM = DMOD (4.72D0 + 83.9971D0 * T, TWOPI)
C
      COSELT = COS (ELT)
      SINEPS = SIN (EPS0)
      COSEPS = COS(EPS0)
      W1 = -R * SIN (ELT)
      W2 = -SPEED * (COSELT + ECCEN * COS (GAMMA))
      SELMM = SIN (ELMM)
      CELMM = COS (ELMM)
C                                       Sun position and velocity
      SUNCV(1) = (R * COSELT + REMB * CELMM) * AU
      SUNCV(2) = -(W1 - REMB * SELMM) * COSEPS * AU
      SUNCV(3) = -W1 * SINEPS * AU
      SUNCV(4) = -(SPEED * (SIN(ELT) + ECCEN * SIN(GAMMA)) +
     *   SEMB * SELMM) * AU
      SUNCV(5) = -(W2 - SEMB * CELMM) * COSEPS * AU
      SUNCV(6) = -W2 * SINEPS * AU
C
 999  RETURN
      END
      SUBROUTINE SUNJPL (YEAR, MONTH, DAY, FDAY, NAME, LINUN, SUNCV)
C-----------------------------------------------------------------------
C   Calculate three coordinates of the Sun and its velocity at
C   the equatorial system of coordinates originating at the Earth
C   center based on the JPL (B. Butler) software.
C   Works for the time interval 1950-2020:
C                      10000 km in position
C                      0.004 km/s in speed
C
C   Inputs:
C      YEAR      I       Year, for example 1996
C      MONTH     I       Month of the year
C      DAY       I       Day of the month
C      FDAY      D       Fraction of day
C      NAME      C       Name of the planet
C      LINUN     L       LINUX/UNIX/ computer
C
C   Outputs:
C      SUNCV     D(6)    Sun position & velocity vector
C                        Position part, SUNCV(1-3), is in km;
C                        Velocity part, SUNCV(4-6), is in km/sec.
C-----------------------------------------------------------------------
      DOUBLE PRECISION  FDAY, SUNCV(6)
      INTEGER YEAR, MONTH, DAY
C
      INCLUDE 'INCS:PSTD.INC'
C

      INTEGER IERR
      LOGICAL  TR
      LOGICAL   LINUN
      CHARACTER   NAME*12, FILEIN*48, PATHIN*256
      DOUBLE PRECISION MJD, XX, YY, DX, DY, ZZ, DZ
      DOUBLE PRECISION AU
C                                       Astronomical unit, meters
      DATA AU /1.4959787066D11/
      DATA TR /.TRUE./
C-----------------------------------------------------------------------
C                                       convert Gregorian Calendar date
C                                       to Modified Julian Date
      CALL SLCLDJ (YEAR, MONTH, DAY, MJD, IERR)
C
      MJD = MJD + FDAY
C                                       calculate the planet position
C                                       without time shift for
C                                       the delay Planet-Earth
C
C                                       LINUX/UNIX
      IF (LINUN) THEN
         FILEIN = 'AIPSTARS:JPLEPH.405.2.LINUX'
      ELSE
         FILEIN = 'AIPSTARS:JPLEPH.405.2.SUNOS'
         END IF
      CALL ZFULLN (FILEIN, '-1', ' ', PATHIN, IERR)
Ctemporally comment the first call jpleph
C
C      CALL JPLEPH (FILEIN, NAME, MJD, 0.0D0,
C     *             0.0D0, 0.0D0, 'J2000', 'GEO', XX, YY, DX, DY, ZZ,
C     *             DZ, FL, IERR)
Ctemporally
C      DIST = ZZ * AU
C                                       add the time of the light
C                                       traveling from Planet to Earth
C                                       to get agreement with John Bens.
Ctemporally do not add the propogation time
C      MJD = MJD + AU/VELITE/86400.D0
Ctemporaly commenr DIST
C      MJD = MJD + DIST/VELITE/86400.D0
C      MJD = MJD + 0.0038
C
C                                       calculate X,Y,Z and their rates
C                                       for the time of emmission
      CALL JPLEPH (PATHIN, NAME, MJD, 0.0D0,
     *             0.0D0, 0.0D0, 'J2000', 'GEO', XX, YY, DX, DY, ZZ,
     *             DZ, TR, IERR)
C                                       convert to km and km/sec
      SUNCV(1) = XX * (AU / 1000.0D0)
      SUNCV(2) = YY * (AU / 1000.0D0)
      SUNCV(3) = ZZ * (AU / 1000.0D0)
      SUNCV(4) = DX * (AU / 1000.0D0) / 86400.0D0
      SUNCV(5) = DY * (AU / 1000.0D0) / 86400.0D0
      SUNCV(6) = DZ * (AU / 1000.0D0) / 86400.0D0
C
 999  RETURN
      END
C
      SUBROUTINE SLCLDJ (IY, IM, ID, DJM, J)
*+
*     - - - - -
*      C L D J
*     - - - - -
*
*  Gregorian Calendar to Modified Julian Date
*
*  Given:
*     IY,IM,ID     int    year, month, day in Gregorian calendar
*
*  Returned:
*     DJM          dp     modified Julian Date (JD-2400000.5) for 0 hrs
*     J            int    status:
*                           0 = OK
*                           1 = bad year   (MJD not computed)
*                           2 = bad month  (MJD not computed)
*                           3 = bad day    (MJD computed)
*
*  The year must be -4699 (i.e. 4700BC) or later.
*
*  The algorithm is derived from that of Hatcher 1984
*  (QJRAS 25, 53-55).
*
*  P.T.Wallace   Starlink   December 1985
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

C IMPLICIT NONE

      INTEGER IY,IM,ID
      DOUBLE PRECISION DJM
      INTEGER J

*  Month lengths in days
      INTEGER MTAB(12)
      DATA MTAB/31,28,31,30,31,30,31,31,30,31,30,31/



*  Preset status
      J=0

*  Validate year
      IF (IY.LT.-4699) THEN
         J=1
      ELSE

*     Validate month
         IF (IM.GE.1.AND.IM.LE.12) THEN

*        Allow for leap year
            IF (MOD(IY,4).EQ.0) THEN
               MTAB(2)=29
            ELSE
               MTAB(2)=28
            END IF
            IF (MOD(IY,100).EQ.0.AND.MOD(IY,400).NE.0)
     :         MTAB(2)=28

*        Validate day
            IF (ID.LT.1.OR.ID.GT.MTAB(IM)) J=3

*        Modified Julian Date
               DJM=DBLE((1461*(IY-(12-IM)/10+4712))/4
     :                  +(306*MOD(IM+9,12)+5)/10
     :                  -(3*((IY-(12-IM)/10+4900)/100))/4
     :                  +ID-2399904)

*        Bad month
         ELSE
            J=2
         END IF

      END IF

      END
C
      SUBROUTINE SPACES (IERR)
C-----------------------------------------------------------------------
C   Routine to read positions of the space craft from the OB table
C
C   Control info from common:
C      NLINES    I    number of elements at the following arrays
C      TMES(*)   D    array of measurement times, in days
C      SPACEX(*) R    array of the spacecraft X, in meters
C      SPACEY(*) R    array of the spacecraft Y, in meters
C      SPACEZ(*) R    array of the spacecraft Z, in meters
C   Output:
C      IERR      I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
      INCLUDE 'INCS:POBV.INC'
      INTEGER OBVER, IOBRNO, LUN, OBKOLS(MAXOBC), OBNUMV(MAXOBC),
     *   IANTOB, ISUBOB, IORB, KORB, NUMREC, INVER, BUFFAN(512)
      INTEGER BUFFOB(512)
C
      REAL      ANGLOB(3), ECLPOB(4), ORIOB
      DOUBLE PRECISION DXYZOB(3), DVELOB(3), DTIMOB, TIMBEG, TIMEND
      INCLUDE 'CLCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
C      INCLUDE 'INCS:DSOU.INC'
C      INCLUDE 'INCS:DDCH.INC'
C      INCLUDE 'INCS:DUVH.INC'
C      INCLUDE 'INCS:DHDR.INC'
      DATA LUN /30/
C-----------------------------------------------------------------------
C                                       Open  AN table to get GSTIA0
      INVER = 1
      CALL ANTINI ('READ', BUFFAN, DISKIN, CNOIN, INVER, CATBLK, LUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *   RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, NUMORB,
     *   NOPCAL, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000)
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                      Close AN extension files
      CALL TABIO ('CLOS', INVER, IANRNO, BUFFAN, BUFFAN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1100)
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IERR = 0
C                                       Timerange
      TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
C
      OBVER = 1
C                                       Open OB table
      CALL OBINI ('READ', BUFFOB, DISKIN, CNOIN, OBVER, CATBLK, LUN,
     *   IOBRNO, OBKOLS, OBNUMV, IERR)
      IF (IERR.NE.0) GO TO 999
C

C                                       Get number of records
      NUMREC = BUFFOB(5)
      IF (NUMREC.LE.0) GO TO 999
      KORB = 0
      DO 500 IORB = 1, NUMREC
C                                       read OB table
         IOBRNO = IORB
         CALL TABOB ('READ', BUFFOB, IOBRNO, OBKOLS, OBNUMV,
     *   IANTOB, ISUBOB, DTIMOB, DXYZOB, DVELOB, ANGLOB, ECLPOB,
     *   ORIOB, IERR)

C                                       check if record is flagged
         IF (IERR.LT.0) GO TO 500
C                                       timerange selection
         IF (DTIMOB.LT.TIMBEG) GO TO 500
         IF (DTIMOB.GT.TIMEND) GO TO 520
         KORB = KORB + 1
         TMES(KORB) = DTIMOB
         SPACEX(KORB) = DXYZOB(1)
         SPACEY(KORB) = DXYZOB(2)
         SPACEZ(KORB) = DXYZOB(3)
  500    CONTINUE
  520 CONTINUE
      NLINES = KORB
C                                       Close input OB table
      CALL TABIO ('CLOS', 0, IOBRNO, BUFFOB, BUFFOB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) OBVER
         CALL MSGWRT (6)
         GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR: Missing AN table!')
 1100 FORMAT ('SPACES: ERROR',I3,' CLOSING AN FILE')
 1200 FORMAT ('ERROR closing OB table', I3)
      END
      SUBROUTINE UT1COR (UT1IFX, UT1PTX, TAIUTC, XJDTIM, UT1VAL)
C-----------------------------------------------------------------------
C  UT1COR interpolates UT1-UTC values using code fragments taken
C  from the GSFS program CALC, version 9.1. The interpolated values
C  are intended to be identical to what the CALC program produces
C  internally.
C  In particular this subroutine can be used to reproduce the
C  calculations carried out by the VLBA correlator.
C
C  J. Benson Aug 2005
C
C  Modified by L. Kogan Feb 2006: add the array of the leap second
C  with possible jump at the interpolation interval to go to smooth
C  UT1-TAI, interpolate it and then return back the interpolated
C  UT1-TAI to UT1-UTC subtracting TAIUTC(2) which is supposed to be
C  the leap second at the observation time.
C
C  Inputs:
C  UT1IFX(4)  D     The UT1 information array.
C                   1. The Julian day of the first tabular point
C                   2. The increment of the tabular points (expected 1),
C                      in days
C                   3. The number of tabular points (5 for VLBA correlator)
C                   4. is not used
C
C  UT1PTX(20) D     The tabular values of 'UT1 - UTC', in sec
C  TAIUTC(20) D     Leap second values, for example equal 32 for 2005
C                   TAIUTC(2) Leap second values for the observation date,
C  XJDTIM     D     Time to interpolate on, in JD
C
C  Output:
C  UT1VAL     D     Interpolated value, in sec
C-----------------------------------------------------------------------
      DOUBLE PRECISION UT1IFX(4), UT1PTX(20), TAIUTC(20), XJDTIM,
     *   UT1VAL
C
      INTEGER   KUT1C, NSPLIN, SHRTFL
      LOGICAL   USESPL
      DOUBLE PRECISION UT1IF(4), UT1PT(20), UT1RS(20), XT(20), YA(20),
     *   Y2S(20), XSPLIN, YSPLIN, YDOT, YDOT2, YDOT3, YP1, YPN,
     *   CT, XJD,  FA(5), FAD(5), TC2000,  ATMUT1, SHORTP, DUT, DLOD,
     *   DOMEGA, SECDAY
      INTEGER   ITAB, II, TABLEN, IERR4, I
C-----------------------------------------------------------------------
      KUT1C   = 0
      SHRTFL = 1
      USESPL = .TRUE.
      SECDAY  = 86400.0
      DO 10 I = 1,4
         UT1IF(I) = UT1IFX(I)
 10      CONTINUE
      TABLEN = UT1IF(3) + 0.01
      DO 15 I = 1,TABLEN
C         UT1PT(i) = TAIUTC - UT1PTX(i)
C                                       change LK
C                                       add the leap second possibly
C                                       with jump to convert to smooth
C                                       UT1-TAI
         UT1PT(I) = TAIUTC(I) - UT1PTX(I)
 15      CONTINUE
C
C  Logic for TAI-UT1S tables (default)
      IF (KUT1C.EQ.0 .OR. KUT1C.EQ.3 .OR. KUT1C.EQ.2) THEN
C
         DO 20 ITAB = 1, TABLEN
C                                       !Already UT1S
            IF(SHRTFL .EQ. -2) THEN
               UT1RS(ITAB) = UT1PT(ITAB)
            ELSE
C                                       ! Fraction of day
               CT  = 0.0D0
               XJD = UT1IF(1) + (ITAB-1)*UT1IF(2)
               CALL NUTFA  (XJD, CT, TC2000, FA, FAD)
               CALL UT1SZT (FA, FAD, DUT, DLOD, DOMEGA)
C
C          DUT = 0.0
C                                       !starting with true UT1
               IF (SHRTFL.EQ.1) THEN
                  UT1RS(ITAB) = UT1PT(ITAB) + DUT
C            write (6,'(1x, f10.6, f10.6)') UT1PT(Itab),UT1RS(Itab)
                  END IF
               END IF
 20         CONTINUE
C
C     Place the UT1 module flow control message into the database.
C     UT1 module on, Tidal terms from UT1S model
C          apply_tidal_correction = .true.
C          Call PUTA('UT1 CFLG      ',LUT1S,40,1,1)
C
         END IF
C ***************
C   Code for spline interpolation initialization, 93DEC08  -DG-
C                                       ! Initialize spline routine
      IF (USESPL) THEN
         NSPLIN = TABLEN
C
         DO 25 II=1,NSPLIN
            YA(II) = UT1RS(II)
 25         CONTINUE
C
         DO 30 II = 1,NSPLIN
            XT(II) =  UT1IF(1) + (II-1)*UT1IF(2)
C        write (6, '(1x, f12.2, d16.8)') XT(ii), ya(ii)
 30         CONTINUE
C
C   If interval (UT1IF(2)) not 1.0 days, then divide by interval ?????
         IF (ABS(UT1IF(2) - 1.D0) .GT. 1.D-10) THEN
            DO 40 II = 1,NSPLIN
               XT(II) = XT(II) / UT1IF(2)
 40            CONTINUE
            END IF
C
C   Take first derivatives at endpoints
         YP1 = (YA(2)-YA(1)) / UT1IF(2)
         YPN = (YA(NSPLIN)-YA(NSPLIN-1))/ UT1IF(2)
C
C  call spline initialization subroutine
         CALL SPLINE (XT, YA, NSPLIN, YP1, YPN, Y2S, IERR4)
C                                       ! Initialize spline routine
         END IF
C ***************
C  Begin interpolation:
C
C***************************************
C                                       !Cubic spline interpolation
      IF (USESPL) THEN
C   Compute time of obs. and divide by interval
         XSPLIN = XJDTIM
C        write (6, '(1x, f12.2)') x_spline
C
C   Do the spline interpolation
         CALL SPLIN4 (XT, YA, Y2S, NSPLIN, XSPLIN, YSPLIN, YDOT,
     *      YDOT2, YDOT3, IERR4)
C                                        fraction of day
         CT  = 0.0D0
         XJD = XJDTIM
         CALL NUTFA  (XJD, CT, TC2000, FA, FAD)
         CALL UT1SZT (FA, FAD, DUT, DLOD, DOMEGA)
         SHORTP = -DUT
         ATMUT1 = YSPLIN + SHORTP
C        DIVUTC = ydot / (UT1IF(2) * SECDAY) + SHORTP_DOT
C                                       change LK
C                                       add the leap second of the
C                                       observe time to return back
C                                       to UT1-UTC
C        UT1VAL = TAIUTC - ATMUT1
         UT1VAL = TAIUTC(2) - ATMUT1
C                                       !Cubic spline interpolation
         END IF
C
      END
      SUBROUTINE PINTER (WOBIFX, XYWOBX, XJDTIM, WOBVAL)
C-----------------------------------------------------------------------
C  PINTER interpolates xpol, ypol values using code fragments taken
C  from the GSFS program CALC, version 9.1. The interpolated values
C  are intended to be identical to what the CALC program produces
C  internally.
C  In particular this subroutine can be used to reproduce the
C  calculations carried out by the VLBA correlator.
C
C  J. Benson Aug 2005
C
C  Inputs:
C  WOBIFX(4)    D    The wobble information array.
C                    1. The Julian day of the first tabular point
C                    2. The increment of the tabular points (expected 1),
C                      in days
C                    3. The number of tabular points
C                      (5 for VLBA correlator)
C                    4. not used
C
C  XYWOBX(2,20) D     The wobble tabular points for the polar motion
C                     (wobble) X & Y offsets. (milliarcsec)
C                     (Note: Based on old BIH conventions, offsets
C                           are assumed to be left-handed.)
C  XJDTIM       D     Time to interpolate on, in JD
C
C  Output:
C  WOBVAL(2)    D     Interpolated value, in milliarcsec
C-----------------------------------------------------------------------
      DOUBLE PRECISION WOBIFX(4), XYWOBX(2,20), XJDTIM, WOBVAL(2)
C
      INTEGER   NSPLIN
      LOGICAL   USESPL
      DOUBLE PRECISION XSPLIN, YP1X, YPNX, YP1Y, YPNY, YYSPLN,
     *   YYDOT, YYDOT2, YYDOT3, YXSPLN, YXDOT, YXDOT2, YXDOT3, SECDAY
      INTEGER  II, TABLEN, IERR4, I
      DOUBLE PRECISION WOBIF(3), XYWOB(2,20), XA(20), YAX(20), YAY(20),
     *   Y2SX(20), Y2SY(20)
C-----------------------------------------------------------------------
      SECDAY = 86400.0
      USESPL = .TRUE.

      DO 10 I = 1, 3
         WOBIF(I) = WOBIFX(I)
 10      CONTINUE
      TABLEN   = WOBIF(3) + 0.01
      DO 20 I = 1, TABLEN
         XYWOB(1,I) = XYWOBX(1,I)
         XYWOB(2,I) = XYWOBX(2,I)
 20      CONTINUE

C ***************
C   Code for spline interpolation initialization, 93DEC08  -DG-
C                                       ! INITIALIZE spline routine
      IF (USESPL) THEN
         NSPLIN = TABLEN
C
         DO 30 II = 1,NSPLIN
            YAX(II) = XYWOB(1,II)
            YAY(II) = XYWOB(2,II)
 30         CONTINUE
C
         XA(1) = WOBIF(1)
         DO 40 II = 2,NSPLIN
            XA(II) = XA(II-1) + WOBIF(2)
 40         CONTINUE
C
C   If interval (WOBIF(2)) not 1.0 days, then divide by interval
         IF (ABS(WOBIF(2) - 1.D0) .GT. 1.D-10) THEN
            DO 50 II = 1,NSPLIN
               XA(II) = XA(II) / WOBIF(2)
 50            CONTINUE
            END IF
C
C   Take first derivatives at endpoints for X-wobble
         YP1X = (YAX(2)-YAX(1)) / (XA(2)-XA(1))
         YPNX = (YAX(NSPLIN)-YAX(NSPLIN-1))/
     .      (XA(NSPLIN)-XA(NSPLIN-1))
C  call spline initialization subroutine for X-wobble
         CALL SPLINE (XA,YAX, NSPLIN, YP1X, YPNX, Y2SX, IERR4)
C  Take first derivatives at endpoints for Y-wobble
         YP1Y = (YAY(2)-YAY(1)) / (XA(2)-XA(1))
         YPNY = (YAY(NSPLIN)-YAY(NSPLIN-1))/
     .      (XA(NSPLIN)-XA(NSPLIN-1))
C  call spline initialization subroutine for Y-wobble
         CALL SPLINE (XA, YAY, NSPLIN, YP1Y, YPNY, Y2SY, IERR4)
C                                       ! Initialize spline routine
         END IF
C ***************
C  Begin interpolation:
C
C***************************************
C                                       !Cubic spline interpolation
      IF (USESPL) THEN
C   Compute time of obs. and divide by interval
         XSPLIN = XJDTIM
C        write (6, '(1x, f12.2)') x_spline
C
C  Perform cubic spline for X-wobble interpolation
         CALL SPLIN4 (XA, YAX, Y2SX, NSPLIN, XSPLIN, YXSPLN,
     *      YXDOT, YXDOT2, YXDOT3, IERR4)
         WOBVAL(1) = YXSPLN
C
C  Perform cubic spline for Y-wobble interpolation
         CALL SPLIN4 (XA, YAY, Y2SY, NSPLIN, XSPLIN, YYSPLN,
     *      YYDOT, YYDOT2, YYDOT3, IERR4)
         WOBVAL(2) = YYSPLN
C                                       !Cubic spline interpolation
         END IF
C
      END
C***********************************************************************
      SUBROUTINE NUTFA (XJD, CT, CENT, FA, FAD)
C-----------------------------------------------------------------------
C   NUTFA computes the number of Julian centuries since J2000 and the
C   fundamental arguments and derivatives to be used in the nutation
C   series.
C
C   References: D.McCarthy, IERS Technical Note 13, 'IERS Conventions
C                          (1992)', Paris 1992
C              T.C. van Flandern, Lunar Occult. Work (fundam.argum.)
C              D.McCarthy, IERS Technical Note 21, 'IERS Conventions
C                          (1996)', Paris 1996
C
C   Calling sequence:
C   input:
C      1. XJD  -  The Julian date at zero hours UTC of the date in
C                      question. (days)
C      2. CT   -  The coordinate time fraction of the coordinate time
C                      day. (days)
C   output:
C      1. CENT -  The number of Julian centuries elapsed since the
C                      epoch January 1.5 2000.(centuries)
C      2. FA(5)-  The fundamental arguments for the nutation theory.
C                      (arcseconds)
C               1 = mean anomaly of the moon
C                 = mean longitude of the moon minus the
C                   mean longitude of the moon's perigee     (l)
C               2 = mean anomaly of the sun
C                 = mean longitude of the sun minus the
C                   mean longitude of the sun's perigee      (l')
C               3 = mean longitude of the moon minus omega   (F)
C               4 = mean elongation of the moon from the sun (D)
C               5 = longitude of the asc.node of the moon's
C                   mean orbit on the ecliptic,
C                   measured from the mean equinox of date   (omega)
C      3. FAD(5)- The CT time derivatives of the fundamental arguments.
C                      (arcsec/century)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XJD, CT, CENT, FA(5), FAD(5), EL, ELP, F, D, OM,
     *   ELC(5), ELPC(5), FC(5), DC(5), OMC(5), CENT2, CENT3, CENT4,
     *   DAYSJ, SEC360
C
      DOUBLE PRECISION CENTJ, DJ2000, EC(4), ARGP(2,6)
      INTEGER   NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C     Variables from:
C        1. centj   -  the number of coordinate time days per Julian century.
C                      (days/century)
C        2. dj2000  -  the Julian date of the epoch January 1.5, 2000. (days)
C
      EXTERNAL NUTCMB
C      INCLUDE 'ccon.i'
      INTEGER   KNUTD
C     Variables from:
C        1. KNUTD  -  nutation module debug flag
C
C     Subroutine interface:
C       Caller subroutines: DRIVR, UT1I
C       Called subroutines: DMOD
C
C     Constants used -
C       ELC(5)   - COEFFICIENTS USED IN THE CALCULATION OF EL
C       ELPC(5)  - COEFFICIENTS USED IN THE CALCULATION OF ELP
C       FC(5)    - COEFFICIENTS USED IN THE CALCULATION OF F
C       DC(5)    - COEFFICIENTS USED IN THE CALCULATION OF D
C       OMC(5)   - COEFFICIENTS USED IN THE CALCULATION OF OM
C
C     DATA statements for the fundamental arguments.
C     Simons et al., 1994 values
C      -Conform to IERS Conventions (1996)-
      DATA ELC    / -0.00024470D0,       0.051635D0,  31.8792D0,
     .          1717915923.2178D0,  485868.249036D0/
      DATA ELPC   / -0.00001149D0,      -0.000136D0,  -0.5532D0,
     .           129596581.0481D0,  1287104.79305D0/
      DATA FC     /  0.00000417D0,      -0.001037D0, -12.7512D0,
     .          1739527262.8478D0,  335779.526232D0/
      DATA DC     / -0.00003169D0,       0.006593D0,  -6.3706D0,
     .          1602961601.2090D0,  1072260.70369D0/
      DATA OMC    /-0.00005939D0,        0.007702D0,   7.4722D0,
     .           -6962890.2665D0,   450160.398036D0/
C                                       ! arcseconds in one turn
      DATA SEC360 / 1296000.0D0 /
C
C  Programmer:
C    93.09.01  Norbert Zacharias - Fundamental arguments computation put into
C              separate subroutine, taken from old NUTG subroutine.
C    98.01.28  David Gordon - Coefficients and computations modified to conform
C              to IERS Conventions (1996).
C
C-------------------------------------------------------------------------------
      KNUTD = 0

C  Compute the number of Julian days elapsed since the epoch January 1.5, 2000.
      DAYSJ = XJD + CT - DJ2000
C
C  Compute the number of Julian centuries elapsed since the epoch January 1.5,
C   2000.
      CENT  = DAYSJ / CENTJ
      CENT2 = CENT * CENT
      CENT3 = CENT * CENT2
      CENT4 = CENT2 * CENT2
C
C  Computation of the fundamental arguments and derivatives
C
      EL = ELC(1)*CENT4 + ELC(2)*CENT3 + ELC(3)*CENT2
     .   + ELC(4)*CENT  + ELC(5)
      FA (1) = DMOD( EL, SEC360 )
      FAD(1) = 4.D0*ELC(1)*CENT3 + 3.D0*ELC(2)*CENT2
     .       + 2.D0*ELC(3)*CENT  +      ELC(4)
C
      ELP = ELPC(1)*CENT4 + ELPC(2)*CENT3 + ELPC(3)*CENT2
     .    + ELPC(4)*CENT  + ELPC(5)
      FA (2) = DMOD( ELP, SEC360 )
      FAD(2) = 4.D0*ELPC(1)*CENT3 + 3.D0*ELPC(2)*CENT2
     .       + 2.D0*ELPC(3)*CENT  +      ELPC(4)
C
      F = FC(1)*CENT4 + FC(2)*CENT3 + FC(3)*CENT2
     .  + FC(4)*CENT  + FC(5)
      FA (3) = DMOD( F, SEC360 )
      FAD(3) = 4.D0*FC(1)*CENT3 + 3.D0*FC(2)*CENT2
     .       + 2.D0*FC(3)*CENT  +      FC(4)
C
      D = DC(1)*CENT4 + DC(2)*CENT3 + DC(3)*CENT2
     .  + DC(4)*CENT  + DC(5)
      FA (4) = DMOD( D, SEC360 )
      FAD(4) = 4.D0*DC(1)*CENT3 + 3.D0*DC(2)*CENT2
     .       + 2.D0*DC(3)*CENT  +      DC(4)
C
      OM = OMC(1)*CENT4 + OMC(2)*CENT3 + OMC(3)*CENT2
     .   + OMC(4)*CENT  + OMC(5)
      FA (5) = DMOD( OM, SEC360 )
      FAD(5) = 4.D0*OMC(1)*CENT3 + 3.D0*OMC(2)*CENT2
     .       + 2.D0*OMC(3)*CENT  +      OMC(4)
C-------------------------------------------------------------------------------
C  Debug output
      IF (KNUTD.NE.0) THEN
        WRITE (6,'(1X,A)') 'Debug output for subroutine NUTFA'
        WRITE (6,'(1X,A,D25.16)') 'CENT  = ', CENT
    8         FORMAT(A,4D25.16/(7X,5D25.16))
        WRITE(6,8)' ELC     ',ELC
        WRITE(6,8)' EL      ',FA (1)
        WRITE(6,8)' ELD     ',FAD(1)
        WRITE(6,8)' ELPC    ',ELPC
        WRITE(6,8)' ELP     ',FA (2)
        WRITE(6,8)' ELPD    ',FAD(2)
        WRITE(6,8)' FC      ',FC
        WRITE(6,8)' F       ',FA (3)
        WRITE(6,8)' FD      ',FAD(3)
        WRITE(6,8)' DC      ',DC
        WRITE(6,8)' D       ',FA (4)
        WRITE(6,8)' DD      ',FAD(4)
        WRITE(6,8)' OMC     ',OMC
        WRITE(6,8)' OM      ',FA (5)
        WRITE(6,8)' OMD     ',FAD(5)
      END IF
C
      END
C***********************************************************************
      SUBROUTINE UT1SZT (FA, FAD, DUT, DLOD, DOMEGA)
C-----------------------------------------------------------------------
C   Purpose: This subroutine evaluates the effects of zonal Earth tides
C   on the rotation of the Earth. The model used is from Yoder,
C   Williams, and Park (1981) and modified by the ocean effects as
C   given in Dickman (1991) as recommended by the IERS Standards,
C    p. 117, 119-120 (1992).
C
C   Special Note: Under the UT1S definition, and as done by this
C   routine, zonal tides of _all_ periods are evaluated, including even
C   those of 18.6 year period. Results will be substantially different
C   (tenths of seconds) from those evaluated with the "UT1R" algorithm,
C   which only includes the 41 terms for periods under 35 days. If you
C   wish to determine the effects from only those periods, please use
C   the original Luzum "zontids" routine, with N set to 41.  (B.A.)
C
C   Input Variables:
C      FA(5)  = Fundamental arguments from subroutine NUTFA (arcseconds)
C      FAD(5) = Time derivatives of fundamental arguments
C              (arcsec/century)
C   Output Variables:
C      DUT    - 'UT1 minus UT1S'. Effect on UT (Subtract from
C                observation,    add to IERS UT1). (sec)
C      DLOD   = Effect on length of day. (seconds).
C      DOMEGA = Effect on rotational speed (radians/second).
C
C     Written by:
C       Brian J. Luzum     92.08.11
C     Modifications:
C       Brent A. Archinal  92.08.27  Name changed from zontids to
C                                    ut1szt, N dropped from argument
C                                    list, comments improved.
C       "     "  "         92.10.27  Special note added above.
C       "     "  "         92.12.17  All real variables set to double
C                                    precision, at Jim Ray's suggestion
C                                    (email of 92.11.20).
C       David Gordon       93.03.17  Array X changed to XS, debug printout
C                                    added for calc 8.0.
C       Norbert Zacharias  93.09.16  Take fundam. arg. from subr. NUTFA
C       David Gordon       94.04.06  Changed to 'Implicit None'.
C       David Gordon       98.01.21  Extensive mods from John Gipson to use
C                                    the series expansion of DUT to calculate
C                                    DUT, DLOD, and DOMEGA instead of seperate
C                                    series expansions. fad(5) added to
C                                    subroutine call. DBLE's removed. CMATH
C                                    common block added. Variable SECCON
C                                    removed and replaced with 1/CONVDS. Sign
C                                    of X(7,62) corrected.
C
      DOUBLE PRECISION DUT, DLOD, DOMEGA, F, D, OM, ARG
      DOUBLE PRECISION L, LP, FA(5), FAD(5), ARGDOT
      INTEGER   N, I
      DOUBLE PRECISION XS(11,62), X1(220), X2(220), X3(220), X4(22)
      EQUIVALENCE (XS(1,  1), X1(1))
      EQUIVALENCE (XS(1, 21), X2(1))
      EQUIVALENCE (XS(1, 41), X3(1))
      EQUIVALENCE (XS(1, 61), X4(1))
C
C      INCLUDE 'ccon.i'
      INTEGER   KNUTD, KUT1D
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION TWOPI, CONVDS, SECDAY
C     DOUBLE PRECISION PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
C     COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
C     DATA PI     /3.1415926535897932D0/,
      DATA TWOPI  /6.2831853071795865D0/
C     DATA HALFPI /1.5707963267948966D0/
C     DATA CONVD  /1.7453292519943296D-02/
      DATA CONVDS /4.8481368110953599D-06/
C     DATA CONVHS /7.2722052166430399D-05/
      DATA SECDAY /8.6400D04/

C          Variables 'from':
C            1. SECDAY -  The number of seconds in a day. (s/day)
C            2. TWOPI  -  PI times 2.0D0
C            3. CONVDS -  THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
C                 (RAD/ARCSECOND)
C
C  N=Number of tidal terms to be used (62 for full set).
C  (This was an argument in the original zontids routine.)
C
      DATA  N /62/
C
C***********************************************************************
C     Table of multiples of arguments and coefficients
C      (DLOD and DOMEGA tables no longer used, 98JAN21 -DG-)
C
C                  Multiple of            DUT        DLOD      DOMEGA
C             l   l'  F   D OMEGA     sin   cos    cos  sin   cos   sin
      DATA X1/1., 0., 2., 2., 2.,    -0.02, 0.00,  0.3, 0.0, -0.2,  0.0,
     /        2., 0., 2., 0., 1.,    -0.04, 0.00,  0.4, 0.0, -0.3,  0.0,
     /        2., 0., 2., 0., 2.,    -0.10, 0.00,  0.9, 0.0, -0.8,  0.0,
     /        0., 0., 2., 2., 1.,    -0.05, 0.00,  0.4, 0.0, -0.4,  0.0,
     /        0., 0., 2., 2., 2.,    -0.12, 0.00,  1.1, 0.0, -0.9,  0.0,
     /        1., 0., 2., 0., 0.,    -0.04, 0.00,  0.3, 0.0, -0.2,  0.0,
     /        1., 0., 2., 0., 1.,    -0.40, 0.01,  2.7, 0.1, -2.3, -0.1,
     /        1., 0., 2., 0., 2.,    -0.98, 0.03,  6.7, 0.2, -5.7, -0.2,
     /        3., 0., 0., 0., 0.,    -0.02, 0.00,  0.1, 0.0, -0.1,  0.0,
     /       -1., 0., 2., 2., 1.,    -0.08, 0.00,  0.5, 0.0, -0.5,  0.0,
     /       -1., 0., 2., 2., 2.,    -0.20, 0.00,  1.3, 0.0, -1.1,  0.0,
     /        1., 0., 0., 2., 0.,    -0.08, 0.00,  0.5, 0.0, -0.4,  0.0,
     /        2., 0., 2.,-2., 2.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        0., 1., 2., 0., 2.,     0.03, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        0., 0., 2., 0., 0.,    -0.30, 0.00,  1.4, 0.0, -1.2,  0.0,
     /        0., 0., 2., 0., 1.,    -3.20, 0.09, 14.7, 0.4,-12.4, -0.4,
     /        0., 0., 2., 0., 2.,    -7.73, 0.21, 35.6, 1.0,-30.0, -0.8,
     /        2., 0., 0., 0.,-1.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        2., 0., 0., 0., 0.,    -0.34, 0.00,  1.5, 0.0, -1.3,  0.0,
     /        2., 0., 0., 0., 1.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0/
      DATA X2/0.,-1., 2., 0., 2.,    -0.02, 0.00,  0.1, 0.0, -0.1,  0.0,
     /        0., 0., 0., 2.,-1.,     0.05, 0.00, -0.2, 0.0,  0.2,  0.0,
     /        0., 0., 0., 2., 0.,    -0.72, 0.02,  3.1, 0.1, -2.6, -0.1,
     /        0., 0., 0., 2., 1.,    -0.05, 0.00,  0.2, 0.0, -0.2,  0.0,
     /        0.,-1., 0., 2., 0.,    -0.05, 0.00,  0.2, 0.0, -0.2,  0.0,
     /        1., 0., 2.,-2., 1.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        1., 0., 2.,-2., 2.,     0.10, 0.00, -0.3, 0.0,  0.2,  0.0,
     /        1., 1., 0., 0., 0.,     0.04, 0.00, -0.1, 0.0,  0.1,  0.0,
     /       -1., 0., 2., 0., 0.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0,
     /       -1., 0., 2., 0., 1.,     0.18, 0.00, -0.4, 0.0,  0.3,  0.0,
     /       -1., 0., 2., 0., 2.,     0.44, 0.00, -1.0, 0.0,  0.9,  0.0,
     /        1., 0., 0., 0.,-1.,     0.53, 0.00, -1.2, 0.0,  1.0,  0.0,
     /        1., 0., 0., 0., 0.,    -8.33, 0.12, 19.0, 0.3,-16.0, -0.2,
     /        1., 0., 0., 0., 1.,     0.54, 0.00, -1.2, 0.0,  1.0,  0.0,
     /        0., 0., 0., 1., 0.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        1.,-1., 0., 0., 0.,    -0.06, 0.00,  0.1, 0.0, -0.1,  0.0,
     /       -1., 0., 0., 2.,-1.,     0.12, 0.00, -0.2, 0.0,  0.2,  0.0,
     /       -1., 0., 0., 2., 0.,    -1.84, 0.02,  3.6, 0.0, -3.0,  0.0,
     /       -1., 0., 0., 2., 1.,     0.13, 0.00, -0.3, 0.0,  0.2,  0.0,
     /        1., 0.,-2., 2.,-1.,     0.02, 0.00,  0.0, 0.0,  0.0,  0.0/
      DATA X3/-1.,-1.,0., 2., 0.,    -0.09, 0.00,  0.2, 0.0, -0.1,  0.0,
     /        0., 2., 2.,-2., 2.,    -0.06, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        0., 1., 2.,-2., 1.,     0.03, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        0., 1., 2.,-2., 2.,    -1.88, 0.00,  1.0, 0.0, -0.8,  0.0,
     /        0., 0., 2.,-2., 0.,     0.25, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        0., 0., 2.,-2., 1.,     1.17, 0.00, -0.4, 0.0,  0.3,  0.0,
     /        0., 0., 2.,-2., 2.,   -48.84, 0.11, 16.8, 0.0,-14.2,  0.0,
     /        0., 2., 0., 0., 0.,    -0.19, 0.00,  0.1, 0.0, -0.1,  0.0,
     /        2., 0., 0.,-2.,-1.,     0.05, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        2., 0., 0.,-2., 0.,    -0.55, 0.00,  0.2, 0.0, -0.1,  0.0,
     /        2., 0., 0.,-2., 1.,     0.04, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        0.,-1., 2.,-2., 1.,    -0.05, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        0., 1., 0., 0.,-1.,     0.09, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        0.,-1., 2.,-2., 2.,     0.83, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        0., 1., 0., 0., 0.,   -15.55, 0.02,  2.6, 0.0, -2.2,  0.0,
     /        0., 1., 0., 0., 1.,    -0.14, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        1., 0., 0.,-1., 0.,     0.03, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        2., 0.,-2., 0., 0.,    -0.14, 0.00,  0.0, 0.0,  0.0,  0.0,
     /       -2., 0., 2., 0., 1.,     0.42, 0.00,  0.0, 0.0,  0.0,  0.0,
     /       -1., 1., 0., 1., 0.,     0.04, 0.00,  0.0, 0.0,  0.0,  0.0/
      DATA X4/0., 0., 0., 0., 2.,     7.90, 0.00,  0.1, 0.0, -0.1,  0.0,
     /        0., 0., 0., 0., 1., -1637.68,-0.10,-10.4, 0.0,  8.8,  0.0/
C       X4(18) corrected, old value +0.10, new value -0.10
C    /        0., 0., 0., 0., 1., -1637.68, 0.10,-10.4, 0.0,  8.8,  0.0/
C
C***********************************************************************
C     Fundamental arguments from subroutine NUTFA in file cnutm.f:
C
C     L = fa (1)
C     LP= fa (2)
C     F = fa (3)
C     D = fa (4)
C     OM= fa (5)
C
      KNUTD  = 0
      KUT1D = 0

      DUT    = 0.0D+0
      DLOD   = 0.0D+0
      DOMEGA = 0.0D+0
C
C     Sum zonal tide terms
C
      DO 10 I = 1,N
C   Formation of multiples of arguments
         ARG = XS(1,I)*FA(1) + XS(2,I)*FA(2) + XS(3,I)*FA(3)
     *      + XS(4,I)*FA(4) + XS(5,I)*FA   (5)
         ARG = DMOD(ARG,1296000.0D0) * CONVDS
C   First derivative
         ARGDOT = XS(1,I)*FAD(1)  + XS(2,I)*FAD(2) + XS(3,I)*FAD(3)
     *      + XS(4,I)*FAD(4) + XS(5,I)*FAD(5)
C     Evaluate zonal tidal terms
         DUT    = XS(6,I)*DSIN(ARG) + XS(7,I)*DCOS(ARG) + DUT
         DLOD   = (XS(6,I)*DCOS(ARG) - XS(7,I)*DSIN(ARG) )*ARGDOT +
     *      DLOD
   10    CONTINUE
      DUT    = DUT    * 1.0D-4
      DLOD   = -DLOD * 1.0D-4 / (3.6525D+4 / CONVDS)
      DOMEGA = -DLOD * TWOPI / SECDAY**2
C
C     Check for debug output.
      IF (KUT1D.EQ.1) THEN
         MSGTXT = 'Debug output for subroutine UT1SZT'
         CALL MSGWRT (4)
         WRITE (MSGTXT,1000) L, LP
         CALL MSGWRT (4)
         WRITE (MSGTXT,1001) F, D
         CALL MSGWRT (4)
         WRITE (MSGTXT,1002) OM, ARG
         CALL MSGWRT (4)
         WRITE (MSGTXT,1003) DUT, DLOD
         CALL MSGWRT (4)
         WRITE (MSGTXT,1004) DOMEGA
         CALL MSGWRT (4)
         END IF
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('L = ',1PD19.12,5X,'LP =',1PD19.12)
 1001 FORMAT ('F = ',1PD19.12,5X,'D =',1PD19.12)
 1002 FORMAT ('OM = ',1PD19.12,5X,'ARG =',1PD19.12)
 1003 FORMAT ('DUT = ',1PD19.12,5X,'DLOD =',1PD19.12)
 1004 FORMAT ('DOMEGA = ',1PD19.12)
      END
C***********************************************************************
      SUBROUTINE SPLINE (XA, YA, N, YP1, YPN, Y2, IERR4)
C-----------------------------------------------------------------------
C   Subroutine spline is the initialization section of a two subroutine
C   module used to do cubic spline interpolations. Spline is called each
C   time you change to a new set of tabular points. Subroutine splin4
C   is the second half and does the actual interpolation.
C
C   Given arrays xa(i), and ya(i) for i=1 to n containing a tabulated
C     function ya(i) = f(xa(i)) with xa(1) < xa(2) <..< xa(n) and values
C     yp1 and ypn whichc are the first derivatives of the function f( ) at
C     points 1 and n, respectively, this routine returns an array
C     y2(i) for i=1 to n which contains the second derivatives of the
C     function f( ) at the tabulated points xa(i). If yp1 and/or ypn are
C     greater than or equal to 1.d30, the routine is signaled to set the
C     corresponding boundary condition for a natural spline, with zero
C     second derivatives on the boundary.
C
C     The cubic spline has the following properties.
C     1) It is continuous and exactly fits at the tabular values.
C     2) The first and second derivatives are everywhere continuous,
C        even at the tabular points when points are added to and
C        deleted from the set of five points used to interpolate.
C     3) The third derivative is not continuous, but rather a series
C        of disconnected constant values.
C     4) The fourth derivative is zero.
C     5) It requires a minimum of five points and values of the first
C        derivative at the two endpoints.
C
C     In order to guarantee that this routine is only used in the modes in
C     which it has been tested, we require that the values of xa be exactly
C     one unit apart.
C
C  References:  'Numerical Recipes in FORTRAN, 2nd Edition' page 109-110.
C
C  Calling sequence -
C
C     Input Variables:
C       1. xa(n) - Array of tabular (time) values corresponding to the ya
C                  array. The xa's must be evenly spaced.
C       2. ya(n) - Array of EOP values at the times corresponding to those
C                  in the xa array. i.e. ya(i) = f(xa(i)).
C       3. n     - Number of points in the xa and ya arrays.
C       4. yp1   - First derivative of the EOP function at the point i=1.
C       5. ypn   - First derivative of the EOP function at the point i=n.
C
C     Output variables:
C       1. y2(n) - Array containing the second derivatives of the
C                   interpolating function at the tabular points xa(i).
C       2. ierr4 - Error return code (0=good, 1=bad)
C
      INTEGER   N, NMAX, IERR4
      DOUBLE PRECISION  YP1, YPN, XA(20), YA(20), Y2(20)
      PARAMETER (NMAX=25)
      INTEGER   I, K
      DOUBLE PRECISION P, QN, SIG, UN, U(NMAX)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C  Program variables:
C       1. NMAX    - The largest anticipated value of n.
C       2. p       -
C       3. qn      -
C       4. sig     -
C       5. un      -
C       6. u(NMAX) -
C
C  Programmer:
C     93.11.22  Jim Ryan     - Initial coding and modification for Calc.
C     93.12.07  David Gordon - Make variables same as in subroutine splin4,
C                              Calc-like documentation inserted.
C
C  SPLINE program structure:
C
C  Verify that the values of xa are one unit apart.
      DO 10 I = 2,N
        IF(ABS(XA(I)-XA(I-1)-1.D0) .GT. 1.D-8) THEN
          MSGTXT = 'spline: independent variable NOT one unit apart!'
          CALL MSGWRT (8)
          WRITE (MSGTXT,1000) I, XA(I), XA(I-1),(XA(I)-XA(I-1))
 1000 FORMAT ('SPLINE:I,XA(I),XA(I-1),DIFF',I5,3(1PD16.9))
          CALL MSGWRT (8)
          IERR4 = 1
          RETURN
        ELSE
          IERR4 = 0
          ENDIF
 10     CONTINUE
C
C  Set lower boundary condition to be "natural" or else to have a specified
C  first derivative.
      IF (YP1 .GT. .99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(XA(2)-XA(1)))*((YA(2)-YA(1))/(XA(2)-XA(1))-YP1)
      ENDIF
C
C  Decomposition loop of the tridiagonal algorithm. y2 and u used for temporary
C  storage of the decomposed factors.
      DO 11 I=2,N-1
        SIG=(XA(I)-XA(I-1))/(XA(I+1)-XA(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((YA(I+1)-YA(I))/(XA(I+1)-XA(I))-
     *       (YA(I)-YA(I-1))/(XA(I)-XA(I-1)))/(XA(I+1)-XA(I-1))-
     *       SIG*U(I-1))/P
  11  CONTINUE
C
C  Set upper boundary condition to be "natural" or else to have a specified
C  first derivative.
      IF (YPN .GT. .99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(XA(N)-XA(N-1)))*(YPN-(YA(N)-YA(N-1))/(XA(N)-XA(N-1)))
      ENDIF
C
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
C
C  Backsubstitution loop of the tridiagonal algorithm.
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
  12  CONTINUE
C
      RETURN
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *1(a)031..
C
C***********************************************************************
      SUBROUTINE SPLIN4 (XA, YA, Y2, N, X, Y, YDOT, YDOT2, YDOT3,
     *   IERR4)
C-----------------------------------------------------------------------
C     Given the arrays xa(i) and ya(i) for i = 1 to n, which tabulate
C     a function with the xa(i)'s in time order, and given the array
C     y2(i), which is the previous output of subroutine spline, and
C     given a value of x, this routine returns the cubic spline
C     interpolated value of y and its first three derivatives.
C
C  References: 'Numerical Recipes in FORTRAN, 2nd Edition' page 109-110.
C
C  Calling sequence -
C
C     Input Variables:
C       1. xa(n) - Array of tabular (time) values corresponding to the ya
C                  array. The xa's must be evenly spaced.
C       2. ya(n) - Array of EOP values at the times corresponding to those
C                  in the xa array. i.e. ya(i) = f(xa(i)).
C       3. y2(n) - Array containing the second derivatives of the
C                  interpolating function at the tabular points xa(i).
C       4. n     - Number of points in the xa and ya arrays.
C       5. x     - Input time for which the corresponding interpolated value
C                  of y (= f(x)) is to be determined.
C
C     Output variables:
C       1. y     - The interpolated value.
C       2. ydot  - The 1st derivative of the interpolated value.
C       3. ydot2 - The 2nd derivative of the interpolated value.
C       4. ydot3 - The 3rd derivative of the interpolated value.
C       5. ierr4 - Error return code (0=good, 1=bad)
C-----------------------------------------------------------------------
      INTEGER   N, IERR4
      DOUBLE PRECISION X, Y, XA(20), Y2(20), YA(20), YDOT, YDOT2, YDOT3
      INTEGER   K, KHI, KLO
      DOUBLE PRECISION A, B, H, ADOT, BDOT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C  Program variables - klo, khi, k, h, a, b, adot, bdot
C
C  Programmer:
C     93.11.22  Jim Ryan     - Initial coding and modification for Calc.
C                              First, second, and third derivatives added.
C     93.12.07  David Gordon - Make variables same as in subroutine spline,
C                              Calc-like documentation inserted.
C
C  SPLIN4 program structure:
C
C  Find right place in table by means of bisection. Optimized for sequential
C   calls being at random values of x.
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K) .GT. X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GO TO 1
      ENDIF
C
C  Make sure the xa's aren't the same values.
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) THEN
        MSGTXT = 'BAD XA INPUT IN SPLIN4'
        CALL MSGWRT (8)
        IERR4 = 1
      ELSE
        IERR4 = 0
      ENDIF
C
C  Evaluate cubic spline polynomial
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+((A**3-A)*Y2(KLO)+(B**3-B)*Y2(KHI))*
     *  (H**2)/6.
C
C  First derivative
      ADOT = -1.D0/H
      BDOT =  1.D0/H
      YDOT = ADOT*YA(KLO) + BDOT*YA(KHI)
     .       + ((3.D0*A**2-1.D0)*ADOT*Y2(KLO)
     .       + (3.D0*B**2-1.D0)*BDOT*Y2(KHI)) * (H**2/6.D0)
C
C  Second derivative.
      YDOT2 = (A*(ADOT**2)*Y2(KLO) + B*(BDOT**2)*Y2(KHI)) * (H**2)
C
C  Third derivative.
      YDOT3 = ((ADOT**3)*Y2(KLO) + (BDOT**3)*Y2(KHI)) * (H**2)
C
      RETURN
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *1(a)031..
C******************************************************************************
      BLOCK DATA NUTCMB
C
C 7.    NUTBD
C
C 7.1   NUTBD PROGRAM SPECIFICATION
C
C 7.1.1 NUTBD IS THE NUTATION MODULE BLOCK DATA INITIALIZATION SECTION.
C       THE NUTATION SERIES IS ESTABLISHED HERE. THIS VERSION CONTAINS
C       THE 1980 IAU THEORY OF NUTATION, FROM THE WORK OF J. M. WAHR,
C       SPECIFICALLY, THE WAHR NUTATION SERIES FOR AXIS B OF GILBERT &
C       DZIEWONSKI EARTH MODEL 1066A.
C
C 7.1.3 REFERENCES - 1) 'THE EXPLANATORY SUPPLEMENT TO THE AMERICAN
C                    EPHEMERIS AND NAUTICAL ALMANAC", P. 41-45, 98
C
C                    2) LIESKE, J.H., ET AL., EXPRESSIONS FOR THE
C                    PRECESSIONAL QUANTITIES BASED ON THE IAU (1976)
C                    SYSTEM OF ASTRONOMICAL CONSTANTS,
C                    ASTRON. ASTROPHYS. 58, 1-16, 1977.
C
C                    3) SEIDELMANN, P. K., 1980 IAU THEORY OF NUTATION:
C                    THE FINAL REPORT OF THE IAU WORKING GROUP ON
C                    NUTATION, CELEST. MECH. 27, PP. 79-106 (1982).
C
C                    4) WAHR, J. M., THE FORCED NUTATIONS OF ... EARTH,
C                    GEOPHYS. J. ROY. ASTR. SOC. 64, PP. 705-727 (1981).
C
C 7.2   NUTBD PROGRAM INTERFACE
C
C 7.2.1 CALLING SEQUENCE - NONE
C
C 7.2.2 COMMON BLOCK -
C
      DOUBLE PRECISION X(9,120)
      COMMON / XWAHR / X
C
      DOUBLE PRECISION CENTJ, DJ2000, EC(4), ARGP(2,6)
      INTEGER   NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C           VARIABLES 'TO':
C              1. CENTJ   - THE NUMBER OF COORDINATE TIME DAYS PER JULIAN
C                           CENTURY. (DAYS/CENTURY) (CENTJ = 36525.D0)
C              2. DJ2000  - THE JULIAN DATE OF THE EPOCH JANUARY 1.5, 2000.
C                           (DAYS) (DJ2000 = 2451545.D0)
C              3. EC(4)   - THE CONSTANTS APPEARING IN TERMS 1-4 IN THE
C                           CALCULATION OF THE MEAN OBLIQUITY OF THE ECLIPTIC.
C                           (ARCSEC) (SEE REFERENCES)
C                           (EC(1) = +8.4381448D4, EC(2) = -46.815D0,
C                             EC(3) = -5.9D-4, EC(4) = +1.813D-3 )
C              4. NOT     - THE NUMBER OF TERMS IN THE NUTATION SERIES.
C                           (NOT = 106)
C              5. X(9,120)- THE ARRAY CONTAINING THE NUTATION SERIES.
C                           (X = 1980 IAU THEORY OF NUTATION)
C              6. NOP     - THE NUMBER OF NUTATION TERMS DESIGNATED FOR WHICH
C                           PARTIALS ARE TO BE COMPUTED. (NOP = 6)
C                                  (Obsolete?)
C              7. IDP(6)  - IDENTIFICATION NUMBERS (TERM NUMBERS) OF DESIGNATED
C                           NUTATION TERMS FOR WHICH PARTIALS ARE TO BE COMPUTED
C                           (IDP(1) =  1, IDP(2) =  2, IDP(3) =  3,
C                             IDP(4) =  4, IDP(5) =  5, IDP(6) =  7 )
C              8. ARGP(2,6)-ARGUMENTS (COMBINATIONS OF FUNDAMENTAL ARGUMENTS)
C                           AND THEIR DERIVATIVES OF DESIGNATED NUTATION TERMS
C                           FOR WHICH PARTIALS ARE TO BE COMPUTED.
C                           (COMPUTED IN NUTW. SET TO 0.0D0 HERE)
C                                  (Obsolete?)
C
C 7.2.3 PROGRAM SPECIFICATIONS -
      DOUBLE PRECISION X1(180), X2(180), X3(180), X4(180), X5(180),
     *   X6(180)
      EQUIVALENCE (X(1,  1),X1(1))
      EQUIVALENCE (X(1, 21),X2(1))
      EQUIVALENCE (X(1, 41),X3(1))
      EQUIVALENCE (X(1, 61),X4(1))
      EQUIVALENCE (X(1, 81),X5(1))
      EQUIVALENCE (X(1,101),X6(1))
C
      DATA  CENTJ  / 36525.D0 /,
     1      DJ2000 / 2451545.D0 /,
     2      EC     / 8.4381448D4, -46.8150D0, -5.9D-4, 1.813D-3 /,
     3      NOT    / 106 /,
     4      NOP    / 6 /,
     5      IDP    / 1, 2, 3, 4, 5, 7 /,
     6      ARGP   / 12 * 0.0D0 /
C***********************************************************************
C
C               1980 IAU THEORY OF NUTATION (WAHR THEORY)
C           TABLE OF MULTIPLES OF ARGUMENTS AND COEFFICIENTS
C
C                   MULTIPLE OF            LONGITUDE        OBLIQUITY
C              L    L'   F    D  OMEGA   COEFF. OF SIN    COEFF. OF COS
      DATA X1/ 0.,  0.,  0.,  0.,  1., -171996., -174.2,  92025.,  8.9,
     /         0.,  0.,  2., -2.,  2.,  -13187.,   -1.6,   5736., -3.1,
     /         0.,  0.,  2.,  0.,  2.,   -2274.,   -0.2,    977., -0.5,
     /         0.,  0.,  0.,  0.,  2.,    2062.,    0.2,   -895.,  0.5,
     /         0.,  1.,  0.,  0.,  0.,    1426.,   -3.4,     54., -0.1,
     /         1.,  0.,  0.,  0.,  0.,     712.,    0.1,     -7.,  0.0,
     /         0.,  1.,  2., -2.,  2.,    -517.,    1.2,    224., -0.6,
     /         0.,  0.,  2.,  0.,  1.,    -386.,   -0.4,    200.,  0.0,
     /         1.,  0.,  2.,  0.,  2.,    -301.,    0.0,    129., -0.1,
     /         0., -1.,  2., -2.,  2.,     217.,   -0.5,    -95.,  0.3,
     /         1.,  0.,  0., -2.,  0.,    -158.,    0.0,     -1.,  0.0,
     /         0.,  0.,  2., -2.,  1.,     129.,    0.1,    -70.,  0.0,
     /        -1.,  0.,  2.,  0.,  2.,     123.,    0.0,    -53.,  0.0,
     /         1.,  0.,  0.,  0.,  1.,      63.,    0.1,    -33.,  0.0,
     /         0.,  0.,  0.,  2.,  0.,      63.,    0.0,     -2.,  0.0,
     /        -1.,  0.,  2.,  2.,  2.,     -59.,    0.0,     26.,  0.0,
     /        -1.,  0.,  0.,  0.,  1.,     -58.,   -0.1,     32.,  0.0,
     /         1.,  0.,  2.,  0.,  1.,     -51.,    0.0,     27.,  0.0,
     /         2.,  0.,  0., -2.,  0.,      48.,    0.0,      1.,  0.0,
     /        -2.,  0.,  2.,  0.,  1.,      46.,    0.0,    -24.,  0.0/
      DATA X2/ 0.,  0.,  2.,  2.,  2.,     -38.,    0.0,     16.,  0.0,
     /         2.,  0.,  2.,  0.,  2.,     -31.,    0.0,     13.,  0.0,
     /         2.,  0.,  0.,  0.,  0.,      29.,    0.0,     -1.,  0.0,
     /         1.,  0.,  2., -2.,  2.,      29.,    0.0,    -12.,  0.0,
     /         0.,  0.,  2.,  0.,  0.,      26.,    0.0,     -1.,  0.0,
     /         0.,  0.,  2., -2.,  0.,     -22.,    0.0,      0.,  0.0,
     /        -1.,  0.,  2.,  0.,  1.,      21.,    0.0,    -10.,  0.0,
     /         0.,  2.,  0.,  0.,  0.,      17.,   -0.1,      0.,  0.0,
     /         0.,  2.,  2., -2.,  2.,     -16.,    0.1,      7.,  0.0,
     /        -1.,  0.,  0.,  2.,  1.,      16.,    0.0,     -8.,  0.0,
     /         0.,  1.,  0.,  0.,  1.,     -15.,    0.0,      9.,  0.0,
     /         1.,  0.,  0., -2.,  1.,     -13.,    0.0,      7.,  0.0,
     /         0., -1.,  0.,  0.,  1.,     -12.,    0.0,      6.,  0.0,
     /         2.,  0., -2.,  0.,  0.,      11.,    0.0,      0.,  0.0,
     /        -1.,  0.,  2.,  2.,  1.,     -10.,    0.0,      5.,  0.0,
     /         1.,  0.,  2.,  2.,  2.,      -8.,    0.0,      3.,  0.0,
     /         0., -1.,  2.,  0.,  2.,      -7.,    0.0,      3.,  0.0,
     /         0.,  0.,  2.,  2.,  1.,      -7.,    0.0,      3.,  0.0,
     /         1.,  1.,  0., -2.,  0.,      -7.,    0.0,      0.,  0.0,
     /         0.,  1.,  2.,  0.,  2.,       7.,    0.0,     -3.,  0.0/
      DATA X3/-2.,  0.,  0.,  2.,  1.,      -6.,    0.0,      3.,  0.0,
     /         0.,  0.,  0.,  2.,  1.,      -6.,    0.0,      3.,  0.0,
     /         2.,  0.,  2., -2.,  2.,       6.,    0.0,     -3.,  0.0,
     /         1.,  0.,  0.,  2.,  0.,       6.,    0.0,      0.,  0.0,
     /         1.,  0.,  2., -2.,  1.,       6.,    0.0,     -3.,  0.0,
     /         0.,  0.,  0., -2.,  1.,      -5.,    0.0,      3.,  0.0,
     /         0., -1.,  2., -2.,  1.,      -5.,    0.0,      3.,  0.0,
     /         2.,  0.,  2.,  0.,  1.,      -5.,    0.0,      3.,  0.0,
     /         1., -1.,  0.,  0.,  0.,       5.,    0.0,      0.,  0.0,
     /         1.,  0.,  0., -1.,  0.,      -4.,    0.0,      0.,  0.0,
     /         0.,  0.,  0.,  1.,  0.,      -4.,    0.0,      0.,  0.0,
     /         0.,  1.,  0., -2.,  0.,      -4.,    0.0,      0.,  0.0,
     /         1.,  0., -2.,  0.,  0.,       4.,    0.0,      0.,  0.0,
     /         2.,  0.,  0., -2.,  1.,       4.,    0.0,     -2.,  0.0,
     /         0.,  1.,  2., -2.,  1.,       4.,    0.0,     -2.,  0.0,
     /         1.,  1.,  0.,  0.,  0.,      -3.,    0.0,      0.,  0.0,
     /         1., -1.,  0., -1.,  0.,      -3.,    0.0,      0.,  0.0,
     /        -1., -1.,  2.,  2.,  2.,      -3.,    0.0,      1.,  0.0,
     /         0., -1.,  2.,  2.,  2.,      -3.,    0.0,      1.,  0.0,
     /         1., -1.,  2.,  0.,  2.,      -3.,    0.0,      1.,  0.0/
      DATA X4/ 3.,  0.,  2.,  0.,  2.,      -3.,    0.0,      1.,  0.0,
     /        -2.,  0.,  2.,  0.,  2.,      -3.,    0.0,      1.,  0.0,
     /         1.,  0.,  2.,  0.,  0.,       3.,    0.0,      0.,  0.0,
     /        -1.,  0.,  2.,  4.,  2.,      -2.,    0.0,      1.,  0.0,
     /         1.,  0.,  0.,  0.,  2.,      -2.,    0.0,      1.,  0.0,
     /        -1.,  0.,  2., -2.,  1.,      -2.,    0.0,      1.,  0.0,
     /         0., -2.,  2., -2.,  1.,      -2.,    0.0,      1.,  0.0,
     /        -2.,  0.,  0.,  0.,  1.,      -2.,    0.0,      1.,  0.0,
     /         2.,  0.,  0.,  0.,  1.,       2.,    0.0,     -1.,  0.0,
     /         3.,  0.,  0.,  0.,  0.,       2.,    0.0,      0.,  0.0,
     /         1.,  1.,  2.,  0.,  2.,       2.,    0.0,     -1.,  0.0,
     /         0.,  0.,  2.,  1.,  2.,       2.,    0.0,     -1.,  0.0,
     /         1.,  0.,  0.,  2.,  1.,      -1.,    0.0,      0.,  0.0,
     /         1.,  0.,  2.,  2.,  1.,      -1.,    0.0,      1.,  0.0,
     /         1.,  1.,  0., -2.,  1.,      -1.,    0.0,      0.,  0.0,
     /         0.,  1.,  0.,  2.,  0.,      -1.,    0.0,      0.,  0.0,
     /         0.,  1.,  2., -2.,  0.,      -1.,    0.0,      0.,  0.0,
     /         0.,  1., -2.,  2.,  0.,      -1.,    0.0,      0.,  0.0,
     /         1.,  0., -2.,  2.,  0.,      -1.,    0.0,      0.,  0.0,
     /         1.,  0., -2., -2.,  0.,      -1.,    0.0,      0.,  0.0/
      DATA X5/ 1.,  0.,  2., -2.,  0.,      -1.,    0.0,      0.,  0.0,
     /         1.,  0.,  0., -4.,  0.,      -1.,    0.0,      0.,  0.0,
     /         2.,  0.,  0., -4.,  0.,      -1.,    0.0,      0.,  0.0,
     /         0.,  0.,  2.,  4.,  2.,      -1.,    0.0,      0.,  0.0,
     /         0.,  0.,  2., -1.,  2.,      -1.,    0.0,      0.,  0.0,
     /        -2.,  0.,  2.,  4.,  2.,      -1.,    0.0,      1.,  0.0,
     /         2.,  0.,  2.,  2.,  2.,      -1.,    0.0,      0.,  0.0,
     /         0., -1.,  2.,  0.,  1.,      -1.,    0.0,      0.,  0.0,
     /         0.,  0., -2.,  0.,  1.,      -1.,    0.0,      0.,  0.0,
     /         0.,  0.,  4., -2.,  2.,       1.,    0.0,      0.,  0.0,
     /         0.,  1.,  0.,  0.,  2.,       1.,    0.0,      0.,  0.0,
     /         1.,  1.,  2., -2.,  2.,       1.,    0.0,     -1.,  0.0,
     /         3.,  0.,  2., -2.,  2.,       1.,    0.0,      0.,  0.0,
     /        -2.,  0.,  2.,  2.,  2.,       1.,    0.0,     -1.,  0.0,
     /        -1.,  0.,  0.,  0.,  2.,       1.,    0.0,     -1.,  0.0,
     /         0.,  0., -2.,  2.,  1.,       1.,    0.0,      0.,  0.0,
     /         0.,  1.,  2.,  0.,  1.,       1.,    0.0,      0.,  0.0,
     /        -1.,  0.,  4.,  0.,  2.,       1.,    0.0,      0.,  0.0,
     /         2.,  1.,  0., -2.,  0.,       1.,    0.0,      0.,  0.0,
     /         2.,  0.,  0.,  2.,  0.,       1.,    0.0,      0.,  0.0/
      DATA X6/ 2.,  0.,  2., -2.,  1.,       1.,    0.0,     -1.,  0.0,
     /         2.,  0., -2.,  0.,  1.,       1.,    0.0,      0.,  0.0,
     /         1., -1.,  0., -2.,  0.,       1.,    0.0,      0.,  0.0,
     /        -1.,  0.,  0.,  1.,  1.,       1.,    0.0,      0.,  0.0,
     /        -1., -1.,  0.,  2.,  1.,       1.,    0.0,      0.,  0.0,
     /         0.,  1.,  0.,  1.,  0.,       1.,    0.0,      0.,  0.0,
     /                      126 *  0./
C
C***********************************************************************
C
C 7.2.4 CONSTANTS USED - CENTJ, DJ2000, EC(4), NOT, X(9,120),
C                    NOP, IDP(6), ARGP(2,6)
C
C 7.2.5 PROGRAMMER - DALE MARKHAM   01/13/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 12/22/77
C                    CHOPO MA       08/04/81
C                    GEORGE KAPLAN  04/24/84
C                    David Gordon   94.04.15 Converted to Implicit None.
C                    David Gordon   95.09.27 X(9,120) table changed from Real*4
C                                   to Real*8
C                    David Gordon   98.02.03 Removed X(9,120) from COMMON
C                                   /NUTCM/ and put it into COMMON /XWAHR/,
C                                   and removed it from most subroutines.
C
C 7.3   NUTBD PROGRAM STRUCTURE - NONE
C
      END
C
      SUBROUTINE CTTAB (VOL, CNO, VER, IERR)
C-----------------------------------------------------------------------
C! Pick up the values of the given column from the CT table
C# Calibration EXT-appl EXT-util
C-----------------------------------------------------------------------
C   Usually used by CLCOR at the optype='EOPS' to restore the VLBA
C   correlator treatment of the Earth rotation
C
C   Inputs:
C      VOL      I    The file disk number
C      CNO      I    The file catalog slot number.
C      VER      I    CT table version
C   Output in common:
C      INDAY    D(*)       Day numbers
C      UT1C     D(*)       UT1-UTC values, in sec
C      WOBX     D(*)       Polar wobble at X, in milliarcsec
C      WOBY     D(*)       Polar wobble at Y, in milliarcsec
C      LEAPS    D(*)       IAT-UTC, in seconds
C      TBEG     D(*)       Begin time of each group (CT table)
C      TEND     D(*)       End time of each group (CT table)
C      TBEG     D(*)       Begin time of each group (CT table)
C      TEND     D(*)       End time of each group
C      NATGRO   I(*)       Number of rows at the group
C      NACUMU   I(*)       Number of rows accumulated
C                          before the group
C      NGROUP   I          Number of groups at the CT table
C      ICASE    I          1,2,3 depending on the CT table content
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DCTV.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'CLCOR.INC'
C-----------------------------------------------------------------------
      INTEGER   VOL, CNO, VER, IERR
      INTEGER   NTR
      DOUBLE PRECISION  TR(2,200)
C                                       CTINI, TABCT
      INTEGER   CTKOLS(12), CTNUMV(12), ICTRNO, ICT, I5, IATGRO,
     *   KATGRO
      DOUBLE PRECISION    TIME, UT1UTC, IATUTC, A1IAT, WOBXY(2), DPSI,
     *   DDPSI, DEPS, DDEPS, TRANGE(2)
C
      CHARACTER    UT1TYP*1, WOBTYP*1, CHSIGN*1
      INTEGER  BUFF(512), HMS(3)

      INTEGER   IGROUP, NUMREC, TABLUN, ICURRE, IPREVI, INDEX
      LOGICAL   TZERO,  IDENTI
      REAL TBREAL, TEREAL, SEC

      DATA TABLUN /27/
C-----------------------------------------------------------------------
C                                       Open table file
      CALL CTINI ('READ', BUFF, VOL, CNO, VER, CATBLK, TABLUN,
     *   ICTRNO, CTKOLS, CTNUMV, IERR)

      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR, VER
         GO TO 990
         END IF
      NUMREC = BUFF(5)
C                                       initiate number of different
C                                       rows in each group
      DO 10 IGROUP = 1, MAXGRO
C                                       Number of rows at the
C                                       group equaled IGROUP
         NATGRO(IGROUP) = 0
C                                       Number of rows accumulated
C                                       before group equaled IGROUP
         NACUMU(IGROUP) = 0
   10    CONTINUE
      IPREVI = -1000
      IGROUP = 1
      IDENTI = .TRUE.
      TZERO = .TRUE.
C
      DO 20 ICTRNO = 1, NUMREC
         ICT = ICTRNO
         CALL TABCT ('READ', BUFF, ICT, CTKOLS, CTNUMV, TIME,
     *      UT1UTC, IATUTC, A1IAT, UT1TYP, WOBXY, WOBTYP, DPSI,
     *      DDPSI, DEPS, DDEPS, TRANGE, IERR)
C                                       Identity of the groups based
C                                       on current TIME < the previous
         ICURRE = TIME
         IF (ICURRE.GT.IPREVI) THEN
            NATGRO(IGROUP) = NATGRO(IGROUP) + 1
            INDEX = NACUMU(IGROUP) + NATGRO(IGROUP)
            INDAY(INDEX) = TIME
            UT1C(INDEX) = UT1UTC
            WOBX(INDEX) = WOBXY(1)*1000
            WOBY(INDEX) = WOBXY(2)*1000
            LEAPS(INDEX) = IATUTC
C
            IF (NATGRO(IGROUP).EQ.1) THEN
               TBEG(IGROUP) = TRANGE(1)
               TEND(IGROUP) = TRANGE(2)
               TZERO = TZERO .AND. (TBEG(IGROUP).EQ.0 .AND.
     *            TEND(IGROUP).EQ.0)
               END IF

            IPREVI = ICURRE
         ELSE
            IPREVI = ICURRE
            IGROUP = IGROUP + 1
            NACUMU(IGROUP) = NACUMU(IGROUP-1) + NATGRO(IGROUP-1)
            NATGRO(IGROUP) = NATGRO(IGROUP) + 1
            INDEX = NACUMU(IGROUP) + NATGRO(IGROUP)
            INDAY(INDEX) = TIME
            UT1C(INDEX) = UT1UTC
            WOBX(INDEX) = WOBXY(1)*1000
            WOBY(INDEX) = WOBXY(2)*1000
            LEAPS(INDEX) = IATUTC
C            IF (NATGRO(IGROUP).EQ.1) THEN
            TBEG(IGROUP) = TRANGE(1)
            TEND(IGROUP) = TRANGE(2)
            TZERO = TZERO .AND. (TBEG(IGROUP).EQ.0 .AND.
     *         TEND(IGROUP).EQ.0)
            IF (IGROUP.GE.2) IDENTI = IDENTI .AND.
     *         UT1C(NACUMU(IGROUP) +1).EQ.
     *         UT1C(NACUMU(IGROUP-1) +1)
     *         .AND. WOBX(NACUMU(IGROUP) +1).EQ.
     *         WOBX(NACUMU(IGROUP-1) +1)
C               END IF
            END IF
   20    CONTINUE
      NGROUP = IGROUP
C                                       determine the cases
      IF(IDENTI) THEN
         ICASE = 0
      ELSE
         ICASE = 1
         IF (TZERO) ICASE = 2
         END IF
C                                       Close table.
      CALL TABIO ('CLOS', 0, ICTRNO, BUFF, BUFF, IERR)
C                                       print out EOP data for
C                                       the first 5 CT rows
      IF (ICASE.EQ.0) THEN
         MSGTXT = 'UT1-UTC(sec), POLX, POLY(milliasec) ' //
     *      'used by correlator'
         CALL MSGWRT (4)
         DO 30 I5 = 1, NCTROW
            WRITE (MSGTXT, 1200) I5, INDAY(I5), UT1C(I5),
     *         WOBX(I5), WOBY(I5)
            CALL MSGWRT (8)
   30       CONTINUE
      ELSE
C                                       read the history  file to get
C                                       TBEG/TEND
         IF (ICASE.EQ.2) THEN
            CALL HIGETT (VOL, CNO, NTR, TR)
            DO 40 IGROUP = 1, NTR
               TBEG(IGROUP) = TR(1, IGROUP)
               TEND(IGROUP) = TR(2, IGROUP)
   40          CONTINUE
            END IF

         DO 60 IGROUP = 1, NGROUP
            MSGTXT = 'UT1-UTC(sec), POLX, POLY(milliasec) ' //
     *         'used by correlator, TRANG'
            CALL MSGWRT (4)
            KATGRO = NATGRO(IGROUP)
            INDEX = NACUMU(IGROUP)
            DO 50 IATGRO = 1, KATGRO
               INDEX = INDEX + 1
               IF (IATGRO.EQ.1) THEN
                  TBREAL = TBEG(IGROUP)
                  CALL TFDHMS (TBREAL, 1, CHSIGN, HMS, SEC)
                  WRITE (MSGTXT, 1220) INDEX, INDAY(INDEX), UT1C(INDEX),
     *               WOBX(INDEX), WOBY(INDEX), CHSIGN, HMS, SEC
                  CALL MSGWRT (8)
               ELSE
                  IF (IATGRO.EQ.2) THEN
                     TEREAL = TEND(IGROUP)
                     CALL TFDHMS (TEREAL, 1, CHSIGN, HMS, SEC)
                     WRITE (MSGTXT, 1220) INDEX, INDAY(INDEX),
     *                  UT1C(INDEX),WOBX(INDEX), WOBY(INDEX), CHSIGN,
     *                  HMS, SEC
                     CALL MSGWRT (8)
                  ELSE
                     WRITE (MSGTXT, 1200) INDEX, INDAY(INDEX),
     *                  UT1C(INDEX), WOBX(INDEX), WOBY(INDEX)
                     CALL MSGWRT (8)
                     END IF
                  END IF
   50          CONTINUE
   60       CONTINUE

         END IF

C                                       use intervals between the group
C                                       ends to exclude the gaps between
C                                       previous end and current begin
         DO 70 IGROUP = 1, NGROUP
            IF (IGROUP.EQ.1) THEN
               TBEG(IGROUP) = -100
            ELSE
               TBEG(IGROUP) = TEND(IGROUP-1)
               END IF
C
            IF (IGROUP.EQ.NGROUP) TEND(IGROUP) = 100
   70       CONTINUE


      GO TO 999

C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR',I5,' OPENING ','CT',' TABLE VERS=',I6)
 1200 FORMAT (I5, F5.0, F10.6, 2F10.3)
 1220 FORMAT (I5, F5.0, F10.6, 2F10.3,
     *   2X, A1,I2,'/',I2.2,':',I2.2,':',F4.1)
      END
      SUBROUTINE HIGETT (DISK, CNO, NTR, TR)
C-----------------------------------------------------------------------
C   HIGETT gets the JOBSTART to JOBSTOP time ranges from the specified
C   HIstory file from the FITLD entries
C   Inputs:
C      DISK     I        Disk number
C      CNO      I        Catalog number
C   Input in common
C      CATBLK   I(256)   Catalog header
C   Outputs:
C      NTR      I        Number time ranges found
C      TR       D(2,*)   Time ranges
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, NTR
      DOUBLE PRECISION TR(2,*)
C
      INTEGER   IHLUN, HIBUFF(256), IERR, IHPTR, NREC, IBLK, ICARD, II,
     *   NTE, LTY, IY, IM, ID, LH, LM, I, ICUR, IHIND
      REAL      LS
      CHARACTER LINE*72, CM*3, CMS(12)*3, DTEMP*8
      DOUBLE PRECISION JD0, JD, T
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA CMS /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     *   'OCT','NOV','DEC'/
C-----------------------------------------------------------------------
      NTR = 0
      NTE = 0
      CALL H2CHR (8, 1, CATH(KHDOB), DTEMP)
      CALL JULDAY (DTEMP, JD0)
C                                       open HI file
      IHLUN = 89
      CALL HIINIT (3)
      CALL HIOPEN (IHLUN, DISK, CNO, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL HILOCT ('SRCH', IHLUN, IHPTR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine no. of hist. recs.
      NREC = HITAB(IHPTR+2)
      IHIND = HITAB(IHPTR+1)
      IBLK = 0
      ICARD = NHILPR
      DO 20 ICUR = 1,NREC
C                                       Read next buffer.
         ICARD = ICARD + 1
         IF (ICARD.GT.NHILPR) THEN
            IBLK = IBLK + 1
            ICARD = 1
            CALL ZFIO ('READ', IHLUN, IHIND, IBLK, HIBUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
C                                       desired task card?
         II = (ICARD-1) * NHIWPL + 5
         CALL H2CHR (72, 1, HIBUFF(II), LINE)
         LTY = 0
         IF (LINE(:14).EQ.'FITLD JOBSTART') LTY = 1
         IF (LINE(:14).EQ.'FITLD JOBSTOP ') LTY = 2
         IF (LTY.GT.0) THEN
            READ (LINE(18:),1000) IY, CM, ID, LH, LM, LS
            DO 10 I = 1,12
               IF (CM.EQ.CMS(I)) IM = I
 10            CONTINUE
            WRITE (DTEMP,1010) IY, IM, ID
            CALL JULDAY (DTEMP, JD)
            T = JD - JD0 + ((LS / 60.0D0 + LM) / 60.0D0 + LH) / 24.0D0
            IF (LTY.EQ.1) THEN
               NTR = NTR + 1
               TR(1,NTR) = T
            ELSE
               NTE = NTE + 1
               IF (NTE.EQ.NTR) THEN
                  TR(2,NTR) = T
               ELSE
                  MSGTXT = 'HIGETT: SYNCHRONIZATION PROBLEM IN HI' //
     *               ' OF FITLD'
                  CALL MSGWRT (7)
                  END IF
               END IF
            END IF
 20      CONTINUE
C
 100  CALL HICLOS (IHLUN, .FALSE., HIBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4,A3,I2,I3,1X,I2,1X,F5.2)
 1010 FORMAT (I4.4,I2.2,I2.2)
      END
