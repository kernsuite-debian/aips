LOCAL INCLUDE 'DFCOR.INC'
C                                                          Include DFCOR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLTAB.INC'
C                                       Inputs and general info
      INTEGER NDAT
      PARAMETER (NDAT= 100000)
      INTEGER NLINES, IAN(NDAT)
      DOUBLE PRECISION  TMES(NDAT)
      REAL VERDEL(NDAT)
C                                       arrays of values for the given
C                                       calibrators
      INTEGER MDAT
      PARAMETER (MDAT= 100000)
      REAL APDLY(MDAT), ADPDLY(MDAT), ATIME(MDAT)
      INTEGER AANT(MDAT)
C
      INTEGER   SEQIN, SUBA, DISKIN, CNOIN, NUMHIS, CLVER, CLUSE,
     *   NSOUWD, SOUWAN(30),
     *   NSOCWD, SOCWAN(30),
     *   NANTSL, ANTENS(50), BIF, EIF, ISTOK,
     *   FREQID, NTERM, NCALLI, NCALID
      LOGICAL   DOSWNT, DOAWNT, DESEL, DOCWNT
      REAL      XSIN, XDISIN, XFQID, XBAND, XFREQ, XBIF, XEIF, XTIME(8),
     *   XANT(50), XSUBA, XGVER, XGUSE, BPARM(20), XBAD(10), SELBAN,
     *   AXOFF
      CHARACTER  HISCRD(30)*64, NAMEIN*12, CLAIN*6, XSOUR(30)*16,
     *   XCALS(30)*16, XSTOK*4, OPCODE*4, INFILE*48
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALS(4,30),
     *   XXSTOK, XOPCOD, XINFIL(12)
      DOUBLE PRECISION FRQOFF(MAXIF), SELFRQ
C                                       Buffers and file info
      INTEGER   BUFFER(1024)
C                                       Important constants
      DOUBLE PRECISION PI, TWOPI, CLIGHT
      PARAMETER (PI=3.1415926536D0, TWOPI=6.2831853072D0,
     *   CLIGHT=2.997925D8)
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
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XXCALS,
     *   XXSTOK, XBAND, XFREQ, XFQID, XBIF, XEIF, XTIME, XANT, XSUBA,
     *   XGVER, XGUSE, XOPCOD, BPARM, XBAD, XINFIL, SELBAN, SEQIN,
     *   DISKIN, CNOIN, SUBA, CLVER, CLUSE
      COMMON /CINFO/ FRQOFF, SELFRQ, DOSWNT,
     *   DOCWNT,
     *   DOAWNT, DESEL, NSOUWD, SOUWAN,
     *   NSOCWD, SOCWAN,
     *   NANTSL, ANTENS, BIF, EIF, ISTOK, FREQID, NUMHIS

      COMMON /GETA/ NLINES, IAN, VERDEL
      COMMON /CALDAT/ NCALLI, APDLY, ADPDLY, ATIME, AANT, NCALID
      COMMON /GETAD/ TMES
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, XSOUR,
     *   XCALS,
     *   XSTOK, OPCODE, INFILE
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFER
C                                       Internal storage
      COMMON /CLRECC/ COSDEC, SINDEC, CLRECD,
     *   GMMOD, PARM, PANGLE, AXOFF, NTERM,
     *   FIXCNT, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, ICODE
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
C                                                          End DFCOR
LOCAL END
      PROGRAM DFCOR
C-----------------------------------------------------------------------
C! Determines applies difference calibration corrections to the CL table.
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2003, 2007-2009, 2012
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
C   Task DFCOR applies corrections to CL tables.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'DFCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'DFCOR '/
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
C
 999  STOP
      END
      SUBROUTINE CLCLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   CLCLIN gets input parameters for DFCOR.
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
      INCLUDE 'DFCOR.INC'
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
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
C      NPARM = 237
      NPARM = 357
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
C                                       default if ATMO
      IF (OPCODE.EQ.'   ') OPCODE = 'ATMO'
      CALL H2CHR (48, 1, XINFIL, INFILE)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         CALL H2CHR (16, 1, XXCALS(1,I), XCALS(I))
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
         MSGTXT = 'NO CL TABLES FOUND, CANNOT DFCOR'
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
C                                       Get cal. source numbers
      CALL FNDSOU (DISKIN, CNOIN, XCALS, BUFFER, NSOCWD, DOCWNT,
     *   SOCWAN, JERR)
C                                       Number of calibrators should be
C                                      .LE.1
      IF (NSOCWD.GT.1 .OR. NSOCWD.EQ.0) THEN
         MSGTXT = 'Number of calibrators can not be >1'
         JERR = 1
         GO TO 990
         END IF
C                                       Source ID of the calibrator
      NCALID = SOCWAN(1)
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
 1055 FORMAT ('CL VERSION INPUT',I4,' OUTPUT',I4)
 1060 FORMAT ('INPUT VIS RECORDS MISORDERED, SORTED = ',A2,
     *   ' SHOULD BE = TB')
       END
      SUBROUTINE CLCUV (IERR)
C-----------------------------------------------------------------------
C   CLCUV is called from DFCOR. CLCUV reads throught the CL table,
C   passing the records selected to the correction routine CLCCOR.
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN, IRCODE, THSOU, LSTSOU, ANT, I, JERR, IFNO(2),
     *   ICLRNO, NUMREC, LOOP
      LOGICAL   SLCTD
      DOUBLE PRECISION TIMBEG, TIMEND
      INCLUDE 'DFCOR.INC'
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
         IF ((EIF-BIF+1).GT.20) THEN
C                                       Too many IFs, > 20.
            IERR = 10
            I = EIF - BIF + 1
            WRITE (MSGTXT,1000) I
            GO TO 990
            END IF
         CALL ANTCOR (DISKIN, CNOIN, SUBA, CATBLK, IFNO, BPARM, BUFFER,
     *      FREQID, IERR)
         IF (IERR.NE.0) GO TO 999
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
C                                       Read the CL table the first time
C                                       to estimate delays & times of
C                                       the calibrator
C                                       Do it only if CALSOUR.NE.''
      IF (NSOCWD.EQ.0) GO TO 60
      DO 50 LOOP = 1,NUMREC
         ICLRNO = LOOP
         CALL TABIO ('READ', IRCODE, ICLRNO, CLRECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 900
         IF (IERR.LT.0) GO TO 50
C                                       Check data
C                                       Time:
         IF ((CLRECD(TIMCL).LT.TIMBEG) .OR.
     *       (CLRECD(TIMCL).GT.TIMEND)) GO TO 50
C                                       Subarray
         IF ((CLRECI(SUBCL).NE.SUBA) .AND. (CLRECI(SUBCL).GT.0))
     *      GO TO 50
C                                       Freq id
         IF ((CLRECI(FRQCL).NE.FREQID) .AND. (CLRECI(FRQCL).GT.0) .AND.
     *      (FREQID.GT.0)) GO TO 50
         IF (NSOCWD.LE.0) GO TO 30
C                                       Check source
         THSOU = CLRECI(SOUCL)
         IF (.NOT.SLCTD (THSOU, SOCWAN, NSOCWD, DOCWNT)) GO TO 50
 30      LSTSOU = THSOU
C                                       Check antenna
         ANT = CLRECI(ANTCL)
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 50
C
         CALL CLCCOR (2, JERR)
         IF (JERR.NE.0) GO TO 50
   50    CONTINUE
C                                       Number of selected records
C                                       of the given calibrator
      NCALLI = FIXCNT
C                                       zero FIXCNT, NSOCWD
   60 CONTINUE
      NSOCWD = 0
      FIXCNT = 0
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
      INTEGER  NOP
      PARAMETER (NOP=20, DEG2RD=57.29577951)
C
      CHARACTER CHTM8*8, OPS(NOP)*4, STRING*8
      INTEGER   I, IANT, LSTSOU, THSOU, LUN, NTERMS, LIM1, LIM2,
     *   IPNT, IINC, ANTNO
      REAL      XT, YT, XXT, YYT, FACTOR, HA, ZA, ELV, POLYN, PFAC,
     *   CFAC, SFAC, DX, DY, AZ
      DOUBLE PRECISION    FREQS, SINLAT, COSLAT, CLTIME, R8T1, R8T2
      INCLUDE 'DFCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA OPS /'PHAS','OPAC','ADEL','GAIN','CLOC','POLR',
     *   'PANG','PONT','IONS','ANTP','PCAL','SBDL','SSLO','RATE','PCFX',
     *   'MBDL','ANAX','ATMO','DUMY','DUMY'/
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
      GO TO (50,55,55,60,70,50,75,80,85,90,95,100,105,45,
     *       110,100,120,130), ICODE
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
         WRITE (HISCRD(NUMHIS),2065) BPARM(1), BPARM(2), BPARM(3)
         IF (NTERMS.GT.3) THEN
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2066) (BPARM(I),I=4,7)
            END IF
         IF (NTERMS.GT.7) THEN
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2066) (BPARM(I),I=8,10)
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
C                                       OPCODE = 'ANTP' (10)
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
            CALL ANTMOD(DISKIN, CNOIN, SUBA, ANTNO, PARM, IERR)
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
         PARM(6) = BPARM(5)/3600.0/DEG2RD
         PARM(7) = BPARM(6)/3600.0/DEG2RD
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
            DX = BPARM(5)/3600.0
            DY = BPARM(6)/3600.0
            CALL SOUMOD(DISKIN, CNOIN, SOUWAN(1), DX, DY, IERR)
            IF (IERR.NE.0) GO TO 990
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
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2090) BPARM(1), BPARM(2), BPARM(3)
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2092) BPARM(5), BPARM(6)
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
         WRITE (HISCRD(NUMHIS), 2130) BPARM(1)
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
C                                       OPCODE='ATMO'(18) Correction
C                                       of the antenna list by the
C                                       vertical atmosphere delay
C                                       given at the INFILE
 130     CONTINUE
C                                       Source number
         PARM(1) = -10
         CALL GETINP (INFILE, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'Error reading INFILE'
            GO TO 990
            END IF
         GO TO 999
C                                       Process record
 200  FIXCNT = FIXCNT + 1
      IANT = CLRECI(ANTCL)
      GO TO (250,300,300,350,400,280,450,500,550,600,610,620,630,
     *       250,640,850,870,890),ICODE
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
C
         IF (ISTOK.EQ.1) GO TO 999
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
            IF (ICODE.EQ.1.OR.ICODE.EQ.6) THEN
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
C                                       GAIN
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
         IF (ISTOK.NE.1) THEN
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
         IF (ISTOK.NE.1) THEN
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
         IF (ISTOK.NE.1) THEN
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
C                                       position error (10)
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
 850     CALL CORMBD
         GO TO 999
C                                       OPCODE='ANAX' (17)
C                                       Correction for antenna axis
C                                       offset
 870     CALL ANAXIS (IERR)
         GO TO 999
C                                       OPCODE='ATMO' (18)
C                                       Correction of the antenna list
C                                       by the vertical atmosphere
C                                       delay given at the INFILE
C
 890     CALL ATMOV (IERR)
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
 1080 FORMAT ('!!! SU table is corrected for the selected source. !!!')
 1081 FORMAT ('!!! So you should apply the corrected CL table     !!!')
 1082 FORMAT ('!!! to match the data. See HELP.                   !!!')
 2040 FORMAT ('OPCODE = ''',A4,''' / Operation code')
 2050 FORMAT ('CLPARM(',I2,')=',F8.2,' / Phase(deg) to rotate gains')
 2055 FORMAT ('CLPARM(1)=',F8.2,' / Zenith opacity')
 2056 FORMAT ('CLPARM=',F8.2,',',F8.2,',',F8.2,
     *   ', / Atm. pres., PP H2O, Temp')
 2057 FORMAT ('      ',F8.2,',',F8.2,',',F8.2,' / Lapse, ht. ',
     *   ' tp, scl. ht. H20')
 2065 FORMAT ('CLPARM =',1PE12.5,2(',',E12.5),' / Gain curve')
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
      END
      SUBROUTINE CLCLHI
C-----------------------------------------------------------------------
C   CLCLHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME(2)*12, LABEL*8
      INTEGER   LUN1, IERR, I, TIME(3), DATE(3), LIMIT, LIMIT2, J
      REAL      TIMBEG, TIMEND
      LOGICAL   T
      INCLUDE 'DFCOR.INC'
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
 20            CONTINUE
            END IF
C                                       Antennas
 25      IF (NANTSL.LE.0) THEN
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
 30            CONTINUE
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
         IF (NUMHIS.LE.0) GO TO 100
         WRITE (LABEL,1011) TSKNAM
         HILINE(1:8) = LABEL(1:8)
         DO 90 I = 1,NUMHIS
            HILINE(9:72) = HISCRD(I)(1:64)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 90         CONTINUE
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
C   indicated by the table header keyword 'POLTYPE'.
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
C      IERR      I      Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, INVER, CATBLK(256), IFNO(2), BUFFER(8),
     *   FREQID, IERR
      REAL      PCOR(*)
C
      CHARACTER    CHPOLT*8, CHSOL(4)*8, SOLTYP*8
      INTEGER   IIF, LUN, IANT, INDEX, LOCS, KEYTYP, ISTYPE, BIF, EIF,
     *   NXIF, NUMREC, IREF
      REAL      POLP1, POLP2, SPCOR, CPCOR
      HOLLERITH XSOLTY(2)
      INCLUDE 'INCS:PUVD.INC'
      REAL      PD(MAXIF)
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHSOL /'ORI-ELP ', 'APPROX  ', 'X-Y LIN', 'VLBI'/
      DATA CHPOLT /'POLTYPE '/
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
C                                       Check FREQID compatibility.
      IF ((ANFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *   (ANFQID.NE.FREQID)) THEN
         MSGTXT = 'WARNING - POTENTIALLY FATAL ERROR'
         CALL MSGWRT (4)
         MSGTXT = '   The polarization variables in your AN table have'
         CALL MSGWRT (4)
         WRITE (MSGTXT,1030) ANFQID
         CALL MSGWRT (4)
         WRITE (MSGTXT,1040) FREQID
         CALL MSGWRT (4)
         MSGTXT = '   Are you sure this is what you want to do?'
         CALL MSGWRT (4)
         END IF
      NUMREC = BUFFER(5)
C                                       IF range to modify.
C                                       This is a risky to tell the
C                                       number of IFs.
      NXIF = ANNUMV(9) / 2
      BIF = IFNO(1)
      IF (BIF.GT.NXIF) BIF = NXIF
      IF (BIF.LE.0) BIF = 1
      EIF = IFNO(2)
      IF (EIF.GT.NXIF) EIF = NXIF
      IF (EIF.LE.0) EIF = 1
C                                       Check solution type keyword.
      CALL TABKEY ('READ', CHPOLT, 1, BUFFER, LOCS, XSOLTY, KEYTYP,
     *   IERR)
      IF (IERR.EQ.0) GO TO 60
         WRITE (MSGTXT,1060) IERR
         GO TO 990
C                                       Decide solution type:
 60   ISTYPE = 0
      CALL H2CHR (8, 1, XSOLTY, SOLTYP)
      IF (SOLTYP.EQ.CHSOL(1)) ISTYPE = 1
      IF (SOLTYP.EQ.CHSOL(2)) ISTYPE = 2
      IF (SOLTYP.EQ.CHSOL(3)) ISTYPE = 3
      IF (SOLTYP.EQ.CHSOL(4)) ISTYPE = 4
      IF (ISTYPE.EQ.0) THEN
C                                       Unknown pol. solution type.
         IERR = 10
         WRITE (MSGTXT,1070) SOLTYP
         GO TO 990
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
C                                       Fetch old phase differences
         CALL PDRGET (DISK, CNO, INVER, LUN, CATBLK, NXIF, IREF, PD,
     *      BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Update values (radians)
         INDEX = 1
         DO 80 IIF = BIF,EIF
            PD(IIF) = PD(IIF) + PCOR(INDEX) * 1.745329E-2
            INDEX = INDEX + 1
 80         CONTINUE
C                                       Save results
         CALL PDRSET (DISK, CNO, INVER, LUN, CATBLK, NXIF, IREF, PD,
     *      BUFFER, IERR)
C                                       Done
         GO TO 999
         END IF
C                                       Read AN records
      DO 200 IANT = 1,NUMREC
         IANRNO = IANT
         CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'READ'
            GO TO 990
            END IF
C                                       Feed polarizations
         INDEX = 1 + (BIF-1) * 2
         DO 150 IIF = BIF,EIF
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
            ELSE
C                                       Nothing for now
               CONTINUE
               END IF
            INDEX = INDEX + 2
 150        CONTINUE
C                                       Write record
         IANRNO = IANT
         CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'WRIT'
            GO TO 990
            END IF
 200     CONTINUE
C                                      Close AN extension files
      CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1200) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANTCOR: ERROR',I3,' OPEN-FOR-',A4,'ING AN FILE')
 1030 FORMAT ('   previously been modified for FQID ',I3)
 1040 FORMAT ('   You are now changing them with FREQID =',I3)
 1060 FORMAT ('ANTCOR: ERROR',I3,' FINDING POL. SOLUTION TYPE KEYWORD')
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
      INTEGER   LUN, IANT,  NUMREC, I
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
     *   NOPCAL, ANFQID, ANTNIF, IERR)
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
      DOUBLE PRECISION HRANG, DARG, FREQS, SINLAT, COSLAT
      INCLUDE 'DFCOR.INC'
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
      COSLAT = COS (STNLAT(IANT))
      SINLAT = SIN (STNLAT(IANT))
      DARG = SINLAT * SINDEC + COSLAT * COSDEC * COS (HRANG)
      IF (ICODE.EQ.3) GO TO 500
C                                       Transmission factor:
C                                       Modified cosecant law from
C                                       Chopo Ma's thesis:
      ARG = PARM(1) / (DARG + (0.00143 / (TAN(ELV) + 0.0045)))
C                                       Three term approx. of exp.
      FACTOR = 1.0 + ARG * (1.0 + 0.5 * ARG )
C                                       Need square root for
C                                       calibration table factor
      FACTOR = SQRT (FACTOR)
C
      IF (ISTOK.EQ.2) GO TO 150
      DO 100 I = BIF,EIF
         XT = CLRECR(RE1CL+I-1)
         YT = CLRECR(IM1CL+I-1)
         IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
            CLRECR(RE1CL+I-1) = XT * FACTOR
            CLRECR(IM1CL+I-1) = YT * FACTOR
            END IF
 100     CONTINUE
 150     IF (ISTOK.EQ.1) GO TO 999
      DO 200 I = BIF,EIF
         XT = CLRECR(RE2CL+I-1)
         YT = CLRECR(IM2CL+I-1)
         IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK)) THEN
            CLRECR(RE2CL+I-1) = XT * FACTOR
            CLRECR(IM2CL+I-1) = YT * FACTOR
            END IF
 200     CONTINUE
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
      IF (ISTOK.EQ.2) GO TO 650
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
     *      CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
         IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *      CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600     CONTINUE
 650     IF (ABS(ISTOK).EQ.1) GO TO 999
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
     *      CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
         IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *      CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
 700     CONTINUE
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
      INCLUDE 'DFCOR.INC'
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
      IF (LSTSOU.NE.THSOU) THEN
C                                       Get source info
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
         PARM(12) = THSOU
         FREQS = FREQ + FREQO(BIF)
         END IF
      CLTIME = CLRECD(TIMCL)
      GDELAY = ((CLTIME - PARM(3)) * 86400.0 * PARM(1)) + PARM(2)
      DGDELY = PARM(1)
C                                       Clock
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
         FQFAC = -TWOPI * (FREQS+FRQOFF(I)) * GDELAY
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
 100     CONTINUE
      IF (ISTOK.EQ.1) GO TO 999
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
         FQFAC = -TWOPI * (FREQS + FRQOFF(I)) * GDELAY
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
 200     CONTINUE
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
      REAL      XT, YT, XXT, YYT, PDLY(2), DPDLY(2),
     *   ZA, ARG, HA, ELV, GDLY(2), DGDLY(2), GDLYIF,
     *   PDLYIF, DGDLYI, DPDLYI, AZ, FR, DELDT
      DOUBLE PRECISION HRANG, DARG, FREQS, SINLAT, COSLAT
      INCLUDE 'DFCOR.INC'
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
      AZ = ATAN2 (-COSDEC*SIN (HRANG),
     *   (SINDEC*COSLAT - COSDEC*COS (HRANG)*SINLAT))
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
            ARG = -FR * ((CLIGHT / (FREQS + FRQOFF(I))) ** 2) +
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
      IF (ISTOK.NE.1) THEN
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
            ARG = FR * ((CLIGHT / (FREQS + FRQOFF(I))) ** 2) +
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
      REAL      TWOPI, CLIGHT
      PARAMETER (TWOPI = 6.2831853)
      PARAMETER (CLIGHT = 2.997925E8)
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
     *   (TWOPI * TWOPI * CLIGHT * CLIGHT)
C                                       Group and Phase delay
C                                       From: Hagfors, Methods of
C                                       Experimental Physics,
C                                       Vol 12 B, Meeks, ed
C***??? is this right???
      PDLY(1) = -40.28 * ZCD * ZAFACT / CLIGHT
      PDLY(2) = PDLY(1)
      DPDLY(1) = -40.28 * ZCD * DZAFDT / CLIGHT
      DPDLY(2) = DPDLY(1)
      GDLY(1) = 40.28 * ZCD * ZAFACT / CLIGHT
      GDLY(2) = GDLY(1)
      DGDLY(1) = 40.28 * ZCD * DZAFDT / CLIGHT
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
      REAL   MLAT, MLONG, ANNTIM, LOCTIM, SUNSPT
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
C      ISTOK    I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C   Output:
C      IERR     I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, I, ITEMP, IANT
      REAL      XT, YT, CFAC, SFAC, FQFAC, EL
      DOUBLE PRECISION CHAD, SHAD, HA, HAD
      DOUBLE PRECISION FREQS, CIR, X, Y, Z, DELAYC, RATEC,
     *   DDEC, DRA, CI, RADSEC, DELAY, RATE, PDLY, DPDLY, HRANG,
     *   PII, TWOPII, SIND, COSD
C                                       CI = 1/speed of light
      PARAMETER (CI = 1.0D0 / 2.997925D8)
C                                       RADSEC = earth rot rate in
C                                       rad/sec.
      PARAMETER (RADSEC = 3.1415926535897932384D0 / 43200.0D0)
      INCLUDE 'DFCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      SAVE  FREQS
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
C                                       Declination
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
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
C---------------------------------
      HAD = HA
      CHAD = COS (HAD)
      SHAD = SIN (HAD)
C                                       Antenna coordinates:
C                                       correct for handedness of
C                                       coordinates.
      X = STNX(IANT)
      Y = STNY(IANT)
      Z = STNZ(IANT)
      CIR = CI * RADSEC
C                                       Delay and rate in sec and
C                                       sec/sec. (want corrections).
C                                       The formulae are written
C                                       at the Right Hand coordinate
C                                       system.
C                                       not corrected delay and rate
      DELAY = CI * ((X * CHAD - Y * SHAD) * COSDEC + Z * SINDEC)
      RATE = CIR * (-X * SHAD - Y * CHAD) * COSDEC
C                                       correcte antenna coordinates
      X = X + PARM(1)
      Y = Y + PARM(2)
      Z = Z + PARM(3)
C                                       Source position error in RA
C                                       direction at the picture plane
      DRA = 0.0
      IF (COSDEC.NE.0.0) DRA = PARM(6) / COSDEC
C                                       Source position error in
C                                       declination direction
      DDEC = PARM(7)
C                                       correct hour angle
      CHAD = COS (HAD - DRA)
      SHAD = SIN (HAD - DRA)
C                                       correct declination
      SIND = SIN (DECAPP + DDEC)
      COSD = COS (DECAPP + DDEC)
C                                       corrected delay and rate
      DELAYC = CI * ((X * CHAD - Y * SHAD) * COSD + Z * SIND)
      RATEC = CIR * (-X * SHAD - Y * CHAD) * COSD
C                                       correction of delay and rate
      PDLY = DELAYC - DELAY
      DPDLY = RATEC - RATE
C                                       Correct CL record
C                                       Geometric delay (2 terms)
      IF (CLRECD(GDLCL).NE.DBLANK) CLRECD(GDLCL) = CLRECD(GDLCL) - PDLY
C                                       Second term
      IF ((CLNUMV(CLDDEL).GE.2) .AND. (CLRECD(GDLCL+1).NE.DBLANK))
     *   CLRECD(GDLCL+1) = CLRECD(GDLCL+1) - DPDLY
      IF (ISTOK.EQ.2) GO TO 650
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
     *      CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
         IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *      CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600     CONTINUE
 650     IF (ISTOK.EQ.1) GO TO 999
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
     *      CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
         IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *      CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
 700     CONTINUE
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
      INCLUDE 'DFCOR.INC'
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
      IF (ISTOK.NE.1) THEN
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
      INCLUDE 'DFCOR.INC'
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
      IF (ISTOK.NE.1) THEN
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
      INCLUDE 'DFCOR.INC'
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
      IF (ISTOK.NE.1) THEN
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
      INCLUDE 'DFCOR.INC'
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
      IF (ISTOK.EQ.2) GO TO 650
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
 600     CONTINUE
C
 650  IF (ISTOK.EQ.1) GO TO 999
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
 700     CONTINUE
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
      INCLUDE 'DFCOR.INC'
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
 500  IF (ISTOK.NE.1) THEN
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
      INCLUDE 'DFCOR.INC'
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
C                                       Get source info
      IF (LSTSOU.NE.THSOU) THEN
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
C                                       Altaz or EW/Nasmyth mount
C                       (folded cassegrain is a varient of Nasmyth)
      IF ((MOUNT.EQ.0) .OR. (MOUNT.GE.3)) THEN
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
C                                       Equatorial mount
      ELSE IF (MOUNT.EQ.1) THEN
         PDLY = AXOFF*CI*COSDEC
         DPDLY = 0.0
         END IF
C                                       Correct CL record
C                                       Geometric delay (2 terms)
      IF (CLRECD(GDLCL).NE.DBLANK) CLRECD(GDLCL) = CLRECD(GDLCL) - PDLY
C                                       Second term
      IF ((CLNUMV(CLDDEL).GE.2) .AND. (CLRECD(GDLCL+1).NE.DBLANK))
     *   CLRECD(GDLCL+1) = CLRECD(GDLCL+1) - DPDLY
C                                       Correct the first polarization
      DO 600 I = 1, NUMIF
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
     *      CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
         IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *      CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600     CONTINUE
C                                       Correct the second polarization
C                                       if it exists
         IF (NUMPOL. EQ. 2) THEN
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
     *            CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
               IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *            CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
  700          CONTINUE
            END IF
C
 999  RETURN
      END
      SUBROUTINE SOUEL (ANTNO, TIME, HA, EL)
C-----------------------------------------------------------------------
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
C      HA         D    Source hour angle (rad) wrt to antenna
C      EL         R    Source elevation (rad) at antenna
C-----------------------------------------------------------------------
      INTEGER   ANTNO
      DOUBLE PRECISION TIME
      REAL      EL
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
      HRANG = DMOD( HRANG, TWOPI)
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
      INTEGER   NENTRY, BUFFAN(512), I, LUN1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA LUN1 /28/
C-----------------------------------------------------------------------
C                                       Open for write
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
      SUBROUTINE SOUMOD (DISK, CNO, SOUS, DX, DY, IERR)
C-----------------------------------------------------------------------
C   Subroutine to make the correction of the selected source position
C   if OPCODE ='ANTP' and the source correction parameters are not
C   equal zero.
C   Inputs:
C      DISK       I    The file disk number
C      CNO        I    The file catalog slot number.
C      SOUS       I    The source ID number
C      DX         R    Correction at the picture plane in RA direction,
C                      in degrees.
C      DY         R    Correction at DEC, in degrees.
C   Input from common /MAPHDR/
C      CATBLK(256)  I    Catalog header record.
C-----------------------------------------------------------------------
C                                       Declarations for recalculation
C                                       aparent coordinates to epoch
      INTEGER IEQ, DIR
      REAL      REQ, POLAR(2)
      CHARACTER OBSDAT*8
      DOUBLE PRECISION JD, DELDAT, OBSPOS(3), RAATT, DECTT
      LOGICAL GR
C
      INTEGER   DISK, CNO, BUFFER(1024), SOUS, IERR
      REAL      DX, DY, DRA, COSDEC
C                                       Declarations for SOUINI
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELDEF*8, VELTYP*8, CALCOD*4, SOUNAM*16
      INTEGER   JERR, IDSOU, SUKOLS(MAXSUC), LUN, SUNUMV(MAXSUC),
     *   QUAL, NUMIF, NSOURC, I, ISURNO, SUFQID
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION    LSRVEL(MAXIF), FREQO(MAXIF), LRESTF(MAXIF)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (IEQ, REQ)
      DATA DELDAT, OBSPOS, POLAR /1.D-6, 0.D0, 0.D0, 0.D0, 0., 0./
      DATA LUN /28/
C-----------------------------------------------------------------------
C                                       take RA, DEC(EPOC) from the
C                                       catalog
      RA = CATD(KDCRV+JLOCR)
      DEC = CATD(KDCRV+JLOCD)
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
      DO 300 I = 1, NSOURC
C                                       Read record
         ISURNO = I
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1200) I
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Put the correction here
         IF (IDSOU.EQ.SOUS) THEN
C                                       Recalculate the source position
C                                       error in RA from the error at
C                                       the picture plane
            COSDEC = COS (DECAPP * DG2RAD)
            DRA = 0.0
            IF (COSDEC.NE.0.0) DRA = DX / COSDEC
            RAAPP  = RAAPP + DRA
            DECAPP = DECAPP + DY
C                                       Find RAEPO, DECEPO for
C                                       the given epoch
C
C                                       Find time of observation
            CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
C                                       Find Julian day, JD
            CALL JULDAY(OBSDAT, JD)
            IEQ = CATBLK(KREPO)
            EPOCH = REQ
            RAATT = RAAPP * DG2RAD
            DECTT = DECAPP * DG2RAD
            GR = .TRUE.
            DIR = -1
            CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *         RAEPO, DECEPO, RAATT, DECTT)
            RAEPO = RAEPO * RAD2DG
            DECEPO = DECEPO * RAD2DG
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
  400 CONTINUE
      WRITE (MSGTXT,1500)
      CALL MSGWRT (8)
C                                       Find time of observation
      CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
C                                       Find Julian day, JD
      CALL JULDAY(OBSDAT, JD)
C                                       Find RAAPP, DECAP for
C                                       observation date.
      IEQ = CATBLK(KREPO)
      EPOCH = REQ
      RAEPO = RA * DG2RAD
      DECEPO = DEC * DG2RAD
      GR = .TRUE.
      DIR = 1
      CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *   RAEPO, DECEPO, RAAPP, DECAPP)
      IERR = 0
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
     *   RAEPO, DECEPO, RAAPP, DECAPP)
      CATD(KDCRV+JLOCR) = RAEPO * RAD2DG
      CATD(KDCRV+JLOCD) = DECEPO * RAD2DG
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('SOUMOD: SU table is not modifyed.',
     *   ' Error opening SU table')
 1200 FORMAT ('SOUMOD: SU table is not modifyed. Error reading row',I4)
 1300 FORMAT ('SOUMOD: SU table is not modifyed. Error writing row',I4)
 1400 FORMAT ('SOUMOD: Error closing SU table')
 1500 FORMAT ('SU table does not exist. So the source coordinates at',
     *   'the header are modified' )
      END
      SUBROUTINE GETINP (FILE, IERR)
C-----------------------------------------------------------------------
C  This subroutine reads, from an input file specified by name "file",
C  the list of the vertical atmosphere delays for the given antenna
C  and time.
C  The first (ascii) data record should specify NLINES, the number of
C  the lines.

C  Then, NLINES records must follow, each with three numbers:
C    The station number;
C    The time in dd:hh:mm:ss;
C    The vertical delay error in cm;
C   Inputs:
C    FILE     C*48  File name
C   Outputs in common:
C    NLINES    I     Number of data lines
C    IAN(*)    I     The antenna number
C    TMES(*)   D     The measurement time, in days
C    VERDEL(*) R     The atmosphere vertical delay, in cm
C   Outputs:
C    IERR     I     Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      INTEGER   I, IERR
      INTEGER   LUN, FIND, NBYTES, KBP
      LOGICAL   F
C
      INTEGER  DD, HH, MM, JT, JTRIM
      REAL SS
      CHARACTER LINE*80
      DOUBLE PRECISION X
      INCLUDE 'DFCOR.INC'
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
         IAN(I) = X
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
         WRITE (MSGTXT,2001) I, IAN(I), DD, HH, MM, SS, TMES(I),
     *      VERDEL(I)
         CALL MSGWRT (6)
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
 2001 FORMAT (I4, I3, 1X, 3(I2,':'), F4.1, 2X, F10.8, F5.2)
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
C      ISTOK     I    Polarization to correct, 1=first, 2=second,
C                    0 = both
C   Output:
C      IERR      I    Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LSTSOU, THSOU, LUN, IANT, I, ITEMP
      REAL      XT, YT, PDLY, DPDLY, CFAC, SFAC, FQFAC, ZA, ELV, HA,
     *   VERINT, DELL, DELR, DELDT, AZ, PDLYC, DPDLYC, DDELL, DDELR
      DOUBLE PRECISION FREQS, SINLAT, COSLAT, TCLT, TMESA,
     *   TLEFT, TRIGHT
C                                       speed of light (meters/sec)
      DOUBLE PRECISION VELITE
      PARAMETER (VELITE = 2.997924562D8)
      INCLUDE 'DFCOR.INC'
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
      PDLYC = 0
      DPDLYC = 0
C                                       Interpolate the calibrator
C                                       delays (rate) to the source time
C                                       DO it only for the second read
C                                       of the CL table, when the
C                                       calibrator data have been read
      IF (NSOCWD.EQ.0) THEN
C                                       do not correct calibrator???
C         IF (CLRECI(SOUCL).EQ.NCALID) GO TO 999
         TLEFT = 0
         TRIGHT = 100
         DO 90 I = 1, NCALLI
            TMESA = ATIME(I)
            IF (IANT.EQ.AANT(I)) THEN
               IF ((TMESA.GE.TCLT) .AND. (TMESA.LE.TRIGHT)) THEN
                  TRIGHT = TMESA
                  DELR = APDLY(I)
                  DDELR = ADPDLY(I)
                  END IF
               IF ((TMESA.LE.TCLT) .AND. (TMESA.GE.TLEFT)) THEN
                  TLEFT = TMESA
                  DELL = APDLY(I)
                  DDELL = ADPDLY(I)
                  END IF
               END IF
   90       CONTINUE
         IF (TRIGHT.EQ.100) THEN
            IF (TLEFT.EQ.0) THEN
               PDLYC = 0
               DPDLYC = 0
            ELSE
               PDLYC = DELL
               DPDLYC = DDELL
               END IF
         ELSE
            IF (TLEFT.EQ.0) THEN
               PDLYC = DELR
               DPDLYC = DDELR
            ELSE
C                                       make the interpolation itself
               IF (TRIGHT.EQ.TLEFT) THEN
                  PDLYC = DELL
                  DPDLYC = DDELL
               ELSE
                  PDLYC = DELL + (DELR-DELL) * (TCLT-TLEFT)/(TRIGHT
     *               -TLEFT)
                  DPDLYC = DDELL + (DDELR-DDELL) *
     *            (TCLT-TLEFT)/(TRIGHT-TLEFT)
                  END IF
               END IF
            END IF
         END IF
C                                       Interpolate the vertical
C                                       atmosphere delay given at the
C                                       array VERDEL(*) read from the
C                                       INFILE
      TLEFT = 0
      TRIGHT = 100
      DO 100 I = 1, NLINES
         TMESA = TMES(I)
         IF (IANT.EQ.IAN(I)) THEN
            IF ((TMESA.GT.TCLT) .AND. (TMESA.LT.TRIGHT)) THEN
               TRIGHT = TMESA
               DELR = VERDEL(I)
               END IF
            IF ((TMESA.LT.TCLT) .AND. (TMESA.GT.TLEFT)) THEN
               TLEFT = TMESA
               DELL = VERDEL(I)
               END IF
            END IF
  100    CONTINUE
      IF (TRIGHT.EQ.100) THEN
         IF (TLEFT.EQ.0) THEN
            VERINT = 0
         ELSE
            VERINT = DELL
            END IF
      ELSE
         IF (TLEFT.EQ.0) THEN
            VERINT = DELR
         ELSE
C                                       make the interpolation itself
            VERINT = DELL + (DELR-DELL) * (TCLT-TLEFT)/(TRIGHT-TLEFT)
            END IF
         END IF
C
C                                       VERINT is the interpolated
C                                       vertical atmosphere delay
C                                       at time of the CL table line
C
C                                       previous source ID
      LSTSOU = PARM(1) + 0.5
C                                       source ID at the CL table line
      THSOU = CLRECI(SOUCL)
      IF (LSTSOU.NE.THSOU) THEN
C                                       Get source info
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       it was here. error
         PARM(1) = THSOU
         FREQS = FREQ + FREQO(BIF)
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
         END IF
C
      CALL SOUELV (IANT, CLRECD(TIMCL), HA, ELV, AZ)
C                                       zenith angle of the source
C                                       from the antenna
      ZA = (1.570796327 - ELV)
      COSLAT = COS (STNLAT(IANT))
      SINLAT = SIN (STNLAT(IANT))
C                                       The mapping function is
C                                       described by sec(ZA)
      IF (ELV.GT.0.05) THEN
         PDLY = ((VERINT/100) / SIN(ELV)) / VELITE
      ELSE
         PDLY = 0
         END IF
C                                       DELDT is derivative of ELV
C                                       (by time) multiplied by
C                                       COS(ELV)
      DELDT = -7.29211E-5 * SIN (HA) * COSLAT * COSDEC
C
      IF (ELV.GT.0.05) THEN
         DPDLY = -PDLY / SIN(ELV) * DELDT
      ELSE
         DPDLY = 0
         END IF
C                                       store arrays of PDLY and DPDLY
C                                       for the calibrator source
C                                       CALSOUR has to include only one
C                                       source
      IF (NSOCWD.EQ.1) THEN
         APDLY(FIXCNT) = PDLY
         ADPDLY(FIXCNT) = DPDLY
         ATIME(FIXCNT) = TCLT
         AANT(FIXCNT) = IANT
Ctemporally!!!!!!!!!
C         WRITE (MSGTXT,1000) FIXCNT, ATIME(FIXCNT), AANT(FIXCNT),
C     *      APDLY(FIXCNT), ADPDLY(FIXCNT)
C         CALL MSGWRT (8)
C 1000    FORMAT (I5, 1X, F8.6, 1X, I2, 1X, 1PE12.5, 1X, 1PE12.5)
         GO TO 999
         END IF
C                                       delay as the difference of the
C                                       source and calibrator delay
      PDLY = PDLY - PDLYC
C                                       delay rate  as the difference
C                                       of the source and calibrator
C                                       delay rate
C
      DPDLY = DPDLY - DPDLYC
C                                       Atmospheric group delay
      IF (CLRECR(ATMCL).NE.FBLANK) CLRECR(ATMCL) = CLRECR(ATMCL) - PDLY
C                                       Atmospheric group delay rate
      IF (CLRECR(DATMCL).NE.FBLANK) CLRECR(DATMCL) = CLRECR(DATMCL) -
     *   DPDLY
      IF (ISTOK.EQ.2) GO TO 650
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
     *      CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
         IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *      CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600     CONTINUE
 650     IF (ABS(ISTOK).EQ.1) GO TO 999
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
     *      CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + PDLY
C                                       Phase rate
         IF (CLRECR(RA2CL+I-1).NE.FBLANK)
     *      CLRECR(RA2CL+I-1) = CLRECR(RA2CL+I-1) + DPDLY
 700     CONTINUE
C
 999  RETURN
      END
