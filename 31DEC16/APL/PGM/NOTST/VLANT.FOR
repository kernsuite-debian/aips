LOCAL INCLUDE 'VLANT.INC'
C                                       Include for VLANT
C                                       Inputs
      HOLLERITH XNAMEI(3), XCLAIN(2), XVCODE
      REAL      XSEQ, XDISK, XSUBA, XGVER,XDOINV
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQ, XDISK, XSUBA, XGVER, XDOINV,
     *   XVCODE
C                                       control parameters
      INTEGER   SEQIN, DISKIN, CNOIN, SUBA, CLVER, CLUSE, SCRTCH(256),
     *   TABUFF(512), NUMHIS
      CHARACTER NAMEIN*12, CLAIN*6, VCODE*4, HISCRD(10)*64
      COMMON /CONPRM/ TABUFF, SCRTCH, SEQIN, DISKIN, CNOIN, SUBA, CLVER,
     *   CLUSE, NUMHIS
C                                       baseline corrections
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION FRQOFF(MAXIF), JDOBS, JDB, JDE, JD
      REAL      XCOR(MAXANT), YCOR(MAXANT), ZCOR(MAXANT)
      LOGICAL   ANDONE(MAXANT), ISEVLA, DOANTE, DOELEV
      CHARACTER STNID(MAXANT)*3
      COMMON /CONDAT/ FRQOFF, JDOBS, JDB, JDE, JD, XCOR, YCOR,
     *   ZCOR, ANDONE, ISEVLA, DOANTE, DOELEV
      COMMON /CONCHR/ NAMEIN, CLAIN, VCODE, STNID, HISCRD
C                                       CL table stuff
      INCLUDE 'INCS:PCLTAB.INC'
      INTEGER   CLRECI(13+32*MAXIF), CLKOLS(MAXCLC), CLNUMV(MAXCLC),
     *   NUMANT, NUMPOL, NUMIF, ICODE, FIXCNT, TIMCL,  INTCL, SOUCL,
     *   ANTCL, SUBCL, FRQCL, IFRCL, GDLCL, DOPCL, ATMCL, DATMCL,
     *   MBD1CL, CLK1CL, DCK1CL, DIS1CL, DDS1CL, RE1CL, IM1CL, DE1CL,
     *   RA1CL, WE1CL, RF1CL, MBD2CL, CLK2CL, DCK2CL, DIS2CL, DDS2CL,
     *   RE2CL, IM2CL, DE2CL, RA2CL, WE2CL, RF2CL, NTERM, FREQID,
     *   LSTSOU, LSTQID
      REAL      GMMOD, CLRECR(13+32*MAXIF), PARM(40), PANGLE(MAXANT),
     *   AXOFF
      DOUBLE PRECISION COSDEC, SINDEC, CLRECD(13+32*MAXIF)
C                                       Internal storage
      COMMON /CLRECC/ COSDEC, SINDEC, CLRECD, GMMOD, PARM, PANGLE,
     *   AXOFF, NTERM, FIXCNT, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *   ICODE, FREQID, LSTSOU, LSTQID
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
C                                       standard includes
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
LOCAL END
      PROGRAM VLANT
C-----------------------------------------------------------------------
C! Applies VLA antenna position corrections to the CL table.
C# UV Calibration EXT-appl VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 2007-2010, 2012, 2016
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
C   Task VLANT applies corrections to CL tables for Operator-entered
C   antenna location corrections appropriate to the data.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'VLANT.INC'
      DATA PRGM /'VLANT '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL VLANIN (PRGM, IRET)
C                                       Apply corrections antennas
      IF ((IRET.EQ.0) .AND. (DOANTE)) CALL VLANCL (IRET)
C                                       Apply corrections elevation
      IF ((IRET.EQ.0) .AND. (DOELEV)) CALL FXEUV (IRET)
C                                       Copy and update HI file.
      IF (IRET.EQ.0) CALL VLANHI
C                                       Close down files, etc.
      IRET = MAX (0, IRET)
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE VLANIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   VLANIN gets the user's parameters, finds the input data file,
C   determines which AN table to use and reads in existing values,
C   determines the observation date, reads the VLA baseline correction
C   files summing up any corrections to apply to the data.
C   Note that this may require going to the web to obtain the latest
C   observer correction file.
C   Inputs:
C      PRGM   C*6   task name to use
C   Outputs:
C      IRET   I     0 => all okay and there are corrections
C                   -1 => all okay but no corrections are needed
C                   > 0 something bad happened
C-----------------------------------------------------------------------
      CHARACTER PRGM*(*)
      INTEGER   IRET
C
      INCLUDE 'VLANT.INC'
      INTEGER   IERR, NPARM, IROUND, NUMCL, BUFFER(512), LUN, LUN2, I,
     *   TIMLIM(6,2)
      DOUBLE PRECISION TIMOFF
      CHARACTER STAT*4, UTYPE*2, OBDATE*8
      INCLUDE 'INCS:DANT.INC'
      DATA LUN, LUN2 /29, 28/
      DATA TIMLIM /2016, 08, 09, 00, 00, 00,
     *             2016, 11, 15, 00, 00, 00/
C-----------------------------------------------------------------------
C                                       system inits
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      NUMHIS = 0
      IRET = 0
C                                       get inputs
      NPARM = 11
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       crunch input parms
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XVCODE, VCODE)
      SEQIN = IROUND (XSEQ)
      DISKIN = IROUND (XDISK)
      SUBA = IROUND (XSUBA)
      SUBA = MAX (1, SUBA)
      IF (XDOINV.GT.0.0) THEN
         XDOINV = 1.0
      ELSE
         XDOINV = -1.0
         END IF
C                                       find UV file, header
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       determine number of CL tables
      CALL FNDEXT ('CL', CATBLK, NUMCL)
      IF (NUMCL.LE.0) THEN
         MSGTXT = 'NO CL TABLES FOUND, CANNOT CLCOR'
         IRET = 1
         GO TO 990
         END IF
      CLVER = IROUND (XGVER)
      IF ((CLVER.LE.0) .OR. (CLVER.GT.NUMCL)) CLVER = NUMCL
      CLUSE = NUMCL + 1
C                                       Get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBA, CATBLK, TABUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (ANAME.EQ.'VLA') THEN
         ISEVLA = .FALSE.
      ELSE IF (ANAME.EQ.'EVLA') THEN
         ISEVLA = .TRUE.
      ELSE
         IERR = 10
         MSGTXT = 'I WORK FOR VLA AND EVLA, NOT ' // ANAME
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       convert the station IDs
      CALL LFILL (MAXANT, .TRUE., ANDONE)
      DO 10 I = 1,NSTNS
         STNID(I) = ' '
         IF (ISEVLA) THEN
            STNID(I) = STNNAM(I)(1:3)
            IF (STNID(I)(2:2).EQ.'0') STNID(I)(2:) = STNNAM(I)(3:3)
         ELSE IF (STNNAM(I)(:5).EQ.'EVLA:') THEN
            STNID(I) = STNNAM(I)(6:8)
         ELSE IF (STNNAM(I)(:5).EQ.'VLA: ') THEN
            STNID(I) = STNNAM(I)(6:8)
         ELSE IF (STNNAM(I)(:5).EQ.'VLA:_') THEN
            STNID(I) = STNNAM(I)(6:8)
         ELSE IF (STNNAM(I)(:4).EQ.'VLA:') THEN
            STNID(I) = STNNAM(I)(5:7)
            END IF
         IF ((STNID(I).NE.' ') .AND. (STNID(I).NE.'OUT')) ANDONE(I) =
     *      .FALSE.
 10      CONTINUE
      CALL JULDAY (RDATE, JDOBS)
C                                       dates for elevation correction
      CALL DAT2JD (TIMLIM(1,1), JDB)
      CALL DAT2JD (TIMLIM(1,2), JDE)
      CALL H2CHR (8, 1, CATH(KHDOB), OBDATE)
      CALL JULDAY (OBDATE, JD)
      IF ((JDB-JD.GT.0.1D0) .OR. (JD-JDE.GT.0.1D0)) THEN
         DOELEV = .FALSE.
      ELSE
         DOELEV = .TRUE.
         END IF
      IF (VCODE.EQ.'NOEL') DOELEV = .FALSE.
      DOANTE = VCODE.NE.'NOAN'
C                                       get start time
      IF (DOANTE) THEN
         CALL STRTIM (DISKIN, CNOIN, CATBLK, SUBA, TIMOFF, IRET)
         IF (IRET.NE.0) GO TO 999
         JDOBS = JDOBS + TIMOFF
C                                       get the corrections
         CALL GETCOR (IRET)
         IF (IRET.NE.0) THEN
            DOANTE = .FALSE.
            IF (.NOT.DOELEV) GO TO 999
            END IF
         END IF
C                                       copy CLVER table to CLUSE table
      CALL TABCOP ('CL', CLVER, CLUSE, LUN, LUN2, DISKIN, DISKIN, CNOIN,
     *   CNOIN, CATBLK, BUFFER, TABUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      WRITE (MSGTXT,1025) CLVER, CLUSE
      CALL MSGWRT (4)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLANIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1015 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1020 FORMAT ('ERROR: COPYING INPUT CL TO OUTPUT:',I4)
 1025 FORMAT ('CL version input',I4,' output',I4)
      END
      SUBROUTINE STRTIM (IVOL, ISLOT, CATBLK, OSUB, TSTART, IRET)
C-----------------------------------------------------------------------
C   Input:
C      IVOL     I        Volume number on which data resides
C      ISLOT    I        Catalogue number of data
C      CATBLK   I(256)   Catalogue header
C      OSUB     I        > 0 => restrict to this subarray
C   Output:
C      TSTART   D        Start time (days)
C      IRET     I        Return code, 0=>OK, otherwise INDEX file
C                        exists but cannot be read.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IVOL, ISLOT, CATBLK(256), OSUB, IRET
      DOUBLE PRECISION TSTART
C
      INTEGER   JERR, NXSUBA, I, FREQID, INXRNO, IXLUN, NXBUFF(512),
     *   NXKOLS(MAXNXC), NXNUMV(MAXNXC), NINDEX, NXIDSO, NXSTA, NXEND
      LOGICAL   TABLE, EXIST, FITASC
      REAL      NXTIME, NXDTIM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Initialize output
      IXLUN = 30
C                                       See if NX file exists.
      CALL ISTAB ('NX', IVOL, ISLOT, 1, IXLUN, NXBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) THEN
         IRET = 1
         MSGTXT = 'NX TABLE IS REQUIRED: RUN INDXR'
         GO TO 990
         END IF
C                                       Open NX table
      CALL NDXINI ('READ', NXBUFF, IVOL, ISLOT, 1, CATBLK, IXLUN,
     *   INXRNO, NXKOLS, NXNUMV, JERR)
      IF (JERR.GT.0) THEN
         IRET = 2
         WRITE (MSGTXT,1000) JERR
         GO TO 990
         END IF
C                                       Get number of scans
      NINDEX = NXBUFF(5)
C                                       Check if empty
      IF (NINDEX.LE.0) THEN
         MSGTXT = 'NX TABLE EXISTS BUT IS EMPTY: RUN INDXR'
         IRET = 3
         GO TO 990
         END IF
C                                       Locate first selected scan.
      IRET = 4
      TSTART = -100.D0
      DO 20 I = 1,NINDEX
         INXRNO = I
         CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, NXTIME,
     *      NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID, JERR)
         IF (JERR.GT.0) THEN
            WRITE (MSGTXT,1020) JERR
            GO TO 990
            END IF
         IF (JERR.EQ.0) THEN
            IF (((OSUB.LE.0) .OR. (NXSUBA.LE.0) .OR. (NXSUBA.EQ.OSUB)))
     *         THEN
               TSTART = NXTIME - 0.5 * NXDTIM
               GO TO 30
            ELSE IF (TSTART.LT.-90.D0) THEN
               TSTART = NXTIME - 0.5 * NXDTIM
               END IF
            END IF
 20      CONTINUE
      MSGTXT = 'REQUESTED SUBARRAY NOT FOUND IN NX TABLE'
      CALL MSGWRT (6)
      MSGTXT = 'USING WHAT WAS FOUND'
      CALL MSGWRT (6)
C                                       close file
 30   CALL TABIO ('CLOS', 0, INXRNO, NXBUFF, NXBUFF, JERR)
      IRET = 0
      GO TO 999
C                                       Errors
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVTIME: ERROR',I3,' OPENING INDEX TABLE')
 1020 FORMAT ('UVTIME: ERROR',I3,' READING INDEX TABLE')
      END
      SUBROUTINE GETCOR (IRET)
C-----------------------------------------------------------------------
C   GETCOR determines the corrections to be applied to an observation
C   on date JDOBS
C   Output:
C      IRET   I   Error code: 0 none, -1 no corrections, else die
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'VLANT.INC'
      INTEGER   IT(6), YEAR, YEAR0, YEAR1, ALDONE, LUN, FIND, MSGSAV,
     *   IERR, I, MD(6), PD(6), IA, GETMON, JJ, JTRIM
      REAL      BX, BY, BZ
      CHARACTER ANFILE*48, LINE*132, MMON*3, PMON*3, STN*3, RMFILE*48
      DOUBLE PRECISION JDN, JDP
      DATA LUN /3/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                       current year
      CALL ZDATE (IT)
      YEAR1 = IT(1)
C                                       starting year
      CALL JD2DAT (JDOBS, IT)
      YEAR0 = IT(1)
C                                       check for missing data
      IF (YEAR0.LT.1992) THEN
         WRITE (MSGTXT,1000) YEAR0
         IRET = 1
         GO TO 990
         END IF
      IF ((YEAR0.EQ.1994) .AND. (IT(2).EQ.11) .AND. (IT(3).GE.23)
     *   .AND. (IT(3).LE.28)) THEN
         MSGTXT = 'ANT DATA MEANINGLESS FOR NOV 23-28, 1994'
         IRET = 1
         GO TO 990
         END IF
C                                       loop over years
      ALDONE = 0
      DO 10 I = 1,MAXANT
         IF (.NOT.ANDONE(I)) ALDONE = ALDONE + 1
         XCOR(I) = 0.0
         YCOR(I) = 0.0
         ZCOR(I) = 0.0
 10      CONTINUE
      IF (ISEVLA) THEN
         MSGTXT = 'EVLA OBSERVATION PRIOR TO 2010 ???'
         IF (YEAR0.LT.2010) CALL MSGWRT (7)
         YEAR0 = MAX (YEAR0, 2010)
      ELSE
         MSGTXT = 'VLA CORRECTIONS TERMINATE WITH 2010'
         IF (YEAR1.GT.2010) CALL MSGWRT (7)
         YEAR1 = 2010
         END IF
      DO 100 YEAR = YEAR0,YEAR1
         IF (ISEVLA) THEN
            WRITE (ANFILE,1010) YEAR
         ELSE
            WRITE (ANFILE,1011) YEAR
            END IF
         MSGSUP = 32000
         CALL ZTXOPN ('QRED', LUN, FIND, ANFILE, .FALSE., IERR)
         MSGSUP = MSGSAV
C                                       get via internet
         RMFILE = ' '
         IF (IERR.EQ.5) CALL TFETCH (ISEVLA, LUN, FIND, YEAR, RMFILE,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1015) IERR, 'OPEN', YEAR
            IRET = 2
            GO TO 990
            END IF
         MD(4) = 0
         MD(5) = 0
         MD(6) = 0
         PD(6) = 0
C                                       read loop
 20      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.EQ.0) THEN
            JJ = JTRIM (LINE)
            IF ((JJ.GT.1) .AND. (LINE(1:1).NE.';')) THEN
               MD(1) = YEAR
               PD(1) = YEAR
               READ (LINE,1020,ERR=900) MMON, MD(3), PMON, PD(3), PD(4),
     *            PD(5), IA, STN, BX, BY, BZ
               IF ((IA.LT.1) .OR. (IA.GT.29)) THEN
                  WRITE (MSGTXT,1025) IA, YEAR
                  CALL MSGWRT (7)
                  GO TO 20
                  END IF
C                                       new EVLA name - fix to old
               IF (STN(2:2).EQ.'0') STN(2:) = STN(3:3)
C                                       put in date
               PD(2) = GETMON (PMON)
               CALL DAT2JD (PD, JDP)
C                                       if moved
               IF (MMON.NE.' ') THEN
C                                       moved date
                  MD(2) = GETMON (MMON)
                  CALL DAT2JD (MD, JDN)
C                                       sometimes move last year
                  IF (JDN.GT.JDP) THEN
                     MD(1) = MD(1) - 1
                     CALL DAT2JD (MD, JDN)
                     END IF
C                                       after obs
                  IF (JDN.GT.JDOBS) THEN
                     IF (.NOT.ANDONE(IA)) ALDONE = ALDONE - 1
                     ANDONE(IA) = .TRUE.
                     IF (ALDONE.LE.0) THEN
                        CALL ZTXCLS (LUN, FIND, IERR)
                        GO TO 110
                        END IF
C                                       before obs
                  ELSE
                     XCOR(IA) = 0.0
                     YCOR(IA) = 0.0
                     ZCOR(IA) = 0.0
                     END IF
                  END IF
C                                       moved before, put in after
               IF ((JDP.GT.JDOBS) .AND. (.NOT.ANDONE(IA)) .AND.
     *                  (STN.EQ.STNID(IA))) THEN
                  XCOR(IA) = XCOR(IA) + BX
                  YCOR(IA) = YCOR(IA) + BY
                  ZCOR(IA) = ZCOR(IA) + BZ
                  END IF
               END IF
            GO TO 20
C                                       IO error
         ELSE IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1015) IERR, 'READ', YEAR
            IRET = 2
            GO TO 990
            END IF
         CALL ZTXCLS (LUN, FIND, IERR)
         IF (RMFILE.NE.' ') THEN
            CALL ZTXZAP (LUN, RMFILE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1090) IERR, RMFILE
               CALL MSGWRT (7)
               END IF
            END IF
 100     CONTINUE
C                                       some still on pads?
      IF (ALDONE.GT.0) THEN
         WRITE (MSGTXT,1100) ALDONE
         CALL MSGWRT (6)
         END IF
C                                       any corrections?
 110  IRET = 0
      IF (XDOINV.GT.0.0) THEN
         DO 115 I = 1,MAXANT
            XCOR(I) = -XCOR(I)
            YCOR(I) = -YCOR(I)
            ZCOR(I) = -ZCOR(I)
 115        CONTINUE
         END IF
      DO 120 I = 1,MAXANT
         IF ((XCOR(I).NE.0.0) .OR. (YCOR(I).NE.0.0) .OR.
     *      (ZCOR(I).NE.0.0)) GO TO 999
 120     CONTINUE
      MSGTXT = 'NO ANTENNA NEEDED CORRECTION'
      IRET = -1
      GO TO 990
C
 900  CALL ZTXCLS (LUN, FIND, IERR)
      WRITE (MSGTXT,1900) YEAR
      CALL MSGWRT (8)
      IRET = 8
      MSGTXT = LINE
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANT DATA UNAVAILABLE FOR YEAR',I5,
     *   ' DATA START WITH 1992')
 1010 FORMAT ('AIPSTARS:EVLA.ant.',I4)
 1011 FORMAT ('AIPSTARS:VLA.ant.',I4)
 1015 FORMAT ('ERROR',I5,1X,A,'ING FILE FOR YEAR',I5)
 1020 FORMAT (A3,I2,13X,A3,I2,I4,1X,I2,I6,3X,A3,3F8.4)
 1025 FORMAT ('ANTENNA NUMBER',I6,' NOT CORRECT: YEAR',I5)
 1090 FORMAT ('ERROR',I4,' DELETING ',A)
 1100 FORMAT ('WARNING:',I3,' ANTENNAS MAY STILL GET MORE CORRECTION')
 1900 FORMAT ('FORMAT ERROR IN YEAR',I6)
      END
      INTEGER FUNCTION GETMON (MON)
C-----------------------------------------------------------------------
C   translates month character string to number
C   Inputs:
C      MON      C*3   Month character (case insensitive)
C   Returns
C      GETMON   I     Month number or 0
C-----------------------------------------------------------------------
      CHARACTER MON*(*)
C
      CHARACTER LMON*3, MONTHS(12)*3
      INTEGER   I
      DATA MONTHS /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     *   'SEP','OCT','NOV','DEC'/
C-----------------------------------------------------------------------
      LMON = MON
      CALL CHLTOU (3, LMON)
      GETMON = 0
      DO 10 I = 1,12
         IF (LMON.EQ.MONTHS(I)) GETMON = I
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE TFETCH (ISEVLA, LUN, FIND, YEAR, RMFILE, IRET)
C-----------------------------------------------------------------------
C   TFETCH gets the data file via the internet for VLA changes during
C   year YEAR
C   Inputs:
C      LUN    I   LUN to use
C      YEAR   I   Year needed
C   Outputs:
C      FIND   I   FTAB pointer for opened file
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, YEAR, IRET
      LOGICAL   ISEVLA
      CHARACTER RMFILE*(*)
C
      CHARACTER SYSOUT*12, FILEN*20, COMMND*256, FUNC(2)*8
      INTEGER   JTRIM, CLEN, SLEN, IDATE(3), J, II
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA FUNC  /'wget -O ', 'curl -o '/
C-----------------------------------------------------------------------
      SYSOUT = '/dev/null'
      SLEN = 0
      CALL ZDATE (IDATE)
      II = 1
      IF (SYSVER(1:3).EQ.'MAC') II = 2
C                                       is there a wget?
      COMMND = 'which ' // FUNC(II)(:4) // ' 1> /dev/null 2> /dev/null'
      CLEN = JTRIM (COMMND)
      CALL ZSHCMD (CLEN, COMMND, SLEN, SYSOUT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'TFETCH: CANNOT FIND COMMAND ' // FUNC(II)(:4)
         GO TO 100
         END IF
C                                       okay go get it
      IF (YEAR.LT.IDATE(1)) THEN
         IF (ISEVLA) THEN
            WRITE (FILEN,1000) YEAR
         ELSE
            WRITE (FILEN,1010) YEAR
            END IF
      ELSE
         J = MOD (YEAR,100)
         IF (ISEVLA) THEN
            WRITE (FILEN,1001) J
         ELSE
            WRITE (FILEN,1011) J
            END IF
         END IF
      J = JTRIM (FILEN)
      RMFILE = '/tmp/' // FILEN(:J)
      COMMND = FUNC(II) // '/tmp/' // FILEN(:J) //
     *   ' ftp://ftp.aoc.nrao.edu/pub/VLA/baselines/' // FILEN
     *   // ' 1> /dev/null 2> /dev/null'
      CLEN = JTRIM (COMMND)
      CALL ZSHCMD (CLEN, COMMND, SLEN, SYSOUT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'TFETCH: ' // FUNC(II)(:4) //
     *      ' OF DATA FILE FAILED AFTER ' // FUNC(II)(:4)
         GO TO 100
         END IF
C                                       double check
      COMMND = 'find /tmp/' // FILEN // ' 1> /dev/null 2> /dev/null'
      CLEN = JTRIM (COMMND)
      CALL ZSHCMD (CLEN, COMMND, SLEN, SYSOUT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'TFETCH: DATA FILE NOT FOUND IN /tmp AFTER ' //
     *      FUNC(II)(:4)
         GO TO 100
         END IF
C                                       open the file
      COMMND = '/tmp/' // FILEN
      CALL ZTXOPN ('QRED', LUN, FIND, COMMND, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'TFETCH: OPEN OF /tmp FILE FAILED'
         GO TO 100
      ELSE
         CLEN = JTRIM (COMMND)
         MSGTXT = COMMND(:CLEN) // ' copied from the VLA web site'
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       try command
 100  CALL MSGWRT (6)
      II = 3 - II
      MSGTXT = 'Trying ' // FUNC(II)(:4) // ' instead'
      CALL MSGWRT (6)
C                                       okay go get it
      IF (YEAR.LT.IDATE(1)) THEN
         IF (ISEVLA) THEN
            WRITE (FILEN,1000) YEAR
         ELSE
            WRITE (FILEN,1010) YEAR
            END IF
      ELSE
         J = MOD (YEAR,100)
         IF (ISEVLA) THEN
            WRITE (FILEN,1001) J
         ELSE
            WRITE (FILEN,1011) J
            END IF
         END IF
      J = JTRIM (FILEN)
      RMFILE = '/tmp/' // FILEN(:J)
      COMMND = FUNC(II) // '/tmp/' // FILEN(:J) //
     *   ' ftp://ftp.aoc.nrao.edu/pub/VLA/baselines/' // FILEN(:J)
     *   // ' 1> /dev/null 2> /dev/null'
      CLEN = JTRIM (COMMND)
      CALL ZSHCMD (CLEN, COMMND, SLEN, SYSOUT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'TFETCH: ' // FUNC(II)(:4) // ' OF DATA FILE FAILED'
         GO TO 990
         END IF
C                                       double check
      COMMND = 'find /tmp/' // FILEN // ' 1> /dev/null 2> /dev/null'
      CLEN = JTRIM (COMMND)
      CALL ZSHCMD (CLEN, COMMND, SLEN, SYSOUT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'TFETCH: DATA FILE NOT FOUND IN /tmp AFTER ' //
     *      FUNC(II)(:4)
         GO TO 990
         END IF
C                                       open the file
      COMMND = '/tmp/' // FILEN
      CALL ZTXOPN ('QRED', LUN, FIND, COMMND, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'TFETCH: OPEN OF /tmp FILE FAILED AFTER ' //
     *      FUNC(II)(:4)
         GO TO 990
      ELSE
         CLEN = JTRIM (COMMND)
         MSGTXT = COMMND(:CLEN) // ' copied from the VLA web site'
         CALL MSGWRT (3)
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EVLA.ant.',I4.4)
 1001 FORMAT ('baselines.evlais',I2.2)
 1010 FORMAT ('VLA.ant.',I4.4)
 1011 FORMAT ('baselines.vlais',I2.2)
      END
      SUBROUTINE VLANCL (IRET)
C-----------------------------------------------------------------------
C   VLANCL applies the corrections determined by VLANIN.
C   Outputs:
C      IRET   I   Error code: > 0 => bad error, < 0 => just quit
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUN, LOOP, IRCODE, ANT, ICLRNO, NUMREC
      INCLUDE 'VLANT.INC'
      DATA LUN /29/
C-----------------------------------------------------------------------
      IRET = 0
      FIXCNT = 0
C                                       Open CL table
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Reformat table?
      CALL CLREFM (DISKIN, CNOIN, CLUSE, CATBLK, LUN, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL CALINI ('WRIT', TABUFF, DISKIN, CNOIN, CLUSE, CATBLK, LUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get number of records
      NUMREC = TABUFF(5)
      IF (NUMREC.LE.0) GO TO 999
      IRCODE = 0
      LSTSOU = -1
      LSTQID = -1
C                                       Update table
      DO 50 LOOP = 1,NUMREC
         ICLRNO = LOOP
         CALL TABIO ('READ', IRCODE, ICLRNO, CLRECR, TABUFF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', LOOP
            GO TO 990
            END IF
         IF (IRET.LT.0) GO TO 50
C                                       Check data
C                                       Subarray
         IF ((CLRECI(SUBCL).NE.SUBA) .AND. (CLRECI(SUBCL).GT.0))
     *      GO TO 50
C                                       Check antenna
         ANT = CLRECI(ANTCL)
         IF ((XCOR(ANT).EQ.0.0) .AND. (YCOR(ANT).EQ.0.0) .AND.
     *      (ZCOR(ANT).EQ.0.0)) GO TO 50
C                                       get correction
         CALL ANTCOR (ANT, IRET)
         IF (IRET.NE.0) GO TO 50
C                                       Rewrite record
         CALL TABIO ('WRIT', IRCODE, ICLRNO, CLRECR, TABUFF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRIT', LOOP
            GO TO 990
            END IF
 50      CONTINUE
C                                       Close table.
      CALL TABIO ('CLOS', IRCODE, LOOP, CLRECR, TABUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOS', NUMREC
         GO TO 990
         END IF
C                                       Update the AN table
      CALL ANTMOD (IRET)
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABIO ERROR',I3,1X,A,'ING CL TABLE ROW',I8)
      END
      SUBROUTINE ANTCOR (ANT, IRET)
C-----------------------------------------------------------------------
C   Routine to correct for errors in antenna position.
C   Corrections are applied to the CL record in Common.
C   Inputs:
C      ANT    I   Current antenna number
C   Output:
C      IRET   I   Return error code , 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   ANT, IRET
C
      INCLUDE 'VLANT.INC'
      INTEGER   THSOU, LUN, I, ITEMP, THSQID, DUMMY(MAXIF), FQBUFF(512),
     *   IIVER
      REAL      XT, YT, CFAC, SFAC, FQFAC, EL, FINC(MAXIF)
      DOUBLE PRECISION CHAD, SHAD, HA, HAD, FREQS, CIR, CI, RADSEC,
     *   PDLY, DPDLY, HRANG
      CHARACTER BNDCOD(MAXIF)*8
C                                       CI = 1/speed of light
      PARAMETER (CI = 1.0D0 / VELITE)
C                                       RADSEC = earth rot rate in
C                                       rad/sec.
      PARAMETER (RADSEC = PI / 43200.0D0)
      SAVE FREQS
      DATA LUN /30/
C-----------------------------------------------------------------------
      THSOU = CLRECI(SOUCL)
      THSQID = CLRECI(FRQCL)
C                                       Get source info
      IF (LSTSOU.NE.THSOU) THEN
         CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IRET)
         IF (IRET.NE.0) GO TO 999
         LSTSOU = THSOU
         FREQS = FREQ + FREQO(1)
C                                       Declination
         SINDEC = SIN (DECAPP)
         COSDEC = COS (DECAPP)
         END IF
C                                       Get IF information
      IF (LSTQID.NE.THSQID) THEN
         IIVER = 1
         CALL CHNDAT ('READ', FQBUFF, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *      NUMIF, FRQOFF, DUMMY, FINC, BNDCOD, THSQID, IRET)
         IF (IRET.NE.0) GO TO 999
         LSTQID = THSQID
         END IF
C                                       HA is double precision im the
C                                       SOUEL output
      CALL SOUEL (ANT, CLRECD(TIMCL), HA, EL)
C                                       Get Greenwich hour angle
C                                       for VLBI and stay local one
C                                       for VLA
      IF ((ABS(ARRAYC(1)).LT.1.D2) .AND. (ABS(ARRAYC(2)).LT.1.D2) .AND.
     *   (ABS(ARRAYC(3)).LT.1.D2)) THEN
         HRANG = HA - STNLON(ANT)
         HRANG = DMOD (HRANG, TWOPI)
         IF (HRANG.GT. PI) HRANG = HRANG - TWOPI
         IF (HRANG.LT.-PI) HRANG = HRANG + TWOPI
         HA = HRANG
         END IF
      HAD = HA
      CHAD = COS (HAD)
      SHAD = SIN (HAD)
      CIR = CI * RADSEC
C                                       Delay and rate in sec and
C                                       sec/sec. (want corrections).
C                                       The formulae are written
C                                       at the Right Hand coordinate
C                                       system.
C                                       Difference formula
      PDLY = CI * ( (XCOR(ANT) * CHAD - YCOR(ANT) * SHAD) * COSDEC +
     *   ZCOR(ANT) * SINDEC)
      DPDLY = CIR * (-XCOR(ANT) * SHAD - YCOR(ANT) * CHAD) * COSDEC
C                                       multiband delay for both stokes
      IF (CLRECR(MBD1CL).NE.FBLANK) CLRECR(MBD1CL) = CLRECR(MBD1CL)
     *      + PDLY
      IF (CLRECR(MBD2CL).NE.FBLANK) CLRECR(MBD2CL) = CLRECR(MBD2CL)
     *      + PDLY
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
     *      CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + PDLY
C                                       Phase rate
         IF (CLRECR(RA1CL+I-1).NE.FBLANK)
     *      CLRECR(RA1CL+I-1) = CLRECR(RA1CL+I-1) + DPDLY
 600     CONTINUE
C                                       2nd polarization if present
      IF (NUMPOL.GT.1) THEN
         DO 700 I = 1,NUMIF
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
C      HA         D    Source hour angle (rad)
C      EL         R    Source elevation (rad)
C   SOUELV with HA as double precision
C-----------------------------------------------------------------------
      INTEGER   ANTNO
      DOUBLE PRECISION TIME, HA
      REAL      EL
C
      DOUBLE PRECISION    HRANG, ANTLST, DARG, DRA, DDEC
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PSTD.INC'
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
      EL = (PI / 2.0D0 - ACOS (DARG))
C
 999  RETURN
      END
      SUBROUTINE ANTMOD (IRET)
C-----------------------------------------------------------------------
C   Subroutine to make the correction of the selected antenna position
C   if OPCODE ='ANTP' and the antenna correction parameters are not
C   equal zero.
C   Outputs:
C      IRET   I   Fatal error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, LUN1, NENTRY
      INCLUDE 'VLANT.INC'
      DATA LUN1 /28/
C-----------------------------------------------------------------------
C                                       Open for write
      CALL ANTINI ('WRIT', TABUFF, DISKIN, CNOIN, SUBA, CATBLK, LUN1,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT FOR WRIT'
         GO TO 990
         END IF
C                                       Loop through AN rows correcting
C                                       the selected antenna position
      NENTRY = TABUFF(5)
      DO 10 I = 1, NENTRY
         IANRNO = I
         CALL TABAN ('READ', TABUFF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1005) IRET, 'READ', I
            GO TO 990
            END IF
C                                       Put the correction here
         STAXYZ(1) = STAXYZ(1) + XCOR(NOSTA)
         STAXYZ(2) = STAXYZ(2) + YCOR(NOSTA)
         STAXYZ(3) = STAXYZ(3) + ZCOR(NOSTA)
         IANRNO = I
         CALL TABAN ('WRIT', TABUFF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1005) IRET, 'WRIT', I
            GO TO 990
            END IF
 10      CONTINUE
C
C                                       Close table
      CALL TABIO ('CLOS', 1, IANRNO, TABUFF, TABUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE FOR WRIT'
         GO TO 990
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANTMOD: ERROR',I3,' ON ',A,'ING')
 1005 FORMAT ('ANTMOD: ERROR',I3,' ON ',A,'ING ROW',I4)
      END
      SUBROUTINE VLANHI
C-----------------------------------------------------------------------
C   VLANHI writes the corrections applied into the HI table
C-----------------------------------------------------------------------
C
      INCLUDE 'VLANT.INC'
      INTEGER   I, DATE(3), TIME(3), LUN1, IRET
      CHARACTER CTIME(2)*12, HILINE*72
      DATA LUN1 /27/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HIOPEN (LUN1, DISKIN, FCNO(NCFILE), SCRTCH, IRET)
      IF (IRET.GT.2) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN'
         CALL MSGWRT (6)
         IRET = -100
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      IF (IRET.EQ.0) CALL HIADD (LUN1, HILINE, SCRTCH, IRET)
C                                       subarray
      WRITE (HILINE,1015) TSKNAM, 'SUBARRAY', SUBA
      IF (IRET.EQ.0) CALL HIADD (LUN1, HILINE, SCRTCH, IRET)
      WRITE (HILINE,1016) TSKNAM, 'DOINVERS', XDOINV
      IF (IRET.EQ.0) CALL HIADD (LUN1, HILINE, SCRTCH, IRET)
      IF (DOANTE) THEN
         HILINE = TSKNAM // ' / Antenna position corrections applied'
      ELSE
         HILINE = TSKNAM // ' / NO antenna position corrections applied'
         END IF
      CALL HIADD (LUN1, HILINE, SCRTCH, IRET)
C                                       corrections
      DO 100 I = 1,MAXANT
         IF ((XCOR(I).NE.0.0) .OR. (YCOR(I).NE.0.0) .OR.
     *      (ZCOR(I).NE.0.0)) THEN
            WRITE (MSGTXT,1020) TSKNAM, I, STNID(I)
            CALL MSGWRT (2)
            HILINE = MSGTXT
            IF (IRET.EQ.0) CALL HIADD (LUN1, HILINE, SCRTCH, IRET)
            WRITE (MSGTXT,1025) TSKNAM, 'X', I, XCOR(I)
            CALL MSGWRT (2)
            HILINE = MSGTXT
            IF (IRET.EQ.0) CALL HIADD (LUN1, HILINE, SCRTCH, IRET)
            WRITE (MSGTXT,1025) TSKNAM, 'Y', I, YCOR(I)
            CALL MSGWRT (2)
            HILINE = MSGTXT
            IF (IRET.EQ.0) CALL HIADD (LUN1, HILINE, SCRTCH, IRET)
            WRITE (MSGTXT,1025) TSKNAM, 'Z', I, ZCOR(I)
            CALL MSGWRT (2)
            HILINE = MSGTXT
            IF (IRET.EQ.0) CALL HIADD (LUN1, HILINE, SCRTCH, IRET)
            END IF
 100     CONTINUE
      IF (DOELEV) THEN
         HILINE = TSKNAM // ' / 2016 EVLA delay correction applied '
         CALL HIADD (LUN1, HILINE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 120
         END IF
C                                      Add any other history.
      IF (NUMHIS.GT.0) THEN
         HILINE = TSKNAM
         DO 110 I = 1,NUMHIS
            HILINE(9:72) = HISCRD(I)(1:64)
            CALL HIADD (LUN1, HILINE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 120
 110        CONTINUE
         END IF
 120  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRIT'
         CALL MSGWRT (6)
         END IF
      IF (IRET.GE.0) CALL HICLOS (LUN1, .TRUE., SCRTCH, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLCLHI: ERROR',I3,1X,A,'ING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1015 FORMAT (A6,A8,' = ',I5)
 1016 FORMAT (A6,A8,' = ',F5.1,' / > 0 corrs below of opposite sign')
 1020 FORMAT (A6,'STNID(',I2,') = ''',A3,'''')
 1025 FORMAT (A6,A1,'COR(',I2,') = ',F9.4)
      END
      SUBROUTINE FXEUV (IERR)
C-----------------------------------------------------------------------
C   FXEUV is called from VLANT. FXEUV reads throught the CL table,
C   passing the records selected to the correction routine FXECOR.
C   Does special EVLA on-line error fix
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN, IRCODE, JERR, ICLRNO, NUMREC, LOOP, BUFFER(1024)
      INCLUDE 'VLANT.INC'
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
      MSGTXT = 'Applying 2016 EVLA atmospheric delay corrections'
      CALL MSGWRT (4)
C                                       Open CL table
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS).GT.1) NUMPOL = 2
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       re-open table
      CALL CALINI ('WRIT', BUFFER, DISKIN, CNOIN, CLUSE, CATBLK, LUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      IRCODE = 0
      LSTSOU = -1
      LSTQID = -1
C                                       Initial call to FXECOR
      CALL FXECOR (1, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Update table
      DO 500 LOOP = 1,NUMREC
         ICLRNO = LOOP
         CALL TABIO ('READ', IRCODE, ICLRNO, CLRECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 900
         IF (IERR.LT.0) GO TO 500
C                                       Subarray
         IF ((CLRECI(SUBCL).NE.SUBA) .AND. (CLRECI(SUBCL).GT.0))
     *      GO TO 500
C                                       Freq id
         IF ((CLRECI(FRQCL).NE.FREQID) .AND. (CLRECI(FRQCL).GT.0) .AND.
     *      (FREQID.GT.0)) GO TO 500
C                                       get correction
         CALL FXECOR (2, JERR)
         IF (JERR.NE.0) GO TO 500
C                                       Rewrite record
         CALL TABIO ('WRIT', IRCODE, ICLRNO, CLRECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 900
 500     CONTINUE
C                                       Final call to FXECOR
      CALL FXECOR (3, JERR)
C                                       Close table.
      CALL TABIO ('CLOS', IRCODE, LOOP, CLRECR, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('TABIO ERROR',I3,' CORRECTING CL TABLE')
      END
      SUBROUTINE FXECOR (IOP, IERR)
C-----------------------------------------------------------------------
C   FXECOR applies corrections to the CL record passed thru common
C   /CLRECC/.
C   Input:
C      IOP        I    Operation code, 1=init, 2=process, 3=finish
C   Input from common:
C      CLRECI(*)  I    The CL table record to be corrected.
C      OPCODE     C*4  Operation code.
C      ICODE      I    Operation code number, set on first call.
C   Output in common:
C      CLRECI(*)  I    Modified record.
C   Output:
C      IERR       I    Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IOP, IERR
C
      INCLUDE 'VLANT.INC'
      CHARACTER ARM(3)*1
      INTEGER   I, IANT, THSOU, LUN, J, K, DUMMY(MAXIF), FQBUFF(512),
     *   IIVER, MYARM(MAXANT), THSQID
      REAL      XXT, YYT, HA, EL, AZ, AZA(3), FINC(MAXIF)
      DOUBLE PRECISION    FREQS, XT, DIST, ETERM, BXC, BYC, BZC, PHASE,
     *   VAL, YT, TF, TC
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE MYARM, AZA
      DATA LUN /28/
      DATA ARM /'N','E','W'/
      DATA BXC, BYC, BZC /-1601185.4D0, -5041977.5D0, 3554875.9D0/
C-----------------------------------------------------------------------
C                                       Initialize
      IF (IOP.EQ.1) THEN
         ICODE = 1
C                                       EVLA error
C                                       parse Stations to get arms
         CALL FILL (MAXANT, 0, MYARM)
         DO 20 I = 1,NSTNS
            K = 1
            IF (STNNAM(I)(1:5).EQ.'EVLA:') K = 6
            DO 10 J = 1,3
               IF (STNNAM(I)(K:K).EQ.ARM(J)) MYARM(I) = J
 10            CONTINUE
 20         CONTINUE
C                                       last source
         AZA(1) = 355.0 * DG2RAD
         AZA(2) = 115.0 * DG2RAD
         AZA(3) = 236.0 * DG2RAD
C                                       Process record
      ELSE IF (IOP.EQ.2) THEN
         IANT = CLRECI(ANTCL)
C                                       Check if source info current
         THSOU = CLRECI(SOUCL)
         THSQID = CLRECI(FRQCL)
C                                       Get new source info.
         IF (LSTSOU.NE.THSOU) THEN
            CALL GETSOU (THSOU, DISKIN, CNOIN, CATBLK, LUN, IERR)
            IF (IERR.NE.0) GO TO 999
            LSTSOU = THSOU
            END IF
C                                       Get IF information
         IF (LSTQID.NE.THSQID) THEN
            IIVER = 1
            CALL CHNDAT ('READ', FQBUFF, DISKIN, CNOIN, IIVER, CATBLK,
     *         LUN, NUMIF, FRQOFF, DUMMY, FINC, BNDCOD, THSQID, IERR)
            IF (IERR.NE.0) GO TO 999
            LSTQID = THSQID
            END IF
C                                       Check time.
         YT = CLRECD(TIMCL)
         XT = CLRECD(TIMCL) + JD
         ETERM = 0.0D0
         IF ((XT.GE.JDB) .AND. (XT.LE.JDE)) THEN
            ETERM = -1.0D-15
            END IF
C                                       do correction
         IF ((ETERM.NE.0.0D0) .AND. (MYARM(IANT).GT.0)) THEN
            FIXCNT = FIXCNT + 1
            CALL SOUELV (IANT, YT, HA, EL, AZ)
            DIST = SQRT ((STNX(IANT)-BXC)**2 + (STNY(IANT)-BYC)**2 +
     *         (STNZ(IANT)-BZC)**2)
            TF = 1.0D0 / TAN (EL) / SIN (EL)
            TC = COS (AZ-AZA(MYARM(IANT)))
            VAL = DIST * TF * TC * ETERM
            IF (XDOINV.GT.0.0) VAL = -VAL
C                                       Polarization 1 corrections:
            DO 110 I = 1,NUMIF
               XXT = CLRECR(RE1CL+I-1)
               YYT = CLRECR(IM1CL+I-1)
               IF ((XXT.NE.FBLANK) .AND. (YYT.NE.FBLANK)) THEN
                  FREQS = CATD(KDCRV+JLOCF) + FRQOFF(I)
                  PHASE = TWOPI * FREQS * VAL
                  XT = COS (PHASE)
                  YT = SIN (PHASE)
                  CLRECR(RE1CL+I-1) = XXT*XT - YYT*YT
                  CLRECR(IM1CL+I-1) = XXT*YT + YYT*XT
                  CLRECR(DE1CL+I-1) = CLRECR(DE1CL+I-1) + VAL
                  END IF
110            CONTINUE
C                                       Polarization 2 corrections:
            IF (NUMPOL.GT.1) THEN
               DO 120 I = 1,NUMIF
                  XXT = CLRECR(RE2CL+I-1)
                  YYT = CLRECR(IM2CL+I-1)
                  IF ((XXT.NE.FBLANK) .AND. (YYT.NE.FBLANK)) THEN
                     FREQS = CATD(KDCRV+JLOCF) + FRQOFF(I)
                     PHASE = TWOPI * FREQS * VAL
                     XT = COS (PHASE)
                     YT = SIN (PHASE)
                     CLRECR(RE2CL+I-1) = XXT*XT - YYT*YT
                     CLRECR(IM2CL+I-1) = XXT*YT + YYT*XT
                     CLRECR(DE2CL+I-1) = CLRECR(DE2CL+I-1) + VAL
                     END IF
 120              CONTINUE
               END IF
            END IF
C                                       Finish - number changed.
      ELSE IF (IOP.EQ.3) THEN
         NUMHIS= NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2900) FIXCNT
         WRITE (MSGTXT,2901) FIXCNT
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 2900 FORMAT (' / ',I6,' Records modified for atmos delay')
 2901 FORMAT (I6,' Records modified for atmos delay')
      END
