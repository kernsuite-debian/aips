LOCAL INCLUDE 'TYSMO.INC'
C                                                          Include TYSMO
C                                       Local include for TYSMO
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXINTP, XYTYPE
      REAL      XSIN, XDISIN, DOBTWN, XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XTIME(8), XANT(50), XSUBA, XFLAG, XINVER, XOUTVR, APARM(10),
     *   CPARM(10), XIPARM(10), CUTOFF, XDOBLK, XBAD(10), SELBAN
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XINTP*4, YTYPE*2
      INTEGER   SEQIN, SUBA, DISKIN, CNOIN, TYVER, ITYIN, ITYOUT,
     *   NSOUWD, SOUWAN(30), NANTSL, ANTENS(50), BIF, EIF, FREQID,
     *   NUMAN(513), FGVER
      LOGICAL   DOSWNT, DOAWNT
      DOUBLE PRECISION FRQOFF(MAXIF), SELFRQ, TSTART, TEND
C                                       Buffers and file info
      INTEGER   XTYRSZ
      PARAMETER (XTYRSZ = 6 + 4*MAXIF)
      INTEGER   BUFFO(512), BUFFI(512), RECORD(XCLRSZ)
      REAL      RECR(XCLRSZ)
      DOUBLE PRECISION RECD(XCLRSZ/2)
      EQUIVALENCE (RECORD, RECR, RECD)
C                                       MXTIME = dim work arrays
      INTEGER   MXTIME
      PARAMETER (MXTIME = 110000)
      REAL      WRKTIM(MXTIME), WORK1(MXTIME), WORK2(MXTIME),
     *   WORK3(MXTIME), WORK4(MXTIME), WORK5(MXTIME), WORK6(MXTIME),
     *   WORK7(MXTIME), WORK8(MXTIME), WORK9(MXTIME),
     *   TCAL(4,MAXIF,MAXANT)
      INTEGER   WRKREC(MXTIME), WRKSRC(MXTIME), WRKTYP(MXTIME)
C                                       Inputs and general info
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, DOBTWN,
     *   XBAND, XFREQ, XFQID, XBIF, XEIF, XTIME, XANT, XSUBA, XFLAG,
     *   XYTYPE, XINVER, XOUTVR, APARM, CPARM, XXINTP, XIPARM, CUTOFF,
     *   XDOBLK, XBAD
      COMMON /CINFO/ FRQOFF, SELFRQ, TSTART, TEND, DOSWNT, DOAWNT,
     *   NSOUWD, SOUWAN, NANTSL, ANTENS, BIF, EIF, FREQID, SELBAN,
     *   SEQIN, DISKIN, CNOIN, SUBA, TYVER, ITYIN, ITYOUT, NUMAN,
     *   TCAL, FGVER
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XINTP, YTYPE
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFO, BUFFI, RECORD
C                                       Align WRK* in memory
      COMMON /XXYYZZ/ WRKTIM, WORK1, WORK2, WORK3, WORK4, WORK5,
     *   WORK6, WORK7, WORK8, WORK9, WRKREC, WRKSRC, WRKTYP
C                                                          End TYSMO
LOCAL END
      PROGRAM TYSMO
C-----------------------------------------------------------------------
C! Smooths a TY or SY table
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2010-2012, 2014-2017
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
C   Task TYSMO smooths a TY table.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYSMO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'TYSMO '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL TYSMIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Clip
      CALL TYCLIP (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Smooth
      CALL TYSMOO (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy and update HI file.
      CALL TYSMHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFO)
C
 999  STOP
      END
      SUBROUTINE TYSMIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   TYSMIN gets input parameters for TYSMO.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      JERR   I     Error code: 0 => ok
C                               1 => Invalid request
C                               5 => catalog troubles
C                               8 => can't start
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      CHARACTER STAT*4, UTYPE*2
      LOGICAL   T, F, ALLANT, DESEL, MATCH, WTABLE, WEXIST, WFITS
      INTEGER   NPARM, IERR, I, NEXT, IARG, LIMIT, J, IROUND, LUN,
     *   LUN2, IIVER, NUMIF, ITY, ISY
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DUMMY(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'TYSMO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /29/, LUN2 /31/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 239
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFO, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFO, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBA = XSUBA + 0.5
      ITYIN = MAX (IROUND (XINVER), 0)
      ITYOUT = MAX (IROUND (XOUTVR), 0)
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 20      CONTINUE
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXINTP, XINTP)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 25      CONTINUE
      CALL H2CHR (2, 1, XYTYPE, YTYPE)
C                                       Find file, read CATBLK
      CNOIN = 1
      STAT = 'SRCH'
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFO, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK, mark "WRIT"
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFO, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Register in DFIL.INC
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       flagver
      CALL FNDEXT ('FG', CATBLK, I)
      J = IROUND (XFLAG)
      IF (J.EQ.0) J = I
      FGVER = J
      IF ((XFLAG.LT.0.0) .OR. (J.LE.0)) FGVER = 0
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FREQID = IROUND (XFQID)
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FREQID, JERR)
C                                       No match
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       antenna count
      CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN, WRKTIM, NUMAN, JERR)
      IF (JERR.NE.0) GO TO 999
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
      JERR = 0
C                                       Antenna list
      ALLANT = T
      NANTSL = 0
      DESEL = F
      DO 100 I = 1,50
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
         DESEL = DESEL .OR. (XANT(I).LT.-0.5)
 100     CONTINUE
      NEXT = 1
C                                       Not all selected - make list
C                                       ANTENNAS array.
      IF (.NOT.ALLANT) THEN
         DO 150 I = 1,50
            IARG = ABS (XANT(I)) + 0.5
C                                       See if already have
            IF (IARG.NE.0) THEN
               LIMIT = NEXT - 1
               IF (LIMIT.GE.1) THEN
                  DO 130 J = 1,LIMIT
                     IF (IARG.EQ.ANTENS(J)) GO TO 150
 130                 CONTINUE
                  END IF
C                                       New antenna
               ANTENS(NEXT) = IARG
               NEXT = NEXT + 1
               END IF
 150        CONTINUE
         END IF
      DOAWNT = .NOT. DESEL
      NANTSL = NEXT - 1
C                                       Get source numbers.
C                                       Check if single-source file.
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUN, BUFFO, WTABLE, WEXIST,
     *   WFITS, JERR)
      IF ((JERR.EQ.0).AND.WEXIST.AND.WTABLE) THEN
         CALL FNDSOU (DISKIN, CNOIN, XSOUR, BUFFO, NSOUWD, DOSWNT,
     *      SOUWAN, JERR)
         IF (JERR.NE.0) GO TO 999
      ELSE
         NSOUWD = 0
         END IF
C                                       Get IF information
      IIVER = 1
      CALL CHNDAT ('READ', BUFFO, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NUMIF, FRQOFF, DUMMY, FINC, BNDCOD, FREQID, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Timerange
      TSTART = XTIME(1) + XTIME(2) / 24.0D0 + XTIME(3) / (24.0D0*60.0D0)
     *   + (XTIME(4) / (24.0D0*60.0D0*60.0D0))
      TEND = XTIME(5) + XTIME(6) / 24.0D0 + XTIME(7) / (24.0D0*60.0D0) +
     *   (XTIME(8) / (24.0D0*60.0D0*60.0D0))
      IF ((TEND.LT.TSTART) .OR. (TEND.LT.1.0D-5)) TEND = 1.0D20
C                                       which type
      CALL FNDEXT ('TY', CATBLK, ITY)
      CALL FNDEXT ('SY', CATBLK, ISY)
      IF (YTYPE.EQ.'TY') THEN
         IF (ITY.LE.0) THEN
            MSGTXT = 'REQUESTED TY TABLE TYPE NOT FOUND IN DATA'
            JERR = 10
            GO TO 990
            END IF
         IF (ITYIN.LE.0) ITYIN = ITY
      ELSE IF (YTYPE.EQ.'SY') THEN
         IF (ISY.LE.0) THEN
            MSGTXT = 'REQUESTED SY TABLE TYPE NOT FOUND IN DATA'
            JERR = 10
            GO TO 990
            END IF
         IF (ITYIN.LE.0) ITYIN = ISY
      ELSE
         IF ((ISY.GT.0) .AND. (ITY.GT.0)) THEN
            MSGTXT = 'BOTH TY AND SY FOUND: SPECIFY INEXT'
            JERR = 10
            GO TO 990
         ELSE IF (ISY.GT.0) THEN
            YTYPE = 'SY'
            IF (ITYIN.LE.0) ITYIN = ISY
         ELSE IF (ITY.GT.0) THEN
            YTYPE = 'TY'
            IF (ITYIN.LE.0) ITYIN = ITY
         ELSE
            MSGTXT = 'NEITHER TY NOR SY FOUND IN DATA'
            JERR = 10
            GO TO 990
            END IF
         END IF
C                                       Copy input TY table version
C                                       to output version. Smoothing
C                                       is carried out on the output
C                                       table in place.
      TYVER = ITYOUT
      IF ((ITYIN.NE.ITYOUT) .AND. (FGVER.LE.0)) THEN
         CALL TABCOP (YTYPE, ITYIN, ITYOUT, LUN, LUN2, DISKIN, DISKIN,
     *      CNOIN, CNOIN, CATBLK, BUFFO, BUFFI, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1200) JERR, 'TABCOP'
            GO TO 990
            END IF
C                                       copy and flag
      ELSE IF (FGVER.GT.0) THEN
         CALL TYFLAG (LUN, LUN2, JERR)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYSMIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1200 FORMAT ('TYSMIN: ERROR',I4,' RETURNED BY ',A)
      END
      SUBROUTINE TYCLIP (IERR)
C-----------------------------------------------------------------------
C   Clips selected portions of TY tables.
C   Leaves the output table sorted in antenna-time order.
C   Inputs from common:
C      TYVER        I    Cal (TY) file version number.
C      TSTART       R    First time to process (days) (no default)
C      TEND         R    Last time to process (days) (no default)
C   Output:
C      IERR         I    Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  COLHED(2)*24
      INTEGER   IRET, KEY(2,2), ICLUN, TYANT, TYTIM, ITYRNO,  NKEY, I,
     *   KOLS(2), TYNUMV(15), TYKOLS(15), NUMPOL, NUMIF, SUB, TYPKOL,
     *   NUMSUB, TIMKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL, TA1KOL, TS1KOL,
     *   TG1KOL, TA2KOL, TS2KOL, TG2KOL, NUMANT, J, KEYSUB(2,2)
      LOGICAL   T, DOTSYS, DOTANT, DOTRAT, DOTGAI
      REAL      FKEY(2,2), STTSYS(3), STTANT(3), MXTSYS, MXTANT,
     *   MNXSYS(2), MNXANT(2), MNXRAT(2), STTRAT(3), MXTRAT, MNXGAI(2),
     *   STTGAI(3), MXTGAI
      INCLUDE 'TYSMO.INC'
      INCLUDE 'INCS:PTYTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA COLHED /'TIME ', 'ANTENNA NO. '/
      DATA T /.TRUE./
      DATA ICLUN /30/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Sort TY table to antenna-time.
C                                       Need col. pointers, sort order.
      IF (YTYPE.EQ.'TY') THEN
         CALL TYINI ('READ', BUFFO, DISKIN, CNOIN, TYVER, CATBLK,
     *      ICLUN, ITYRNO, TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Get column pointers
         NKEY = 2
         CALL FNDCOL (NKEY, COLHED, 24, T, BUFFO, KOLS, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 999
         IRET = 0
         TYTIM = KOLS(1)
         TYANT = KOLS(2)
         TYPKOL = -1
         TIMKOL = TYKOLS(TYRTIM)
         SOUKOL = TYKOLS(TYISID)
         ANTKOL = TYKOLS(TYIANT)
         SUBKOL = TYKOLS(TYISUB)
         FRQKOL = TYKOLS(TYIFQI)
         TS1KOL = TYKOLS(TYRTS1)
         TA1KOL = TYKOLS(TYRTA1)
         IF (NUMPOL.GT.1) THEN
            TS2KOL = TYKOLS(TYRTS2)
            TA2KOL = TYKOLS(TYRTA2)
         ELSE
            TS2KOL = -1000
            TA2KOL = -1000
            END IF
C                                       SY table
      ELSE
         CALL SYINI ('READ', BUFFO, DISKIN, CNOIN, TYVER, CATBLK,
     *      ICLUN, ITYRNO, TYKOLS, TYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Get column pointers
         NKEY = 2
         CALL FNDCOL (NKEY, COLHED, 24, T, BUFFO, KOLS, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 999
         IRET = 0
         TYTIM = KOLS(1)
         TYANT = KOLS(2)
         TIMKOL = TYKOLS(1)
         TYPKOL = TYKOLS(3)
         SOUKOL = TYKOLS(4)
         ANTKOL = TYKOLS(5)
         SUBKOL = TYKOLS(6)
         FRQKOL = TYKOLS(7)
         TS1KOL = TYKOLS(8)
         TA1KOL = TYKOLS(9)
         TG1KOL = TYKOLS(10)
         IF (NUMPOL.GT.1) THEN
            TS2KOL = TYKOLS(11)
            TA2KOL = TYKOLS(12)
            TG2KOL = TYKOLS(13)
         ELSE
            TS2KOL = -1000
            TA2KOL = -1000
            TG2KOL = -1000
            END IF
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFO, BUFFO, IERR)
      IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 999
      KEY(1,1) = TYANT
      KEY(1,2) = TYTIM
      KEY(2,1) = 0
      KEY(2,2) = 0
C                                       Sort to antenna time order.
      IF (((BUFFO(43).NE.TYANT) .OR. (BUFFO(44).NE.TYTIM))) THEN
         CALL TABSRT (DISKIN, CNOIN, YTYPE, TYVER, TYVER, KEY, KEYSUB,
     *      FKEY, BUFFO, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Reopen write
      IF (YTYPE.EQ.'TY') THEN
         CALL TYINI ('WRIT', BUFFO, DISKIN, CNOIN, TYVER, CATBLK,
     *      ICLUN, ITYRNO, TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
      ELSE
         CALL SYINI ('WRIT', BUFFO, DISKIN, CNOIN, TYVER, CATBLK,
     *      ICLUN, ITYRNO, TYKOLS, TYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
C                                       Find number of subarrays.
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      NUMSUB = MAX (NUMSUB, 1)
C                                       Determine smoothing parameters.
      MNXSYS(1) = APARM(1)
      MNXSYS(2) = APARM(6)
      IF (MNXSYS(2).LE.MNXSYS(1)) MNXSYS(2) = 99999.
      MNXANT(1) = APARM(2)
      MNXANT(2) = APARM(7)
      IF (MNXANT(2).LE.MNXANT(1)) MNXANT(2) = 99999.
      IF (YTYPE.EQ.'SY') THEN
         MNXRAT(1) = APARM(3)
         MNXRAT(2) = APARM(8)
         MNXGAI(1) = APARM(4)
         MNXGAI(2) = APARM(9)
      ELSE
         MNXRAT(1) = 0.0
         MNXRAT(2) = 0.0
         MNXGAI(1) = 0.0
         MNXGAI(2) = 0.0
         END IF
      IF (MNXRAT(2).LE.MNXRAT(1)) MNXRAT(2) = 99999.
      IF (MNXGAI(2).LE.MNXGAI(1)) MNXGAI(2) = 99999.
      APARM(6) = MNXSYS(2)
      APARM(7) = MNXANT(2)
      APARM(8) = MNXRAT(2)
      APARM(9) = MNXGAI(2)
      STTSYS(1) = CPARM(1) / 1440.0
      STTANT(1) = CPARM(2) / 1440.0
      STTRAT(1) = CPARM(3) / 1440.0
      STTGAI(1) = CPARM(4) / 1440.0
      MXTSYS = ABS (CPARM(6))
      MXTANT = ABS (CPARM(7))
      MXTRAT = ABS (CPARM(8))
      MXTGAI = ABS (CPARM(8))
      DOTSYS =  (MXTSYS.GE.1.0E-10) .OR. (MNXSYS(1).GT.0.0) .OR.
     *   (MNXSYS(2).LT.99999.)
      DOTANT =  (MXTANT.GE.1.0E-10) .OR. (MNXANT(1).GT.0.0) .OR.
     *   (MNXANT(2).LT.99999.)
      DOTRAT =  (MXTRAT.GE.1.0E-10) .OR. (MNXRAT(1).GT.0.0) .OR.
     *   (MNXRAT(2).LT.99999.)
      DOTGAI =  (MXTGAI.GE.1.0E-10) .OR. (MNXGAI(1).GT.0.0) .OR.
     *   (MNXGAI(2).LT.99999.)
      DOTRAT =  (DOTRAT) .AND. (YTYPE.EQ.'SY')
      DOTGAI =  (DOTGAI) .AND. (YTYPE.EQ.'SY')
      IF (MXTSYS.LT.1.0E-10) MXTSYS = 1.0E20
      IF (MXTANT.LT.1.0E-10) MXTANT = 1.0E20
      IF (MXTRAT.LT.1.0E-10) MXTRAT = 1.0E20
      IF (MXTGAI.LT.1.0E-10) MXTGAI = 1.0E20
C                                       Inform user of smoothing:
      IF ((DOTSYS) .OR. (DOTANT) .OR. (DOTRAT) .OR. (DOTGAI)) THEN
         MSGTXT ='TYSMTH: Clipping ' // YTYPE // ' table'
         CALL MSGWRT (4)
C                                       Loop over subarrays
         DO 200 SUB = 1,NUMSUB
C                                       Want this subarray?
            IF ((SUBA.LE.0) .OR. (SUB.EQ.SUBA)) THEN
C                                       If SY, need CalDevice values
               IF (YTYPE.EQ.'SY') THEN
                  J = 0
                  CALL GETCDS (DISKIN, CNOIN, J, SUB, FREQID, CATBLK,
     *               TCAL, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
C                                       Clip TY/SY
               DO 100 I = BIF,EIF
                  CALL CLPTY (MNXSYS, MNXANT, MNXRAT, MNXGAI, STTSYS,
     *               STTANT, STTRAT, STTGAI, MXTSYS, MXTANT, MXTRAT,
     *               MXTGAI, DOTSYS, DOTANT, DOTRAT, DOTGAI, SUB,
     *               NUMAN(SUB+1), TYPKOL, TIMKOL, SUBKOL, ANTKOL,
     *               SOUKOL, FRQKOL, TS1KOL+I-1, TA1KOL+I-1, TG1KOL+I-1,
     *               TS2KOL+I-1, TA2KOL+I-1, TG2KOL+I-1, I, IRET)
                  IF (IRET.NE.0) GO TO 999
 100              CONTINUE
               END IF
 200        CONTINUE
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFO, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CLPTY (MNXSYS, MNXANT, MNXRAT, MNXGAI, STTSYS, STTANT,
     *   STTRAT, STTGAI, MXTSYS, MXTANT, MXTRAT, MXTGAI, DOTSYS, DOTANT,
     *   DOTRAT, DOTGAI, SUB, NUMANT, TYPKOL, TIMKOL, SUBKOL, ANTKOL,
     *   SOUKOL, FRQKOL, TS1KOL, TA1KOL, TG1KOL, TS2KOL, TA2KOL, TG2KOL,
     *   BEGIF, IRET)
C-----------------------------------------------------------------------
C   Routine to clip Tsys and Tant by comparison with a median window
C   filter.  All poln present and the range of IF specified by IFBEG
C   and IFEND are clipped.  The values in a single polarization are
C   averaged.
C   Inputs:
C      MNXSYS   R(2)    Min/max Tsys
C      MNXANT   R(2)    Min/max Tant
C      STTSYS   R       Tsys smoothing time (days)
C      STTANT   R       Tant smoothing time (days)
C      MXTSYS   R       Max. Tsys residual
C      MXTANT   R       Max. Tant residual (rad)
C      DOTSYS   L       Smooth Tsys
C      DOTANT   L       Smooth Tant
C      SUB      I       Desired subarray
C      NUMANT   I       Number of antennas
C      TIMKOL   I       Time column pointer.
C      SUBKOL   I       Subarray column pointer
C      ANTKOL   I       Antenna column pointer
C      FRQKOL   I       FQ id column pointer
C      SOUKOL   I       Source ID column pointer
C      TS1KOL   I       Tsys pol 1  column pointer
C      TA1KOL   I       Tant 1  column pointer
C      TS2KOL   I       Tsys pol 2 pointer <1 => not present
C      TA2KOL   I       Tant 2  column pointer <1 => not present
C      BEGIF    I       IF for display purposes only
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      REAL      MNXSYS(2), MNXANT(2), MNXRAT(2), MNXGAI(2), STTSYS(*),
     *   STTANT(*), STTRAT(*), STTGAI(*), MXTSYS, MXTANT, MXTRAT, MXTGAI
      LOGICAL   DOTSYS, DOTANT, DOTRAT, DOTGAI
      INTEGER   SUB, NUMANT, TYPKOL, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   FRQKOL, TS1KOL, TA1KOL, TG1KOL, TS2KOL, TA2KOL, TG2KOL, BEGIF,
     *   IRET
C
      INTEGER   LOOPR, LOOPA, IRCODE, NUMTIM, ANT, NUMREC, FSTREC,
     *   NLEFT, SAVE, ITYRNO, ITIME, NRECS, NTANT, NTSYS, NSYVAL, NTRAT,
     *   NTGAI, ITC
      LOGICAL   SLCTD, TYWANT, BAD, WANT
      REAL      DIFF, RTIME
      DOUBLE PRECISION TIMOFF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Get number of records in table
      NUMREC = BUFFO(5)
      IF (NUMREC.LE.0) GO TO 999
      IF ((.NOT.DOTSYS) .AND. (.NOT.DOTANT) .AND. (.NOT.DOTRAT))
     *   GO TO 999
      FSTREC = 0
      NRECS = 0
      NTANT = 0
      NTSYS = 0
      NTRAT = 0
      NTGAI = 0
      NSYVAL = 0
C                                       Loop over antenna
      DO 600 LOOPA = 1,NUMANT
         ANT = LOOPA
C                                       Want this antenna?
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 600
C                                       Set pointers, counters
         NUMTIM = 0
         NLEFT = NUMREC - FSTREC
C                                       Loop in time, reading
         DO 100 LOOPR = 1,NLEFT
            ITYRNO = FSTREC + LOOPR
            CALL TABIO ('READ', IRCODE, ITYRNO, RECORD, BUFFO, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 100
               END IF
            IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
            IF (RECORD(ANTKOL).GT.ANT) GO TO 110
            IF (YTYPE.EQ.'SY') THEN
               RTIME = RECD(TIMKOL)
            ELSE
               RTIME = RECR(TIMKOL)
               END IF
C                                       See if wanted.
            WANT = TYWANT (RECORD(SOUKOL), RECORD(FRQKOL),
     *         RECORD(ANTKOL), RECORD(SUBKOL), RTIME)
C                                       Check subarray
            WANT = WANT .AND. (RECORD(SUBKOL).EQ.SUB)
C                                       Not all antennas wanted
            WANT = WANT .AND. (RECORD(ANTKOL).EQ.ANT)
            IF (WANT) THEN
               IF (NUMTIM.EQ.MXTIME) THEN
                  MSGTXT = 'NUMBER OF TIMES EXCESSIVE, OPERATION' //
     *               ' TRUNCATED'
                  CALL MSGWRT (8)
                  END IF
               IF (NUMTIM.LT.MXTIME) THEN
                  NUMTIM = NUMTIM + 1
                  IF (NUMTIM.EQ.1) TIMOFF = RTIME
                  WRKTIM(NUMTIM) = RTIME - TIMOFF
                  WRKREC(NUMTIM) = ITYRNO
                  IF (TYPKOL.GT.0) THEN
                     WRKTYP(NUMTIM) = RECORD(TYPKOL)
                  ELSE
                     WRKTYP(NUMTIM) = 0
                     END IF
C                                       basic clipping included
                  WORK2(NUMTIM) = RECR(TS1KOL)
                  IF ((YTYPE.EQ.'TY') .AND.
     *               (ABS(WORK2(NUMTIM)-999.0).LT.0.1))
     *               WORK2(NUMTIM) = FBLANK
                  IF ((WORK2(NUMTIM).NE.FBLANK) .AND.
     *               ((WORK2(NUMTIM).LE.MNXSYS(1)) .OR.
     *               (WORK2(NUMTIM).GE.MNXSYS(2)))) WORK2(NUMTIM)=FBLANK
                  WORK4(NUMTIM) = RECR(TA1KOL)
                  IF ((YTYPE.EQ.'TY') .AND.
     *               (ABS(WORK4(NUMTIM)-999.0).LT.0.1))
     *               WORK4(NUMTIM) = FBLANK
                  IF ((WORK4(NUMTIM).NE.FBLANK) .AND.
     *               ((WORK4(NUMTIM).LE.MNXANT(1)) .OR.
     *               (WORK4(NUMTIM).GE.MNXANT(2)))) WORK4(NUMTIM)=FBLANK
                  WORK6(NUMTIM) = FBLANK
                  IF (YTYPE.EQ.'SY') THEN
C                                       Psys 1
                     IF (WRKTYP(NUMTIM).EQ.1) THEN
                        ITC = 3
                     ELSE
                        ITC = 1
                        END IF
                     IF ((WORK2(NUMTIM).LE.0.0) .OR.
     *                  (WORK4(NUMTIM).EQ.FBLANK) .OR.
     *                  (WORK2(NUMTIM).EQ.FBLANK) .OR.
     *                  (TCAL(ITC,BEGIF,ANT).EQ.FBLANK)) THEN
                        WORK2(NUMTIM) = FBLANK
                        WORK4(NUMTIM) = FBLANK
                     ELSE
                        WORK6(NUMTIM) = WORK4(NUMTIM)/WORK2(NUMTIM) / 2.
     *                     * TCAL(ITC,BEGIF,ANT)
                        IF ((WORK6(NUMTIM).LE.MNXRAT(1)) .OR.
     *                     (WORK6(NUMTIM).GE.MNXRAT(2))) THEN
                           WORK2(NUMTIM) = FBLANK
                           WORK4(NUMTIM) = FBLANK
                           WORK6(NUMTIM) = FBLANK
                           END IF
                        END IF
C                                       Pgain 1
                     WORK8(NUMTIM) = RECR(TG1KOL)
                     IF ((WORK8(NUMTIM).NE.FBLANK) .AND.
     *                  ((WORK8(NUMTIM).LE.MNXGAI(1)) .OR.
     *                  (WORK8(NUMTIM).GE.MNXGAI(2))))
     *                  WORK8(NUMTIM) = FBLANK
                     END IF
                  IF (TS2KOL.GT.0) THEN
                     WORK3(NUMTIM) = RECR(TS2KOL)
                     IF ((YTYPE.EQ.'TY') .AND.
     *                  (ABS(WORK3(NUMTIM)-999.0).LT.0.1))
     *                  WORK3(NUMTIM) = FBLANK
                     IF ((WORK3(NUMTIM).NE.FBLANK) .AND.
     *                  ((WORK3(NUMTIM).LE.MNXSYS(1)) .OR.
     *                  (WORK3(NUMTIM).GE.MNXSYS(2)))) WORK3(NUMTIM) =
     *                  FBLANK
                     WORK5(NUMTIM) = RECR(TA2KOL)
                     IF ((YTYPE.EQ.'TY') .AND.
     *                  (ABS(WORK5(NUMTIM)-999.0).LT.0.1))
     *                  WORK5(NUMTIM) = FBLANK
                     IF ((WORK5(NUMTIM).NE.FBLANK) .AND.
     *                  ((WORK5(NUMTIM).LE.MNXANT(1)) .OR.
     *                  (WORK5(NUMTIM).GE.MNXANT(2)))) WORK5(NUMTIM) =
     *                  FBLANK
                     WORK7(NUMTIM) = FBLANK
                     IF (YTYPE.EQ.'SY') THEN
                        IF (WRKTYP(NUMTIM).EQ.1) THEN
                           ITC = 4
                        ELSE
                           ITC = 2
                           END IF
C                                       Psys 2
                        IF ((WORK3(NUMTIM).LE.0.0) .OR.
     *                     (WORK5(NUMTIM).EQ.FBLANK) .OR.
     *                     (WORK3(NUMTIM).EQ.FBLANK) .OR.
     *                     (TCAL(ITC,BEGIF,ANT).EQ.FBLANK)) THEN
                           WORK3(NUMTIM) = FBLANK
                           WORK5(NUMTIM) = FBLANK
                        ELSE
                           WORK7(NUMTIM) = WORK5(NUMTIM) / WORK3(NUMTIM)
     *                        *  TCAL(ITC,BEGIF,ANT) / 2.0
                           IF ((WORK7(NUMTIM).LE.MNXRAT(1)) .OR.
     *                        (WORK7(NUMTIM).GE.MNXRAT(2))) THEN
                              WORK3(NUMTIM) = FBLANK
                              WORK5(NUMTIM) = FBLANK
                              WORK7(NUMTIM) = FBLANK
                              END IF
                           END IF
C                                       Pgain 1
                        WORK9(NUMTIM) = RECR(TG2KOL)
                        IF ((WORK9(NUMTIM).NE.FBLANK) .AND.
     *                     ((WORK9(NUMTIM).LE.MNXGAI(1)) .OR.
     *                     (WORK9(NUMTIM).GE.MNXGAI(2))))
     *                     WORK9(NUMTIM) = FBLANK
                        END IF
                  ELSE
                     WORK3(NUMTIM) = FBLANK
                     WORK5(NUMTIM) = FBLANK
                     WORK7(NUMTIM) = FBLANK
                     WORK9(NUMTIM) = FBLANK
                     END IF
                  IF (DOBTWN.LE.0.0) THEN
                     WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
                  ELSE
                     WRKSRC(NUMTIM) = -1
                     END IF
                  END IF
               END IF
 100        CONTINUE
 110     SAVE = ITYRNO - 1
         IF (NUMTIM.LE.0) GO TO 590
C                                       Tsys smooth as requested
         IF (DOTSYS) THEN
            CALL TYSMSM ('MWF ', STTSYS, WRKTIM, WORK2, FBLANK, NUMTIM,
     *         WRKSRC, WORK1)
C                                       Second Poln?
            IF (TS2KOL.GT.0) THEN
               CALL TYSMSM ('MWF ', STTSYS, WRKTIM, WORK3, FBLANK,
     *            NUMTIM, WRKSRC, WORK2)
               END IF
         ELSE
            CALL RCOPY (NUMTIM, WORK2, WORK1)
            CALL RCOPY (NUMTIM, WORK3, WORK2)
            END IF
C                                       Tant
         IF (DOTANT) THEN
            CALL TYSMSM ('MWF ', STTANT, WRKTIM, WORK4, FBLANK, NUMTIM,
     *         WRKSRC, WORK3)
C                                       Second Poln?
            IF (TS2KOL.GT.0) THEN
               CALL TYSMSM ('MWF ', STTANT, WRKTIM, WORK5, FBLANK,
     *            NUMTIM, WRKSRC, WORK4)
               END IF
         ELSE
            CALL RCOPY (NUMTIM, WORK4, WORK3)
            CALL RCOPY (NUMTIM, WORK5, WORK4)
            END IF
C                                       Ratio Psum/Pdif
         IF (DOTRAT) THEN
            CALL TYSMSM ('MWF ', STTRAT, WRKTIM, WORK6, FBLANK, NUMTIM,
     *         WRKSRC, WORK5)
C                                       Second Poln?
            IF (TS2KOL.GT.0) THEN
               CALL TYSMSM ('MWF ', STTRAT, WRKTIM, WORK7, FBLANK,
     *            NUMTIM, WRKSRC, WORK6)
               END IF
         ELSE
            CALL RCOPY (NUMTIM, WORK6, WORK5)
            CALL RCOPY (NUMTIM, WORK7, WORK6)
            END IF
C                                       Tsys smooth as requested
         IF (DOTGAI) THEN
            CALL TYSMSM ('MWF ', STTGAI, WRKTIM, WORK8, FBLANK, NUMTIM,
     *         WRKSRC, WORK7)
C                                       Second Poln?
            IF (TS2KOL.GT.0) THEN
               CALL TYSMSM ('MWF ', STTGAI, WRKTIM, WORK9, FBLANK,
     *            NUMTIM, WRKSRC, WORK8)
               END IF
         ELSE
            CALL RCOPY (NUMTIM, WORK8, WORK7)
            CALL RCOPY (NUMTIM, WORK9, WORK8)
            END IF
C                                       Clip
         DO 200 ITIME = 1,NUMTIM
            ITYRNO = WRKREC(ITIME)
            CALL TABIO ('READ', IRCODE, ITYRNO, RECORD, BUFFO, IRET)
            IF (IRET.GT.0) GO TO 900
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 200
               END IF
            BAD = .FALSE.
C                                       test SY again
            IF (YTYPE.EQ.'SY') THEN
C                                       Psum > Pdif: 1
               IF ((RECR(TA1KOL).NE.FBLANK) .AND.
     *            (RECR(TS1KOL).NE.FBLANK) .AND.
     *            (RECR(TA1KOL).LE.RECR(TS1KOL))) THEN
                  RECR(TS1KOL) = FBLANK
                  RECR(TA1KOL) = FBLANK
                  BAD = .TRUE.
                  NSYVAL = NSYVAL + 1
                  END IF
C                                       blank on Psum/Pdif: 1
               IF ((DOTRAT) .AND. (RECR(TS1KOL).NE.FBLANK) .AND.
     *            (RECR(TS1KOL).NE.0.0) .AND.
     *            (RECR(TA1KOL).NE.FBLANK)) THEN
                  IF ((TYPKOL.GT.0) .AND. (RECORD(TYPKOL).EQ.1)) THEN
                     ITC = 3
                  ELSE
                     ITC = 1
                     END IF
                  IF ((WORK5(ITIME).NE.FBLANK) .AND.
     *               (TCAL(ITC,BEGIF,ANT).NE.FBLANK)) THEN
                     DIFF = ABS (RECR(TA1KOL)/RECR(TS1KOL) / 2. *
     *                  TCAL(ITC,BEGIF,ANT) - WORK5(ITIME))
                     IF (DIFF.GT.MXTRAT) THEN
                        RECR(TA1KOL) = FBLANK
                        RECR(TS1KOL) = FBLANK
                        BAD = .TRUE.
                        NTRAT = NTRAT + 1
                        END IF
                  ELSE
                     RECR(TA1KOL) = FBLANK
                     RECR(TS1KOL) = FBLANK
                     BAD = .TRUE.
                     NTRAT = NTRAT + 1
                     END IF
                  END IF
C                                       IF 2 Psum > Pdif
               IF (TS2KOL.GT.0) THEN
                  IF ((RECR(TA2KOL).NE.FBLANK) .AND.
     *               (RECR(TS2KOL).NE.FBLANK) .AND.
     *               (RECR(TA2KOL).LE.RECR(TS2KOL))) THEN
                     RECR(TS2KOL) = FBLANK
                     RECR(TA2KOL) = FBLANK
                     BAD = .TRUE.
                     NSYVAL = NSYVAL + 1
                     END IF
C                                       blank on Psum/Pdif: 2
                  IF ((DOTRAT) .AND. (RECR(TS2KOL).NE.FBLANK) .AND.
     *               (RECR(TS2KOL).NE.0.0) .AND.
     *               (RECR(TA2KOL).NE.FBLANK)) THEN
                     IF ((TYPKOL.GT.0) .AND. (RECORD(TYPKOL).EQ.1)) THEN
                        ITC = 4
                     ELSE
                        ITC = 2
                        END IF
                     IF ((WORK6(ITIME).NE.FBLANK) .AND.
     *                  (TCAL(ITC,BEGIF,ANT).NE.FBLANK)) THEN
                        DIFF = ABS (RECR(TA2KOL) / RECR(TS2KOL) / 2. *
     *                     TCAL(ITC,BEGIF,ANT) - WORK6(ITIME))
                        IF (DIFF.GT.MXTRAT) THEN
                           RECR(TA2KOL) = FBLANK
                           RECR(TS2KOL) = FBLANK
                           BAD = .TRUE.
                           NTRAT = NTRAT + 1
                           END IF
                     ELSE
                        RECR(TA2KOL) = FBLANK
                        RECR(TS2KOL) = FBLANK
                        BAD = .TRUE.
                        NTRAT = NTRAT + 1
                        END IF
                     END IF
                  END IF
C                                       Pgain
               IF (RECR(TG1KOL).NE.FBLANK) THEN
                  IF (WORK7(ITIME).NE.FBLANK) THEN
                     DIFF = ABS (RECR(TG1KOL) - WORK7(ITIME))
                     IF ((DIFF.GT.MXTGAI) .AND. (DOTGAI)) THEN
                        RECR(TG1KOL) = FBLANK
                        BAD = .TRUE.
                        NTGAI = NTGAI + 1
                        END IF
                  ELSE
                     RECR(TG1KOL) = FBLANK
                     BAD = .TRUE.
                     NTGAI = NTGAI + 1
                     END IF
                  END IF
               IF (TG2KOL.GT.0) THEN
                  IF (RECR(TG2KOL).NE.FBLANK) THEN
                     IF (WORK8(ITIME).NE.FBLANK) THEN
                        DIFF = ABS (RECR(TG2KOL) - WORK8(ITIME))
                        IF ((DIFF.GT.MXTGAI) .AND. (DOTGAI)) THEN
                           RECR(TG2KOL) = FBLANK
                           BAD = .TRUE.
                           NTGAI = NTGAI + 1
                           END IF
                     ELSE
                        RECR(TG2KOL) = FBLANK
                        BAD = .TRUE.
                        NTGAI = NTGAI + 1
                        END IF
                     END IF
                  END IF
               END IF
C                                       Pdif or Tsys
            IF ((YTYPE.EQ.'TY') .AND. (ABS(RECR(TS1KOL)-999.0).LT.0.1))
     *         RECR(TS1KOL) = FBLANK
            IF (RECR(TS1KOL).NE.FBLANK) THEN
               IF (WORK1(ITIME).NE.FBLANK) THEN
                  DIFF = ABS (RECR(TS1KOL) - WORK1(ITIME))
                  IF ((DIFF.GT.MXTSYS) .AND. (DOTSYS)) THEN
                     RECR(TS1KOL) = FBLANK
                     BAD = .TRUE.
                     NTSYS = NTSYS + 1
                     END IF
               ELSE
                  RECR(TS1KOL) = FBLANK
                  BAD = .TRUE.
                  NTSYS = NTSYS + 1
                  END IF
               END IF
C                                       Psum or Tant
            IF ((YTYPE.EQ.'TY') .AND. (ABS(RECR(TA1KOL)-999.0).LT.0.1))
     *         RECR(TA1KOL) = FBLANK
            IF (RECR(TA1KOL).NE.FBLANK) THEN
               IF (WORK3(ITIME).NE.FBLANK) THEN
                  DIFF = ABS (RECR(TA1KOL) - WORK3(ITIME))
                  IF ((DIFF.GT.MXTANT) .AND. (DOTANT)) THEN
                     RECR(TA1KOL) = FBLANK
                     BAD = .TRUE.
                     NTANT = NTANT + 1
                     END IF
               ELSE
                  RECR(TA1KOL) = FBLANK
                  BAD = .TRUE.
                  NTANT = NTANT + 1
                  END IF
               END IF
C                                       Second polarization?
            IF (TS2KOL.GT.0) THEN
C                                       Pdif or Tsys
               IF ((YTYPE.EQ.'TY') .AND.
     *            (ABS(RECR(TS2KOL)-999.0).LT.0.1))
     *            RECR(TS2KOL) = FBLANK
               IF (RECR(TS2KOL).NE.FBLANK) THEN
                  IF (WORK2(ITIME).NE.FBLANK) THEN
                     DIFF = ABS (RECR(TS2KOL) - WORK2(ITIME))
                     IF ((DIFF.GT.MXTSYS) .AND. (DOTSYS)) THEN
                        RECR(TS2KOL) = FBLANK
                        BAD = .TRUE.
                        NTSYS = NTSYS + 1
                        END IF
                  ELSE
                     RECR(TS2KOL) = FBLANK
                     BAD = .TRUE.
                     NTSYS = NTSYS + 1
                     END IF
                  END IF
C                                       Psum or Tant
               IF ((YTYPE.EQ.'TY') .AND.
     *            (ABS(RECR(TA2KOL)-999.0).LT.0.1))
     *            RECR(TA2KOL) = FBLANK
               IF (RECR(TA2KOL).NE.FBLANK) THEN
                  IF (WORK4(ITIME).NE.FBLANK) THEN
                     DIFF = ABS (RECR(TA2KOL) - WORK4(ITIME))
                     IF ((DIFF.GT.MXTANT) .AND. (DOTANT)) THEN
                        RECR(TA2KOL) = FBLANK
                        BAD = .TRUE.
                        NTANT = NTANT + 1
                        END IF
                  ELSE
                     RECR(TA2KOL) = FBLANK
                     BAD = .TRUE.
                     NTANT = NTANT + 1
                     END IF
                  END IF
               END IF
C                                       Rewrite record
            IF (BAD) THEN
               CALL TABIO ('WRIT', IRCODE, ITYRNO, RECORD, BUFFO, IRET)
               IF (IRET.NE.0) GO TO 900
               NRECS = NRECS + 1
               END IF
 200        CONTINUE
 590     FSTREC = SAVE
C                                       End of antenna loop
 600     CONTINUE
      IF (NRECS.GT.0) THEN
         IF (YTYPE.EQ.'TY') THEN
            WRITE (MSGTXT,1600) BEGIF, NTSYS, 'Tsys'
            CALL MSGWRT (4)
            WRITE (MSGTXT,1600) BEGIF, NTANT, 'Tant'
            CALL MSGWRT (4)
            WRITE (MSGTXT,1602) BEGIF, NRECS
            CALL MSGWRT (4)
         ELSE
            WRITE (MSGTXT,1601) BEGIF, NSYVAL
            CALL MSGWRT (4)
            WRITE (MSGTXT,1600) BEGIF, NTSYS, 'Pdif'
            CALL MSGWRT (4)
            WRITE (MSGTXT,1600) BEGIF, NTANT, 'Psum'
            CALL MSGWRT (4)
            WRITE (MSGTXT,1600) BEGIF, NTRAT, 'Tcal*Psum/Pdif/2'
            CALL MSGWRT (4)
            WRITE (MSGTXT,1600) BEGIF, NTGAI, 'Pgain'
            CALL MSGWRT (4)
            WRITE (MSGTXT,1602) BEGIF, NRECS
            CALL MSGWRT (4)
            END IF
         END IF
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1600 FORMAT ('IF',I3,' clipped',I10,1X,A)
 1601 FORMAT ('IF',I3,' blanked',I10,' improper SY values')
 1602 FORMAT ('IF',I3,' changed',I10,' table records due to clipping')
 1900 FORMAT ('CLIPTY: TABIO ERROR',I3,' CLIPPING DELAYS')
      END
      SUBROUTINE TYSMOO (IERR)
C-----------------------------------------------------------------------
C   Smooths selected portions of TY tables.
C   Leaves the output table sorted in antenna-time order.
C    Inputs from common:
C      TYVER        I    Cal (TY) file version number.
C      TSTART       R    First time to process (days) (no default)
C      TEND         R    Last time to process (days) (no default)
C    Output:
C      IERR         I    Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER COLHED(2)*24
      INTEGER   IRET, KEY(2,2), ICLUN, TYANT, TYTIM, ITYRNO, NKEY, I,
     *   KOLS(2), TYNUMV(15), TYKOLS(15), NUMPOL, NUMIF, SUB, NUMSUB,
     *   TIMKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL, TS1KOL, TA1KOL, TG1KOL,
     *   TS2KOL, TA2KOL, TG2KOL, NUMANT, KEYSUB(2,2), TYPKOL
      LOGICAL   T, DOTANT, DOTSYS, DOTGAI
      REAL      FKEY(2,2), STTSYS(3), STTANT(3), STTGAI(3)
      INCLUDE 'TYSMO.INC'
      INCLUDE 'INCS:PTYTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA COLHED /'TIME ', 'ANTENNA NO. '/
      DATA T /.TRUE./
      DATA ICLUN /30/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Sort TY table to antenna-time.
C                                       Need col. pointers, sort order.
      IF (YTYPE.EQ.'TY') THEN
         CALL TYINI ('READ', BUFFO, DISKIN, CNOIN, TYVER, CATBLK,
     *   ICLUN, ITYRNO, TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
C                                       Get column pointers
         NKEY = 2
         CALL FNDCOL (NKEY, COLHED, 24, T, BUFFO, KOLS, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 999
         IRET = 0
         TYTIM = KOLS(1)
         TYANT = KOLS(2)
         TYPKOL = -1
         TIMKOL = TYKOLS(TYRTIM)
         SOUKOL = TYKOLS(TYISID)
         ANTKOL = TYKOLS(TYIANT)
         SUBKOL = TYKOLS(TYISUB)
         FRQKOL = TYKOLS(TYIFQI)
         TS1KOL = TYKOLS(TYRTS1)
         TA1KOL = TYKOLS(TYRTA1)
         TG1KOL = -1
         TG2KOL = -1
         IF (NUMPOL.GT.1) THEN
            TS2KOL = TYKOLS(TYRTS2)
            TA2KOL = TYKOLS(TYRTA2)
         ELSE
            TS2KOL = -1000
            TA2KOL = -1000
            END IF
      ELSE
         CALL SYINI ('READ', BUFFO, DISKIN, CNOIN, TYVER, CATBLK,
     *      ICLUN, ITYRNO, TYKOLS, TYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Get column pointers
         NKEY = 2
         CALL FNDCOL (NKEY, COLHED, 24, T, BUFFO, KOLS, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 999
         IRET = 0
         TYTIM = KOLS(1)
         TYANT = KOLS(2)
         TIMKOL = TYKOLS(1)
         TYPKOL = TYKOLS(3)
         SOUKOL = TYKOLS(4)
         ANTKOL = TYKOLS(5)
         SUBKOL = TYKOLS(6)
         FRQKOL = TYKOLS(7)
         TS1KOL = TYKOLS(8)
         TA1KOL = TYKOLS(9)
         TG1KOL = TYKOLS(10)
         IF (NUMPOL.GT.1) THEN
            TS2KOL = TYKOLS(11)
            TA2KOL = TYKOLS(12)
            TG2KOL = TYKOLS(13)
         ELSE
            TS2KOL = -1000
            TA2KOL = -1000
            TG2KOL = -1000
            END IF
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFO, BUFFO, IERR)
      IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 999
C                                       Sort to antenna time order.
      KEY(1,1) = TYANT
      KEY(1,2) = TYTIM
      IF (((BUFFO(43).NE.TYANT) .OR. (BUFFO(44).NE.TYTIM))) THEN
         CALL TABSRT (DISKIN, CNOIN, 'TY', TYVER, TYVER, KEY, KEYSUB,
     *      FKEY, BUFFO, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Reopen write
      IF (YTYPE.EQ.'TY') THEN
         CALL TYINI ('WRIT', BUFFO, DISKIN, CNOIN, TYVER, CATBLK,
     *      ICLUN, ITYRNO, TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
      ELSE
         CALL SYINI ('WRIT', BUFFO, DISKIN, CNOIN, TYVER, CATBLK,
     *      ICLUN, ITYRNO, TYKOLS, TYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
C                                       Find number of subarrays.
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      NUMSUB = MAX (NUMSUB, 1)
C                                       Determine smoothing parameters.
      STTSYS(1) = XIPARM(1) / 1440.0
      STTANT(1) = XIPARM(2) / 1440.0
      STTGAI(1) = XIPARM(4) / 1440.0
      STTSYS(2) = XIPARM(6) / 1440.0
      STTANT(2) = XIPARM(7) / 1440.0
      STTGAI(2) = XIPARM(9) / 1440.0
      STTSYS(3) = CUTOFF
      STTANT(3) = CUTOFF
      STTGAI(3) = CUTOFF
      DOTSYS =  STTSYS(1) .GE. 1.0E-10
      DOTANT =  STTANT(1) .GE. 1.0E-10
      DOTGAI =  (STTGAI(1) .GE. 1.0E-10) .AND. (YTYPE.EQ.'SY')
C                                       Inform user of smoothing:
      MSGTXT ='TYSMTH: Smoothing ' // YTYPE // ' table'
      CALL MSGWRT (4)
C                                       Loop over subarrays
      DO 200 SUB = 1,NUMSUB
C                                       Want this subarray?
         IF ((SUBA.LE.0) .OR. (SUB.EQ.SUBA)) THEN
C                                       Smooth amp/phase
C                                       Loop over IF
            DO 100 I = BIF,EIF
               CALL SMOTY (STTSYS, STTANT, STTGAI, DOTSYS, DOTANT,
     *            DOTGAI, SUB, NUMAN(SUB+1), TYPKOL, TIMKOL, SUBKOL,
     *            ANTKOL,SOUKOL, FRQKOL, TS1KOL+I-1, TA1KOL+I-1,
     *            TG1KOL+I-1,TS2KOL+I-1, TA2KOL+I-1, TG2KOL+I-1, IRET)
               IF (IRET.NE.0) GO TO 999
 100           CONTINUE
            END IF
 200     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFO, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE SMOTY (STTSYS, STTANT, STTGAI, DOTSYS, DOTANT, DOTGAI,
     *   SUB, NUMANT, TYPKOL, TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL,
     *   TS1KOL, TA1KOL, TG1KOL, TS2KOL, TA2KOL, TG2KOL, IRET)
C-----------------------------------------------------------------------
C   Routine to smooth TYs in an open table in antenna-time order.
C   Inputs:
C      STTSYS   R    Tsys smoothing time (days)
C      STTANT   R    Tant smoothing time (days)
C      DOTSYS   L    Smooth Tsys?
C      DOTANT   L    Smooth Tant?
C      SUB      I    Desired subarray
C      NUMANT   I    Number of antennas
C      TIMKOL   I    Time column pointer.
C      SUBKOL   I    Subarray column pointer
C      ANTKOL   I    Antenna column pointer
C      FRQKOL   I    FQ id column pointer
C      SOUKOL   I    Source ID column pointer
C      TS1KOL   I    Tsys 1 column pointer
C      TA1KOL   I    Tant 1 column pointer
C      TS2KOL   I    Tsys 2 column pointer <1 => not present
C      TA2KOL   I    Tant 2 pointer <1 => not present
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      REAL      STTSYS(*), STTANT(*), STTGAI(*)
      LOGICAL   DOTSYS, DOTANT, DOTGAI
      INTEGER   SUB, NUMANT, TYPKOL, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   FRQKOL, TS1KOL, TA1KOL, TG1KOL, TS2KOL, TA2KOL, TG2KOL, IRET
C
      INTEGER   LOOPR, LOOPA, IRCODE, NUMTIM, ANT, NUMREC, FSTREC,
     *   NLEFT, SAVE, ITYRNO, ITIME
      LOGICAL   SLCTD, TYWANT, WANT
      DOUBLE PRECISION TIMOFF
      REAL      RTIME
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Get number of records in table
      NUMREC = BUFFO(5)
      IF (NUMREC.LE.0) GO TO 999
      IF ((.NOT.DOTSYS) .AND. (.NOT.DOTANT) .AND. (.NOT.DOTGAI))
     *   GO TO 999
      FSTREC = 0
C                                       Loop over antenna
      DO 600 LOOPA = 1,NUMANT
         ANT = LOOPA
C                                       Want this antenna?
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 600
C                                       Set pointers, counters
         NUMTIM = 0
         NLEFT = NUMREC - FSTREC
C                                       Loop in time, reading
         DO 100 LOOPR = 1,NLEFT
            ITYRNO = FSTREC + LOOPR
            CALL TABIO ('READ', IRCODE, ITYRNO, RECORD, BUFFO, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 100
               END IF
            IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
            IF (RECORD(ANTKOL).GT.ANT) GO TO 110
            IF (YTYPE.EQ.'SY') THEN
               RTIME = RECD(TIMKOL)
            ELSE
               RTIME = RECR(TIMKOL)
               END IF
C                                       See if wanted.
            WANT = TYWANT (RECORD(SOUKOL), RECORD(FRQKOL),
     *         RECORD(ANTKOL), RECORD(SUBKOL), RTIME)
C                                       Check subarray
            WANT = WANT .AND. (RECORD(SUBKOL).EQ.SUB)
C                                       Not all antennas wanted
            WANT = WANT .AND. (RECORD(ANTKOL).EQ.ANT)
            IF ((WANT) .AND. (NUMTIM.EQ.MXTIME)) THEN
               MSGTXT = 'NUMBER OF TIMES EXCESSIVE, OPERATION' //
     *            ' TRUNCATED'
               CALL MSGWRT (8)
               END IF
            IF ((WANT) .AND. (NUMTIM.LT.MXTIME)) THEN
               NUMTIM = NUMTIM + 1
               IF (NUMTIM.EQ.1) TIMOFF = RTIME
               WRKTIM(NUMTIM) = RTIME - TIMOFF
               WRKREC(NUMTIM) = ITYRNO
               IF (YTYPE.EQ.'TY') THEN
                  IF (ABS(RECR(TS1KOL)-999.0).LT.0.1)
     *               RECR(TS1KOL) = FBLANK
                  IF (ABS(RECR(TA1KOL)-999.0).LT.0.1)
     *               RECR(TA1KOL) = FBLANK
                  IF (TS2KOL.GT.0) THEN
                     IF (ABS(RECR(TS2KOL)-999.0).LT.0.1)
     *                  RECR(TS2KOL) = FBLANK
                     IF (ABS(RECR(TA2KOL)-999.0).LT.0.1)
     *                  RECR(TA2KOL) = FBLANK
                     END IF
                  END IF
               WORK2(NUMTIM) = RECR(TS1KOL)
               WORK3(NUMTIM) = RECR(TA1KOL)
               IF (TG1KOL.GT.0) WORK6(NUMTIM) = RECR(TG1KOL)
               IF (TS2KOL.GT.0) THEN
                  WORK4(NUMTIM) = RECR(TS2KOL)
                  WORK5(NUMTIM) = RECR(TA2KOL)
                  IF (TG2KOL.GT.0) WORK7(NUMTIM) = RECR(TG2KOL)
                  END IF
               IF (DOBTWN.LE.0.0) THEN
                  WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
               ELSE
                  WRKSRC(NUMTIM) = -1
                  END IF
               IF (TYPKOL.GT.0) THEN
                  WRKTYP(NUMTIM) = RECORD(TYPKOL)
               ELSE
                  WRKTYP(NUMTIM) = 0
                  END IF
               END IF
 100        CONTINUE
 110     SAVE = ITYRNO - 1
         IF (NUMTIM.LE.0) GO TO 590
C                                       Smooth as requested
         IF (DOTSYS) CALL TYSMSM (XINTP, STTSYS, WRKTIM, WORK2, FBLANK,
     *      NUMTIM, WRKSRC, WORK1)
         IF (DOTANT) CALL TYSMSM (XINTP, STTANT, WRKTIM, WORK3, FBLANK,
     *      NUMTIM, WRKSRC, WORK2)
C                                       Second Poln?
         IF (TS2KOL.GT.0) THEN
            IF (DOTSYS) CALL TYSMSM (XINTP, STTSYS, WRKTIM, WORK4,
     *         FBLANK, NUMTIM, WRKSRC, WORK3)
C                                       Second Poln?
            IF (DOTANT) CALL TYSMSM (XINTP, STTANT, WRKTIM, WORK5,
     *         FBLANK, NUMTIM, WRKSRC, WORK4)
            END IF
C                                       Pgain
         IF (DOTGAI) THEN
            CALL TYSMSM (XINTP, STTGAI, WRKTIM, WORK6,
     *         FBLANK, NUMTIM, WRKSRC, WORK5)
            IF (TG2KOL.GT.0) CALL TYSMSM (XINTP, STTGAI, WRKTIM, WORK7,
     *         FBLANK, NUMTIM, WRKSRC, WORK6)
            END IF

C                                       Replace with smoothed values
         DO 200 ITIME = 1,NUMTIM
            ITYRNO = WRKREC(ITIME)
            CALL TABIO ('READ', IRCODE, ITYRNO, RECORD, BUFFO, IRET)
            IF (IRET.NE.0) GO TO 900
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 200
               END IF
            IF (YTYPE.EQ.'TY') THEN
               IF (ABS(RECR(TS1KOL)-999.0).LT.0.1) RECR(TS1KOL) = FBLANK
               IF (ABS(RECR(TA1KOL)-999.0).LT.0.1) RECR(TA1KOL) = FBLANK
               IF (TS2KOL.GT.0) THEN
                  IF (ABS(RECR(TS2KOL)-999.0).LT.0.1)
     *               RECR(TS2KOL) = FBLANK
                  IF (ABS(RECR(TA2KOL)-999.0).LT.0.1)
     *               RECR(TA2KOL) = FBLANK
                  END IF
               END IF
C                                       Update if desired
            IF (DOTSYS) THEN
               IF (RECR(TS1KOL).EQ.FBLANK) THEN
                  IF (XDOBLK.GE.0.0) RECR(TS1KOL) = WORK1(ITIME)
               ELSE
                  IF (XDOBLK.LE.0.0) RECR(TS1KOL) = WORK1(ITIME)
                  END IF
C                                       Second polarization?
               IF (TS2KOL.GT.0) THEN
                  IF (RECR(TS2KOL).EQ.FBLANK) THEN
                     IF (XDOBLK.GE.0.0) RECR(TS2KOL) = WORK3(ITIME)
                  ELSE
                     IF (XDOBLK.LE.0.0) RECR(TS2KOL) = WORK3(ITIME)
                     END IF
                  END IF
               END IF
            IF (DOTANT) THEN
               IF (RECR(TA1KOL).EQ.FBLANK) THEN
                  IF (XDOBLK.GE.0.0) RECR(TA1KOL) = WORK2(ITIME)
               ELSE
                  IF (XDOBLK.LE.0.0) RECR(TA1KOL) = WORK2(ITIME)
                  END IF
C                                       Second polarization?
               IF (TA2KOL.GT.0) THEN
                  IF (RECR(TA2KOL).EQ.FBLANK) THEN
                     IF (XDOBLK.GE.0.0) RECR(TA2KOL) = WORK4(ITIME)
                  ELSE
                     IF (XDOBLK.LE.0.0) RECR(TA2KOL) = WORK4(ITIME)
                     END IF
                  END IF
               END IF
C                                       Pgain
            IF (DOTGAI) THEN
               IF (RECR(TG1KOL).EQ.FBLANK) THEN
                  IF (XDOBLK.GE.0.0) RECR(TG1KOL) = WORK5(ITIME)
               ELSE
                  IF (XDOBLK.LE.0.0) RECR(TG1KOL) = WORK5(ITIME)
                  END IF
C                                       Second polarization?
               IF (TG2KOL.GT.0) THEN
                  IF (RECR(TG2KOL).EQ.FBLANK) THEN
                     IF (XDOBLK.GE.0.0) RECR(TG2KOL) = WORK6(ITIME)
                  ELSE
                     IF (XDOBLK.LE.0.0) RECR(TG2KOL) = WORK6(ITIME)
                     END IF
                  END IF
               END IF
C                                       Rewrite record
            CALL TABIO ('WRIT', IRCODE, ITYRNO, RECORD, BUFFO, IRET)
            IF (IRET.NE.0) GO TO 900
 200        CONTINUE
 590     FSTREC = SAVE
C                                       End of antenna loop
 600     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('SMOTY: TABIO ERROR',I3,' SMOOTHING TYS')
      END
      SUBROUTINE TYSMSM (SMMETH, SMOTIM, TIME, IN, BLANK, NUMTIM, S,
     *   OUT)
C-----------------------------------------------------------------------
C   Routine to call appropriate smoothing routine.  Magic value blanking
C   is supported.
C   Inputs:
C      SMMETH   C*4    Method 'BOX','MWF', unknown = 'BOX'
C      SMOTIM   R(*)   Smoothing time (days)
C      TIME     R(*)   Times (days)
C      IN       R(*)   Input values.
C      BLANK    R      Magic blank value.
C      NUMTIM   I      Number of time/values
C      S        I(*)   Source number list
C   Output:
C      OUT      R(*)   Output array
C-----------------------------------------------------------------------
      CHARACTER SMMETH*4
      REAL      SMOTIM(*), TIME(*), IN(*), BLANK, OUT(*)
      INTEGER   NUMTIM, S(*)
C-----------------------------------------------------------------------
C                                       Any work to do?
      IF (NUMTIM.LE.0) GO TO 999
C                                       Median window filter
      IF (SMMETH.EQ.'MWF') THEN
         CALL MWFBSM (SMOTIM, TIME, IN, S, BLANK, NUMTIM, OUT)
C                                       function types
      ELSE IF (SMMETH.EQ.'GAUS') THEN
         CALL FUNBSM (SMMETH, SMOTIM(3), SMOTIM, TIME, IN, S, BLANK,
     *      NUMTIM, OUT)
      ELSE IF (SMMETH.EQ.'EXP ') THEN
         CALL FUNBSM (SMMETH, SMOTIM(3), SMOTIM, TIME, IN, S, BLANK,
     *      NUMTIM, OUT)
      ELSE IF (SMMETH.EQ.'LINE') THEN
         CALL FUNBSM (SMMETH, SMOTIM(3), SMOTIM, TIME, IN, S, BLANK,
     *      NUMTIM, OUT)
C                                       2-point
      ELSE IF (SMMETH.EQ.'2PT ') THEN
         CALL TPTBSM (SMOTIM, TIME, IN, S, BLANK, NUMTIM, .FALSE., OUT)
C                                       2-point - hanning
      ELSE IF (SMMETH.EQ.'2PTH') THEN
         CALL TPTBSM (SMOTIM, TIME, IN, S, BLANK, NUMTIM, .TRUE., OUT)
C                                       Default = Boxcar
      ELSE
         CALL BOXBSM (SMOTIM, TIME, IN, S, BLANK, NUMTIM, OUT)
         END IF
C
 999  RETURN
      END
      SUBROUTINE TYSMHI
C-----------------------------------------------------------------------
C   TYSMHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME(2)*12
      INTEGER   LUN1, IERR, I, TIME(3), DATE(3), LIMIT, LIMIT2, J
      REAL      TIMBEG, TIMEND
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYSMO.INC'
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
      CALL HIOPEN (LUN1, DISKIN, FCNO(NCFILE), BUFFO, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
C                                       Task message
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN1, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Sources
      IF (NSOUWD.LE.0) THEN
         WRITE (HILINE,3000) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,3001) TSKNAM
         IF (DOSWNT) WRITE (HILINE,3002) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       1st 2 and label.
         WRITE (HILINE,3003) TSKNAM, XSOUR(1), XSOUR(2)
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Rest of sources
         DO 20 I = 3,NSOUWD,2
            WRITE (HILINE,3004) TSKNAM, XSOUR(I), XSOUR(I+1)
            CALL HIADD (LUN1, HILINE, BUFFO, IERR)
            IF (IERR.NE.0) GO TO 100
 20         CONTINUE
         END IF
C                                       Antennas
      IF (NANTSL.LE.0) THEN
         WRITE (HILINE,3005) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,3006) TSKNAM
         IF (DOAWNT) WRITE (HILINE,3007) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       1st 12 and label.
         LIMIT = MIN (12, NANTSL)
         WRITE (HILINE,3008) TSKNAM, (ANTENS(J),J=1,LIMIT)
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Rest of antennas
         DO 30 I = 13,NANTSL,12
            LIMIT = I
            LIMIT2 = I + 11
            LIMIT2 = MIN (NANTSL, LIMIT2)
            WRITE (HILINE,3009) TSKNAM, (ANTENS(J),J=LIMIT,LIMIT2)
            CALL HIADD (LUN1, HILINE, BUFFO, IERR)
            IF (IERR.NE.0) GO TO 100
 30         CONTINUE
         END IF
C                                       Timerange
      TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
      CALL HITIME (TIMBEG, TIMEND, LUN1, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       IF range
      WRITE (HILINE,2004) TSKNAM, BIF, EIF
      CALL HIADD (LUN1, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       SUBARRAY, TYVER
      WRITE (HILINE,2002) TSKNAM, SUBA, YTYPE, TYVER, YTYPE
      CALL HIADD (LUN1, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Interpolation fn
      WRITE (HILINE,2006) TSKNAM, XINTP
      CALL HIADD (LUN1, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       SY
      IF (YTYPE.EQ.'SY') THEN
         HILINE = TSKNAM // '/ Improper SY values blanked'
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       what was changed
      IF (XDOBLK.GT.0.0) THEN
         HILINE = TSKNAM // '/ Only blanked solutions changed'
      ELSE IF (XDOBLK.EQ.0.0) THEN
         HILINE = TSKNAM // '/ Blanked and good solutions changed'
      ELSE
         HILINE = TSKNAM // '/ Only good solutions changed'
         END IF
      CALL HIADD (LUN1, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Interpolation parms
      WRITE (HILINE,2007) TSKNAM, XIPARM(1), XIPARM(2), 'support'
      CALL HIADD (LUN1, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       extra parms
      IF ((XINTP.EQ.'GAUS') .OR. (XINTP.EQ.'EXP ') .OR.
     *   (XINTP.EQ.'LINE')) THEN
         WRITE (HILINE,2007) TSKNAM, XIPARM(6), XIPARM(7), 'FWHM'
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,4008) TSKNAM, CUTOFF
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       straight clipping parms
      WRITE (HILINE,4010) TSKNAM, APARM(1), APARM(6)
      CALL HIADD (LUN1, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,4011) TSKNAM, APARM(2), APARM(7)
      CALL HIADD (LUN1, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (YTYPE.EQ.'SY') THEN
         WRITE (HILINE,4012) TSKNAM, APARM(3), APARM(8)
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,4013) TSKNAM, APARM(4), APARM(9)
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       MWF Clipping parameters
         WRITE (HILINE,4015) TSKNAM, CPARM(1), CPARM(2), CPARM(3),
     *      CPARM(4), 'smooth'
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,4015) TSKNAM, CPARM(6), CPARM(7), CPARM(8),
     *      CPARM(9), 'clip'
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       MWF Clipping parameters
      ELSE
         WRITE (HILINE,4009) TSKNAM, CPARM(1), CPARM(2), 'smooth'
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,4009) TSKNAM, CPARM(6), CPARM(7), 'clip'
         CALL HIADD (LUN1, HILINE, BUFFO, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN1, T, BUFFO, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYSMHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2002 FORMAT (A6,'SUBARRAY =',I3,1X,A2,'VER =',I4,' /',A2,' table')
 2004 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 2006 FORMAT (A6,'INTERPOL = ''',A4,''' / Interpolation type')
 2007 FORMAT (A6,'INTPARM = ',2F10.5,' / Interpolation',A)
 4008 FORMAT (A6,'CUTOFF =',F8.5,'  / sum of weights cutoff')
 4009 FORMAT (A6,'CPARM: ',2F10.5,' / MWF clipping parms: ',A)
 4010 FORMAT (A6,'APARM: ',2F12.5,' / Clip min/max Tsys/Pdif')
 4011 FORMAT (A6,'APARM: ',2F12.5,' / Clip min/max Tant/Psum')
 4012 FORMAT (A6,'APARM: ',2F12.5,' / Clip min/max Tcal*Psum/Pdif/2')
 4013 FORMAT (A6,'APARM: ',2F12.5,' / Clip min/max Pgain')
 4015 FORMAT (A6,'CPARM:',4F8.3,' / MWF clip parms: ',A)
 3000 FORMAT (A6,'SOURCES = ''''     /All sources selected')
 3001 FORMAT (A6,'/Sources excluded:')
 3002 FORMAT (A6,'/Sources included:')
 3003 FORMAT (A6,'SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 3005 FORMAT (A6,'ANTENNAS = 0     /All antennas selected')
 3006 FORMAT (A6,'/Antennas excluded:')
 3007 FORMAT (A6,'/Antennas included:')
 3008 FORMAT (A6,'ANTENNAS = ',12(I3,' '))
 3009 FORMAT (A6,'           ',12(I3,' '))
      END
      LOGICAL FUNCTION TYWANT (SOUR, FQID, ANT, SUB, TIME)
C-----------------------------------------------------------------------
C   Function to determine if a source, FQid, antenna, subarray and time
C   have been selected.  Returns .TRUE. if task selection criteria are
C   met else .FALSE.
C   Inputs
C      SOUR     I    Source Id
C      FQID     I    FQ id
C      ANT      I    Antenna number
C      SUB      I    Subarray number
C      TIME     R    Time
C   Inputs from common:
C      SOUWAN   I(*)  List of selected source IDs.
C      NSOUWD   I     Number of values in SOUWAN, 0=any source
C      DOSWNT   L     If .TRUE. values in SOUWAN are selected else
C                     deselected.
C      FREQID   I     Selected FQ id, .le. 0 => any
C      SUBA     I     Selected subarray, .le. 0 => any
C      ANTENS   I(*)  List of selected antennas
C      NANTSL   I     Number of values in ANTENS, 0=any antenna
C      DOAWNT   L     If .TRUE. values in ANTENS are selected else
C                     deselected.
C      TSTART   D     Start time
C      TEND     D     End time
C-----------------------------------------------------------------------
      INTEGER   SOUR, FQID, ANT, SUB
      REAL      TIME
C
      LOGICAL WANT, SLCTD
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYSMO.INC'
C-----------------------------------------------------------------------
C                                       Want this source?
      WANT = SLCTD (SOUR, SOUWAN, NSOUWD, DOSWNT)
C                                       Want this FQ id?
      WANT = WANT .AND.
     *   ((FQID.EQ.FREQID) .OR. (FREQID.LE.0) .OR. (FQID.LE.0))
C                                       Check subarray
      WANT = WANT .AND.
     *   ((SUB.EQ.SUBA) .OR. (SUBA.LE.0) .OR. (SUB.LE.0))
C                                       Want this antenna?
      WANT = WANT .AND.
     *   SLCTD (ANT, ANTENS, NANTSL, DOAWNT)
C                                       Check time
      WANT = WANT .AND.
     *   ((TIME.GE.TSTART) .AND. (TIME.LE.TEND))
      TYWANT = WANT
C
 999  RETURN
      END
      SUBROUTINE TYFLAG (LUNI, LUNO, IRET)
C-----------------------------------------------------------------------
C   TVFLAG copies the TY/SY table applying flags to it
C   Inputs:
C      LUNI   I   LUN to use on read
C      LUNO   I   LUN to use on write
C   Outputs:
C      IRET   I   > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   LUNI, LUNO, IRET
C
      INCLUDE 'TYSMO.INC'
      INTEGER   IERR, TIMKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL, TA1KOL,
     *   TS1KOL, TA2KOL, TS2KOL, NKEY, TYNUMV(15), TYKOLS(15), KOLS(2),
     *   KEY(2,2), KEYSUB(2,2), NUMPOL, NUMIF, NUMANT, ITYRNO, ITYRNI,
     *   NUMREC, IRCODE, PARTFL, FULLFL, NDROP, I, LIMIT, FSOUR, FSUBA,
     *   FFREQI, FANTS(2), FIFS(2), FCHANS(2), IFLAG, IT, TYTIM, TYANT,
     *   NFGREC, FGLUN, TG1KOL, TG2KOL
      LOGICAL   TABLE, EXIST, FITASC, PFLAGS(4), WAS1
      REAL      FKEY(2,2), FTIMER(2), TIME
      CHARACTER COLHED(2)*24, REASON*24
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PTYTAB.INC'
      DATA FKEY /1.0, 0.0, 1.0, 0.0/
      DATA KEYSUB /4*1/
      DATA COLHED /'TIME ', 'ANTENNA NO. '/
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      FGLUN = 30
C                                       do not overwrite
      IF (ITYIN.NE.ITYOUT) THEN
         CALL ISTAB (YTYPE, DISKIN, CNOIN, ITYOUT, LUNO, BUFFI, TABLE,
     *      EXIST, FITASC, IERR)
         IF ((EXIST) .OR. (IERR.NE.0)) THEN
            IRET = 1
            MSGTXT = 'CANNOT OVERWRITE A PRE-EXISTING TABLE OTHER' //
     *         ' THAN INPUT'
            GO TO 990
            END IF
         END IF
C                                       Sort TY table to time-antenna
C                                       Need col. pointers, sort order.
      IF (YTYPE.EQ.'TY') THEN
         CALL TYINI ('READ', BUFFO, DISKIN, CNOIN, ITYIN, CATBLK,
     *      LUNI, ITYRNO, TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Get column pointers
         NKEY = 2
         CALL FNDCOL (NKEY, COLHED, 24, .TRUE., BUFFO, KOLS, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 999
         IRET = 0
         TYTIM = KOLS(1)
         TYANT = KOLS(2)
         TIMKOL = TYKOLS(TYRTIM)
         SOUKOL = TYKOLS(TYISID)
         ANTKOL = TYKOLS(TYIANT)
         SUBKOL = TYKOLS(TYISUB)
         FRQKOL = TYKOLS(TYIFQI)
         TS1KOL = TYKOLS(TYRTS1)
         TA1KOL = TYKOLS(TYRTA1)
         TG1KOL = -1
         TG2KOL = -1
         IF (NUMPOL.GT.1) THEN
            TS2KOL = TYKOLS(TYRTS2)
            TA2KOL = TYKOLS(TYRTA2)
         ELSE
            TS2KOL = -1000
            TA2KOL = -1000
            END IF
C                                       SY table
      ELSE
         CALL SYINI ('READ', BUFFO, DISKIN, CNOIN, ITYIN, CATBLK,
     *      LUNI, ITYRNO, TYKOLS, TYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Get column pointers
         NKEY = 2
         CALL FNDCOL (NKEY, COLHED, 24, .TRUE., BUFFO, KOLS, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 999
         IRET = 0
         TYTIM = KOLS(1)
         TYANT = KOLS(2)
         TIMKOL = TYKOLS(1)
         SOUKOL = TYKOLS(3)
         ANTKOL = TYKOLS(4)
         SUBKOL = TYKOLS(5)
         FRQKOL = TYKOLS(6)
         TS1KOL = TYKOLS(7)
         TA1KOL = TYKOLS(8)
         TG1KOL = TYKOLS(9)
         IF (NUMPOL.GT.1) THEN
            TS2KOL = TYKOLS(10)
            TA2KOL = TYKOLS(11)
            TG2KOL = TYKOLS(12)
         ELSE
            TS2KOL = -1000
            TA2KOL = -1000
            TG2KOL = -1000
            END IF
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFO, BUFFO, IERR)
      IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 999
      KEY(1,1) = TYTIM
      KEY(1,2) = TYANT
      KEY(2,1) = 0
      KEY(2,2) = 0
C                                       Sort to time-antenna order.
      IF (((BUFFO(43).NE.TYTIM) .OR. (BUFFO(44).NE.TYANT))) THEN
         CALL TABSRT (DISKIN, CNOIN, YTYPE, ITYIN, ITYIN, KEY, KEYSUB,
     *      FKEY, BUFFO, CATBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       sort FG table?
      CALL FGREFM (DISKIN, CNOIN, FGVER, CATBLK, LUNI, IERR)
      CALL FLGINI ('READ', FGBUFF, DISKIN, CNOIN, FGVER, CATBLK, FGLUN,
     *   IFGRNO, FGKOLS, FGNUMV, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1000) IERR, 'OPENING FLAG TABLE'
         GO TO 990
         END IF
      KEY(1,1) = 5
      KEY(1,2) = 1
      IF (((FGBUFF(43).NE.5) .OR. (FGBUFF(44).NE.1))) THEN
         CALL TABIO ('CLOS', 0, IFGRNO, FGBUFF, FGBUFF, IERR)
         CALL TABSRT (DISKIN, CNOIN, 'FG', FGVER, FGVER, KEY, KEYSUB,
     *      FKEY, BUFFO, CATBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL FLGINI ('READ', FGBUFF, DISKIN, CNOIN, FGVER, CATBLK,
     *      FGLUN, IFGRNO, FGKOLS, FGNUMV, IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1000) IERR, 'OPENING FLAG TABLE'
            GO TO 990
            END IF
         END IF
      NFGREC = FGBUFF(5)
      NUMFLG = 0
      TMFLST = -1.E6
C                                       Open files
      IF (YTYPE.EQ.'TY') THEN
         CALL TYINI ('READ', BUFFI, DISKIN, CNOIN, ITYIN, CATBLK, LUNI,
     *      ITYRNI, TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TYINI ('WRIT', BUFFO, DISKIN, CNOIN, ITYOUT, CATBLK,
     *      LUNO, ITYRNO, TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
      ELSE
         CALL SYINI ('READ', BUFFI, DISKIN, CNOIN, ITYIN, CATBLK, LUNI,
     *      ITYRNI, TYKOLS, TYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL SYINI ('WRIT', BUFFO, DISKIN, CNOIN, ITYOUT, CATBLK,
     *      LUNO, ITYRNO, TYKOLS, TYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
      NUMREC = BUFFI(5)
      PARTFL = 0
      FULLFL = 0
      DO 90 ITYRNI = 1,NUMREC
         CALL TABIO ('READ', IRCODE, ITYRNI, RECORD, BUFFI, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 90
            END IF
         IF (IRET.GT.0) GO TO 900
         IF (YTYPE.EQ.'SY') THEN
            TIME = RECD(TIMKOL)
         ELSE
            TIME = RECR(TIMKOL)
            END IF
C                                       new time: NXTFLG clone
         IF (TMFLST.LT.TIME) THEN
            TMFLST = TIME
C                                       any to drop from list?
 10         NDROP = 0
            IF ((NUMFLG.GT.0) .AND. (TIMORD)) THEN
               DO 20 I = 1,NUMFLG
                  IF (FLGTND(I).LT.TIME) NDROP = I
 20               CONTINUE
               END IF
C                                       Compress, dropping flag.
            IF (NDROP.GT.0) THEN
               IF (NDROP.LT.NUMFLG) THEN
                  LIMIT = NDROP + 1
                  DO 30 I = LIMIT,NUMFLG
                     IT = I - 1
                     FLGTST(IT) = FLGTST(I)
                     FLGTND(IT) = FLGTND(I)
                     FLGSOU(IT) = FLGSOU(I)
                     FLGANT(IT) = FLGANT(I)
                     FLGFQD(IT) = FLGFQD(I)
                     FLGSUB(IT) = FLGSUB(I)
                     FLGBIF(IT) = FLGBIF(I)
                     FLGEIF(IT) = FLGEIF(I)
                     FLGPOL(1,IT) = FLGPOL(1,I)
                     FLGPOL(2,IT) = FLGPOL(2,I)
                     FLGPOL(3,IT) = FLGPOL(3,I)
                     FLGPOL(4,IT) = FLGPOL(4,I)
 30                  CONTINUE
                  END IF
               NUMFLG = NUMFLG - 1
               GO TO 10
               END IF
C                                       read FG table
 40         LIMIT = IFGRNO
            DO 50 I = LIMIT,NFGREC
               IFGRNO = I
               CALL TABFLG ('READ', FGBUFF, IFGRNO, FGKOLS, FGNUMV,
     *            FSOUR, FSUBA, FFREQI, FANTS, FTIMER, FIFS, FCHANS,
     *            PFLAGS, REASON, IERR)
               IF (IERR.GT.0) GO TO 900
               IF (IERR.EQ.0) THEN
                  IF (TIME.LT.FTIMER(1)) GO TO 60
                  IF (TIME.GT.FTIMER(2)) GO TO 50
                  IF ((FFREQI.GT.0) .AND. (FREQID.GT.0) .AND.
     *               (FFREQI.NE.FREQID)) GO TO 50
                  IF ((FIFS(1).GT.0) .AND. (FIFS(1).GT.EIF)) GO TO 50
                  IF ((FIFS(2).GT.0) .AND. (FIFS(2).LT.BIF)) GO TO 50
                  IF ((FSUBA.GT.0) .AND. (SUBA.GT.0) .AND.
     *               (FSUBA.NE.SUBA)) GO TO 50
                  IF ((.NOT.PFLAGS(1)) .AND. (.NOT.PFLAGS(2))) GO TO 50
                  IF ((FANTS(1).GT.0) .AND. (FANTS(2).GT.0)) GO TO 50
C                                       ignore channel dependent
                  IF (FCHANS(1).LE.0) FCHANS(1) = 1
                  IF (FCHANS(2).LE.0) FCHANS(2) = CATBLK(KINAX+JLOCF)
                  IF ((FCHANS(1).GT.1) .OR.
     *               (FCHANS(2).LT.CATBLK(KINAX+JLOCF))) GO TO 50
                  IF (NUMFLG.EQ.MAXFLG) THEN
                     MSGTXT = 'NUMBER OF FLAGS AT 1 TIME EXCEEDS LIMIT'
                     CALL MSGWRT (7)
                     MSGTXT = 'NOT ALL TABLE ROWS PROPERLY FLAGGED'
                     CALL MSGWRT (7)
                     GO TO 60
                     END IF
                  NUMFLG = NUMFLG + 1
C                                       Fill in tables
                  FLGTST(NUMFLG) = FTIMER(1)
                  FLGTND(NUMFLG) = FTIMER(2)
                  FLGSOU(NUMFLG) = FSOUR
                  FLGFQD(NUMFLG) = FFREQI
                  FLGANT(NUMFLG) = MAX (FANTS(1), FANTS(2))
                  FLGSUB(NUMFLG) = FSUBA
                  FLGBIF(NUMFLG) = FIFS(1)
                  FLGEIF(NUMFLG) = FIFS(2)
                  IF (FLGBIF(NUMFLG).LE.0) FLGBIF(NUMFLG) = 1
                  IF (FLGEIF(NUMFLG).LE.0) THEN
                     IF (JLOCIF.GT.0) FLGEIF(NUMFLG) =
     *                  CATBLK(KINAX+JLOCIF)
                     IF (JLOCIF.LE.0) FLGEIF(NUMFLG) = 1
                     END IF
C
                  CALL COPY (4, PFLAGS, FLGPOL(1,NUMFLG))
                  GO TO 40
                  END IF
 50            CONTINUE
            END IF
C                                       end get new flags
C                                       check flags to see if apply
 60      WAS1 = .FALSE.
         DO 70 IFLAG = 1,NUMFLG
            IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *         GO TO 70
            I = RECORD(SOUKOL)
            IF ((FLGSOU(IFLAG).GT.0) .AND. (I.NE.FLGSOU(IFLAG)) .AND.
     *         (I.GT.0)) GO TO 70
            I = RECORD(ANTKOL)
            IF ((FLGANT(IFLAG).NE.I) .AND. (FLGANT(IFLAG).GT.0) .AND.
     *         (I.GT.0)) GO TO 70
            I = RECORD(SUBKOL)
            IF ((FLGSUB(IFLAG).NE.I) .AND. (FLGSUB(IFLAG).GT.0) .AND.
     *         (I.GT.0)) GO TO 70
            I = RECORD(FRQKOL)
            IF ((FLGFQD(IFLAG).NE.I) .AND. (FLGFQD(IFLAG).GT.0) .AND.
     *         (I.GT.0)) GO TO 70
            CALL COPY (4, FLGPOL(1,IFLAG), PFLAGS)
C                                       something to be flagged w this
            DO 65 I = FLGBIF(IFLAG),FLGEIF(IFLAG)
               IF (PFLAGS(1)) THEN
                  RECR(TS1KOL+I-1) = FBLANK
                  RECR(TA1KOL+I-1) = FBLANK
                  IF (TG1KOL.GT.0) RECR(TG1KOL+I-1) = FBLANK
                  END IF
               IF ((NUMPOL.GT.1) .AND. (PFLAGS(2))) THEN
                  RECR(TS2KOL+I-1) = FBLANK
                  RECR(TA2KOL+I-1) = FBLANK
                  IF (TG2KOL.GT.0) RECR(TG2KOL+I-1) = FBLANK
                  END IF
 65            CONTINUE
            WAS1 = .TRUE.
 70         CONTINUE
C                                       all bad?
         IF (WAS1) THEN
            PARTFL = PARTFL + 1
            DO 75 I = BIF,EIF
               IF (RECR(TS1KOL+I-1).NE.FBLANK) GO TO 80
               IF (RECR(TA1KOL+I-1).NE.FBLANK) GO TO 80
               IF (TG1KOL.GT.0) THEN
                  IF (RECR(TG1KOL+I-1).NE.FBLANK) GO TO 80
                  END IF
               IF (TG2KOL.GT.0) THEN
                  IF (RECR(TG2KOL+I-1).NE.FBLANK) GO TO 80
                  END IF
               IF (NUMPOL.GT.1) THEN
                  IF (RECR(TS2KOL+I-1).NE.FBLANK) GO TO 80
                  IF (RECR(TA2KOL+I-1).NE.FBLANK) GO TO 80
                  END IF
 75            CONTINUE
            FULLFL = FULLFL + 1
            GO TO 90
            END IF
 80      CALL TABIO ('WRIT', IRCODE, ITYRNO, RECORD, BUFFO, IRET)
         IF (IRET.GT.0) GO TO 900
         ITYRNO = ITYRNO + 1
 90      CONTINUE
      PARTFL = PARTFL - FULLFL
      WRITE (MSGTXT,1090) ITYRNO, ITYIN, ITYOUT
      CALL MSGWRT (3)
      WRITE (MSGTXT,1091) PARTFL
      IF (PARTFL.GT.0) CALL MSGWRT (3)
      WRITE (MSGTXT,1092) FULLFL
      IF (FULLFL.GT.0) CALL MSGWRT (3)
C
 900  CALL TABIO ('CLOS', 0, NUMREC, BUFFI, BUFFI, IERR)
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFO, BUFFO, IERR)
      CALL TABIO ('CLOS', 0, IFGRNO, FGBUFF, FGBUFF, IERR)
      GO TO 999
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYFLAG: ERROR',I5,1X,A)
 1090 FORMAT ('TYFLAG: wrote',I9,' records from table',I3,' to table',
     *   I3)
 1091 FORMAT ('TYFLAG:      ',I9,' records were partly flagged')
 1092 FORMAT ('TYFLAG:      ',I9,' records were omitted since',
     *   ' fully flagged')
      END
