LOCAL INCLUDE 'SDVEL.INC'
      INCLUDE 'INCS:DSEL.INC'
C                                      Character declerations
      CHARACTER  NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
C                                      Input parameters
      HOLLERITH  XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL       XSIN, XDISIN, XSOUT, XDSOUT, XFLAG, XTIME(8),
     *   XPARM(10), XCO(6), XBADD(10), TIME1, TIME2
      INTEGER   SEQIN, CNOIN, DISKIN, SEQOUT, CNOOUT, DISOUT, UVBFSZ,
     *   CNTFLG, CNTMSG, COTYPE
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDSOUT, XFLAG, XTIME, XPARM, XCO, XBADD,
     *   SEQIN, CNOIN, DISKIN, SEQOUT, CNOOUT, DISOUT, UVBFSZ, CNTFLG,
     *   CNTMSG, TIME1, TIME2, COTYPE
C                                      Buffers
      REAL      BUFFER(UVBFSS), BUFF1(512), BUFFC(3*MAXCIF),
     *   BUFF2(UVBFSS)
      COMMON /CMBUFF/ BUFFER, BUFFC, BUFF2, BUFF1
C                                      File Control
      INTEGER   ICHLUN, NDECOM, DECOM(2,MAXIF*4), LLOCWT, LLOCSC, ILOCSC
      COMMON /FILCON/ ICHLUN, NDECOM, DECOM, LLOCWT, LLOCSC, ILOCSC
C                                      Misc. parameters
      DOUBLE PRECISION RAPP0, DAPP0, FRFROM, REFRA, REFDEC, USRRA,
     *   USRDEC
      INTEGER   KSEQO, NUMFRQ, INCFU, INCIFU, INCSU
      LOGICAL   WUVCMP, NEWSCN, NEWOFF
      COMMON /OPARM/ RAPP0, DAPP0, FRFROM, REFRA, REFDEC, USRRA, USRDEC,
     *   KSEQO, NUMFRQ, INCFU, INCIFU, INCSU, WUVCMP, NEWSCN, NEWOFF
C                                       O/P catalogue
      INTEGER   CATOUT(256)
      HOLLERITH CATOH(256)
      DOUBLE PRECISION CATOD(128)
      REAL      CATOR(256)
      COMMON /OUTHDR/ CATOUT
      EQUIVALENCE (CATOUT, CATOH, CATOR, CATOD)
C                                       Flag table info
      INTEGER   FKNCOR, FKNCF, FKNCIF, FKNCS, FKCOR0
C                                       FLAG table info
      COMMON /SDVLFG/ FKNCOR, FKNCF, FKNCIF, FKNCS, FKCOR0
LOCAL END
      PROGRAM SDVEL
C-----------------------------------------------------------------------
C! SDVEL shifts spectral-line single-dish data to a specified velocity.
C# Calibration Spectral UV VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2001, 2004-2009, 2011, 2015
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
C   SDVEL will shift spectral line data to a specified velocity.  The
C   program assumes that the telescope has tracked the velocity in the
C   header for the reference coordinates in the header.  It then shifts
C   each spectrum so that that velocity is wrt the actual coordinate
C   observed rather than the reference coordinate.  It works only on
C   single-source, single-dish uv data sets.
C   Inputs:
C      AIPS adverb          Description.
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      OUTNAME....Output UV file name.            Standard defaults.
C      OUTCLASS...Output UV file name (class).    Standard defaults.
C      OUTSEQ.....Output UV file name (seq. #).   0 => highest unique
C      OUTDISK....Disk drive # of output UV file. 0 => highest with
C                 space for the file.
C      FLAGVER....Flag file to apply (< 0 -> none)
C      DPARM......Controls:
C                 (1) Tells the task when to compute the reference
C                 doppler shift.
C                 0 -> only at the start using the reference coordinate.
C                 1 -> only at the start, but using the first coordinate.
C                 2 -> on each "off" scan (detected by a missing scan
C                 number) using the last coordinate before the off.
C                 3 -> on each scan using the last coordinate of the
C                 previous scan.
C                 (2) A warning is issued if the shift exceeds DPARM(2)
C                 channels.  0 -> NCHAN/20.  The task limits the number
C                 of consecutive samples warned to 10 but if there are
C                 then good samples, it will resume warnings on the next
C                 "bad".
C                 (3) A sample is deleted if the shift exceeds DPARM(3)
C                 channels.  0 -> NCHAN.  This may be used to detect and
C                 flag periods in which the antenna is blown way off
C                 course.
C      BADDISK....Disks to avoid for scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'SDVEL.INC'
      DATA PRGM /'SDVEL '/
C-----------------------------------------------------------------------
C                                       Get input parameters
C                                       and open O/P file.
      CALL SDVLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read, shift and write
      CALL CVSHFT (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Update history
      CALL SDVLHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE SDVLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   SDVLIN gets the input parms for SDVEL, fins the input file
C   opens the output file and performs some checks on the
C   validity of the SDVEL operation.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C     JERR    I     Error code: 0 => ok
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH CATH(256)
      CHARACTER STAT*4, PRGN*6, UTYPE*2, CHSCAN*8
      INTEGER   JERR, NUMHIS, NPARM, IROUND, IERR, I, CATSAV(256),
     *   KEY(2,2), NREAD, TVER, MODE, KEYSUB(2,2)
      REAL      FKEY(2,2)
      LOGICAL   F, TABLE, EXIST, FITASC, ISNEG
      INCLUDE 'SDVEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA F /.FALSE./
      DATA KEY  /5,0, 1,0/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 49
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
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Obtain disc parms
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISOUT = IROUND (XDSOUT)
C                                       Check file exists
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       If it does extract the CATBLK
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1002) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      IF (TYPUVD.NE.1) THEN
         WRITE (MSGTXT,1005) TYPUVD
         JERR = 8
         GO TO 990
         END IF
      CALL H2CHR (8, 1, CATH(KHPTP+2*ILOCU), CHSCAN)
      COTYPE = -1
      IF (CHSCAN.EQ.'RA') COTYPE = 0
      IF (CHSCAN.EQ.'GLON') COTYPE = 1
      IF (CHSCAN.EQ.'ELON') COTYPE = 2
      IF (CHSCAN.NE.'RA') THEN
         MSGTXT = 'HANDLES ONLY RA/DEC COORDS, NOT ' // CHSCAN
         JERR = 8
         GO TO 990
         END IF
      IF (JLOCIF.GE.0) THEN
         MSGTXT = 'DOES NOT HANDLE MULTIPLE IFS'
         JERR = 8
         IF (CATBLK(KINAX+JLOCIF).GT.1) GO TO 990
         END IF
C                                       SCAN pointer
      CHSCAN = 'SCAN'
      CALL AXEFND (4, CHSCAN, CATBLK(KIPCN), CATH(KHPTP), ILOCSC, IERR)
      IF (IERR.NE.0) ILOCSC = -1
C                                       Compressed data?
      WUVCMP = CATBLK(KINAX).EQ.1
C                                       BADDISK
      DO 10 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 10      CONTINUE
C                                       Put selection criteria into
C                                       correct common.
      FGVER = IROUND (XFLAG)
      DOFLAG = FGVER.GE.0
C                                       Set LUN's
C                                       UV LUN
      IULUN = 26
C                                       FG table LUN
      IFLUN = 30
C                                       Check if single source file
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, IFLUN, BUFF1, TABLE, EXIST,
     *   FITASC, IERR)
C                                       If single give warning message.
      IF ((ILOCSU.GE.0) .OR. ((EXIST) .AND. (TABLE))) THEN
         MSGTXT = 'WARNING: SOURCE TABLE AND/OR IDS IGNORED!'
         CALL MSGWRT (7)
         END IF
C                                       If multi-source file sort the
C                                       CL table to time-ant order.
C                                       Put new values in CATOUT
      CALL COPY (256, CATBLK, CATSAV)
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      CALL UVCREA (DISOUT, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         JERR = 8
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CCNO
      FVOL(NCFILE) = DISOUT
      FRW(NCFILE) = 2
      CALL COPY (256, CATBLK, CATOUT)
      CALL COPY (256, CATSAV, CATBLK)
C                                       Set output CNO
      CNOOUT = CCNO
C                                       copy keywords
      CALL KEYCOP (DISKIN, CNOIN, DISOUT, CNOOUT, IERR)
C                                       Fill AN information
C                                       into common in DANS.INC
      CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1070) JERR
         GO TO 990
         END IF
C
      NUMFRQ = CATBLK(KINAX+JLOCF)
      UVBFSZ = UVBFSS * 2
C                                       Init Flag file
      CALL FNDEXT ('FG', CATBLK, TVER)
      DOFLAG = DOFLAG .AND. (TVER.GT.0)
      TIMORD = ISORT(1:1).EQ.'T'
      IF (DOFLAG) THEN
         MSGTXT = 'Some data may be flagged and interpolated'
         CALL MSGWRT (3)
         TMFLST = -1.0E20
         NUMFLG = 0
         FKNCOR = NCOR
         FKCOR0 = ICOR0
         FKNCF = INCF / CATBLK(KINAX)
         FKNCIF = INCIF / CATBLK(KINAX)
         FKNCS = INCS / CATBLK(KINAX)
         MSGSUP = 32000
C                                       Reformat table?
         CALL FGREFM (DISKIN, CNOIN, FGVER, CATBLK, IFLUN, JERR)
         CALL FLGINI ('READ', FGBUFF, DISKIN, CNOIN, FGVER, CATBLK,
     *      IFLUN, IFGRNO, FGKOLS, FGNUMV, JERR)
         MSGSUP = 0
         IF (JERR.NE.0) THEN
            DOFLAG = F
C                                       Sort to time order.
         ELSE IF (FGBUFF(43).NE.KEY(1,1)) THEN
            CALL TABIO ('CLOS', 0, NREAD, BUFFER, FGBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABSRT (DISKIN, CNOIN, 'FG', FGVER, FGVER, KEY, KEYSUB,
     *         FKEY, FGBUFF, CATBLK, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Re initialize.
            CALL FLGINI ('READ', FGBUFF, DISKIN, CNOIN, FGVER, CATBLK,
     *         IFLUN, IFGRNO, FGKOLS, FGNUMV, IERR)
            END IF
         END IF
      IF (DOFLAG) THEN
         WRITE (MSGTXT,1035) FGVER
      ELSE
         MSGTXT = 'Applying no flag table to the input data'
         END IF
      CALL MSGWRT (3)
C                                       First set freq. selection
      BCHAN = 1
      ECHAN = NUMFRQ
      BIF = 1
      EIF = 1
      IF (JLOCIF.GE.0) EIF = CATBLK(KINAX+JLOCIF)
C                                       Disk, volume no.
      IUDISK = DISKIN
      IUCNO = CNOIN
C                                       Pointers for freq/IF/Stokes
      KNCF = INCF / CATBLK(KINAX)
      KNCIF = INCIF / CATBLK(KINAX)
      KNCS = INCS / CATBLK(KINAX)
C                                       No antenna selection
      CALL FILL (MAXANT, 0, ANTENS)
C                                       Copy CATBLK to CATUV
      CALL COPY (256, CATBLK, CATUV)
C                                       Position controls:
      MODE = IROUND (XPARM(1))
      IF ((MODE.LT.0) .OR. (MODE.GT.3)) MODE = 0
      XPARM(1) = MODE
      MODE = IROUND (XPARM(2))
      IF ((MODE.LT.0) .OR. (MODE.GT.2)) MODE = 0
      XPARM(2) = MODE
      ISNEG = (XCO(1).LT.0.0) .OR. (XCO(2).LT.0.0) .OR. (XCO(3).LT.0.0)
      USRRA = ABS (XCO(1)) + ABS (XCO(2))/60.0D0 + ABS (XCO(3))/3600.0D0
      IF (ISNEG) USRRA = -USRRA
      USRRA = USRRA * 15.0D0
      ISNEG = (XCO(4).LT.0.0) .OR. (XCO(5).LT.0.0) .OR. (XCO(6).LT.0.0)
      USRDEC = ABS(XCO(4)) + ABS(XCO(5))/60.0D0 + ABS(XCO(6))/3600.0D0
      IF (ISNEG) USRDEC = -USRDEC
      GO TO 999
C                                       Write the error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDVLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1001 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1002 FORMAT ('SDVLIN: ERROR',I3,' COPYING CATBLK')
 1005 FORMAT ('WORKS ON SINGLEDISH DATA ONLY, NOT TYPE',I3)
 1035 FORMAT ('SDVLIN: applying flag table version',I5,' to the data')
 1050 FORMAT ('SDVLIN: ERROR',I3,' CREATING OUTPUT FILE')
 1070 FORMAT ('SDVLIN: ERROR',I3,' OBTAINING ANTENNA INFORMATION')
      END
      SUBROUTINE CVSHFT (IRET)
C-----------------------------------------------------------------------
C   CVSHFT reads the UV data, shifts it to the new velocity/frequency
C   and writes it to the O/P file.
C   Outputs:
C      IRET   I   Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH CATH(256)
      CHARACTER NAME*48, SNAMES*8, TC1*1, TC2*1
      INTEGER   VISOFF, JBUFSZ, IRET, LUN, FIND, BIND, LENBU, INIO, NIO,
     *   LRECO, NOPOL, NPERC, BO, I, XCOUNT, NCORI, ONXRNO,
     *   JNXRNO, SBCHAN, SECHAN, J, LDATA, KBIND, TIT1(3),
     *   TIT2(3), JERR, IBIND, NCOPY, VISNUM, COUNTD, CNTLSG
      LOGICAL   T, F, DROP
      REAL      VISIN(3*MAXCIF), WORK(3*MAXCIF), TIME, RPARM(20),
     *   REPDEL, CATR(256), FTIME, LTIME, REPMAX(2), TS1, TS2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'SDVEL.INC'
      INCLUDE 'INCS:DCVL.INC'
      EQUIVALENCE (CATBLK, CATH, CATR)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, BO /40,1/
C-----------------------------------------------------------------------
C                                       Set lengths of input axes.
      NOPOL = CATBLK(KINAX+JLOCS)
      NUMFRQ = CATBLK(KINAX+JLOCF)
      NEWSCN = .TRUE.
      NEWOFF = .TRUE.
      RAPP0 = -1000.
      DAPP0 = -1000.
      CNTMSG = 0
      CNTLSG = 0
      CNTFLG = 0
      IF (XPARM(3).LE.0.0) XPARM(3) = NUMFRQ / 20.0
      IF (XPARM(4).LE.0.0) XPARM(4) = NUMFRQ
      TIME1 = (XTIME(2) + (XTIME(3) + XTIME(4)/60.0) / 60.0) / 24.0
     *   + XTIME(1)
      TIME2 = (XTIME(6) + (XTIME(7) + XTIME(8)/60.0) / 60.0) / 24.0
     *   + XTIME(5)
      IF (TIME2.LE.0.0) TIME2 = 1.E6
C                                       Assume no IF
      CNNIF = 1
      CFOFF(1) = 0
      CSBAND(1) = 1
      CFINC(1) = CATR(KRCIC+JLOCF)
      CALL H2CHR (8, 1, CATH(KHOBJ), SNAMES)
C                                       Get data compression pointers
      INCFU = INCF
      INCIFU = INCIF
      INCSU = INCS
      IF (WUVCMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), LLOCWT,
     *      JERR)
         IF ((JERR.NE.0) .OR. (LLOCWT.LT.0)) THEN
            IRET = 5
            MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
            GO TO 990
            END IF
         CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP), LLOCSC,
     *      JERR)
         SBCHAN = 0
         SECHAN = 0
         CALL CMPARM (1, 1, 1, NUMFRQ, SBCHAN, SECHAN, NDECOM, DECOM)
         INCFU = INCF * 3
         INCIFU = INCIF * 3
         INCSU = INCS * 3
         END IF
      NCORI = (LREC - NRPARM) / CATBLK(KINAX)
      NCOPY = LREC - NRPARM
C
      LENBU = 1
      LRECO = LREC
      JBUFSZ = UVBFSS * 2
C                                       Open input data file
      CALL ZPHFIL ('UV', DISKIN, CNOIN, 1, UFILE, IRET)
      CALL ZOPEN (IULUN, IUFIND, DISKIN, UFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'INPUT'
         GO TO 990
         END IF
      CALL UVINIT ('READ', IULUN, IUFIND, NVIS, VISOFF, LREC, LENBU,
     *   JBUFSZ, BUFF2, BO, IBIND, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'INPUT'
         GO TO 990
         END IF
C                                       Open output file.
      CALL ZPHFIL ('UV', DISOUT, CNOOUT, 1, NAME, IRET)
      CALL ZOPEN (LUN, FIND, DISOUT, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'OUTPUT'
         GO TO 990
         END IF
C                                       Init output vis file for write
      CALL UVINIT ('WRIT', LUN, FIND, CATBLK(KIGCN), VISOFF,
     *   LRECO, LENBU, JBUFSZ, BUFFER, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'OUTPUT'
         GO TO 990
         END IF
      IF (WUVCMP) KBIND = BIND
C                                       Do the shift
      XCOUNT = 0
      NPERC = 10
C                                       Loop over visibilities
      ONXRNO = 0
      JNXRNO = 1
      VISNUM = 1
      FTIME = 0.0
      LTIME = 0.0
      DO 100 I = 1,NVIS
C                                       Read input data
         CALL UVDISK ('READ', IULUN, IUFIND, BUFF2, INIO, IBIND, IRET)
         IF ((IRET.EQ.4) .OR. (INIO.LE.0)) GO TO 110
         IF (IRET.NE.0) GO TO 999
         TIME = BUFF2(IBIND+ILOCT)
         IF ((TIME.LT.TIME1) .OR. (TIME.GT.TIME2)) GO TO 100
         CALL RCOPY (NRPARM, BUFF2(IBIND), RPARM)
         IF (WUVCMP) THEN
            CALL ZUVXPN (NCORI, BUFF2(IBIND+NRPARM),
     *         BUFF2(IBIND+LLOCWT), VISIN)
         ELSE
            CALL RCOPY (NCOPY, BUFF2(IBIND+NRPARM), VISIN)
            END IF
C                                       Flagging ?
         IF (DOFLAG) THEN
            CALL CVFLAG (RPARM, VISIN, DROP, IRET)
C                                       Remove fully flagged samples
            IF (DROP) GO TO 100
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Message every scan
         TIME = BUFF2(IBIND+ILOCT)
         IF (FTIME.LE.0.0) FTIME = TIME
         IF (ILOCSC.GE.0) THEN
            JNXRNO = BUFF2(IBIND+ILOCSC)
            IF (JNXRNO.NE.ONXRNO) THEN
               IF (ONXRNO.GT.0) THEN
                  IF ((XPARM(1).EQ.2.0) .AND. (JNXRNO-ONXRNO.GT.1))
     *               NEWOFF = .TRUE.
                  IF (XPARM(1).LE.1.) NEWOFF = .TRUE.
                  CALL TFDHMS (FTIME, 1, TC1, TIT1, TS1)
                  CALL TFDHMS (LTIME, 1, TC2, TIT2, TS2)
                  WRITE (MSGTXT,1010) ONXRNO, SNAMES, TIT1, TS1, TIT2,
     *               TS2
                  IF (MSGTXT(38:38).EQ.' ') MSGTXT(38:38) = '0'
                  IF (MSGTXT(55:55).EQ.' ') MSGTXT(55:55) = '0'
                  CALL MSGWRT (3)
                  IF (COUNTD.GT.0) THEN
                     REPDEL = REPDEL / COUNTD
                     WRITE (MSGTXT,1015) REPMAX(1), REPDEL, REPMAX(2)
                     CALL MSGWRT (3)
                     END IF
                  END IF
               NEWSCN = .TRUE.
               FTIME = TIME
               ONXRNO = JNXRNO
               REPDEL = 0.0
               REPMAX(1) = 10000.0
               REPMAX(2) = -10000.0
               COUNTD = 0
            ELSE
               NEWSCN = .FALSE.
               END IF
            END IF
         LTIME = TIME
C                                       Shift desired sources
         IF (NEWOFF) FRFROM = -1000.0D0
         IF (VISNUM.EQ.1) THEN
            RAPP0 = USRRA
            DAPP0 = USRDEC
            END IF
         CALL DSHIFT (DISKIN, CNOIN, RPARM, VISIN, WORK, WUVCMP, VISNUM,
     *      XPARM, RAPP0, DAPP0, FRFROM, REPDEL, REPMAX, COUNTD, CNTMSG,
     *      CNTLSG, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1050) IRET
            GO TO 990
C                                       skip = flagged
         ELSE IF (IRET.LT.0) THEN
            CNTFLG = CNTFLG + 1
C                                       Write new
         ELSE
            VISNUM = VISNUM + 1
            NEWSCN = .FALSE.
            NEWOFF = .FALSE.
C
            CALL RCOPY (NRPARM, RPARM, BUFFER(BIND))
            LDATA = LREC - NRPARM
            IF (WUVCMP) THEN
               LDATA = LDATA * 3
               CALL RCOPY (LDATA, VISIN, BUFFC)
            ELSE
               CALL RCOPY (LDATA, VISIN, BUFFER(BIND+NRPARM))
               END IF
C                                       Write new
            NIO = 1
            XCOUNT = XCOUNT + 1
            IF (WUVCMP) THEN
               DO 90 J = 1, NDECOM
                  CALL ZUVPAK (DECOM(1,J), BUFFC, BUFFER(BIND+LLOCWT),
     *               BUFFER(BIND+NRPARM))
 90               CONTINUE
               CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, KBIND, IRET)
               BIND = KBIND
            ELSE
               CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IRET)
               END IF
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRIT', 'OUTPUT'
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       If FINISH shut down output
 110  NVIS = XCOUNT
C                                       Flush output
      NIO = 0
      IF (WUVCMP) THEN
         CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, KBIND, IRET)
      ELSE
         CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FLUSH', 'OUTPUT'
         GO TO 990
         END IF
C                                       Final message
      CALL TFDHMS (FTIME, 1, TC1, TIT1, TS1)
      CALL TFDHMS (LTIME, 1, TC2, TIT2, TS2)
      WRITE (MSGTXT,1010) ONXRNO, SNAMES, TIT1, TS1, TIT2, TS2
      IF (MSGTXT(38:38).EQ.' ') MSGTXT(38:38) = '0'
      IF (MSGTXT(55:55).EQ.' ') MSGTXT(55:55) = '0'
      CALL MSGWRT (3)
      IF (COUNTD.GT.0) THEN
         REPDEL = REPDEL / COUNTD
         WRITE (MSGTXT,1015) REPMAX(1), REPDEL, REPMAX(2)
         CALL MSGWRT (3)
         END IF
      IF (CNTMSG.GT.0) THEN
          WRITE (MSGTXT,1110) CNTMSG
          CALL MSGWRT (6)
          END IF
      IF (CNTFLG.GT.0) THEN
          WRITE (MSGTXT,1111) CNTFLG
          CALL MSGWRT (6)
          END IF
C                                       Compress output file.
      NVIS = XCOUNT + VISOFF
      CALL UCMPRS (NVIS, DISOUT, CNOOUT, LUN, CATOUT, IRET)
C                                       Close files
      CALL ZCLOSE (LUN, FIND, IRET)
      CALL ZCLOSE (IULUN, IUFIND, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CVSHFT: ERROR',I5,1X,A,'ING ',A,' FILE')
 1010 FORMAT ('Shift scan',I6,' source ',A8,I4,'/',2(I2.2,':'),F4.1,
     *   ' -',I4,'/',2(I2.2,':'),F4.1)
 1015 FORMAT (5X,'Min, Avg, Max shift =',2(F12.5,','),F12.5)
 1050 FORMAT ('CVSHFT: ERROR',I3,' RETURNED FROM DSHIFT')
 1110 FORMAT ('There were',I7,' apparently excessive shifts performed')
 1111 FORMAT ('There were',I7,' excessive shifts deleted')
      END
      SUBROUTINE DSHIFT (DISKIN, CNOIN, RPARMS, VIS, WORK, WUVCMP,
     *   VISNUM, XPARM, RAPP0, DAPP0, FRFROM, REPDEL, REPMAX, COUNTD,
     *   CNTMSG, CNTLSG, IRET)
C-----------------------------------------------------------------------
C   DSHIFT calculates the necessary channel shift and does it by calling
C   the routine ACSHFT
C   Input:
C      DISKIN   I        Volume number
C      CNOIN    I        File catalogue number
C      WUVCMP   L        Input/output data are compressed
C      VISNUM   I        Visibility number. 1 => some things need to be
C                        opened. -1 => some need to be closed.
C      XPARM    R(*)     (1) = 0 => start with ref coord, else 1st
C                           coord
C                        (2) warning level - shifts > xparm(2) warned
C                        (3) delete it level - shifts > xparm(3) flagged
C   Input/Output:
C      RPARMS   R(*)     Random parameters
C      VIS      R(3,*)   The complex visibility + weight; On output will
C                        contain the shifted data.
C      RAPP0    D        Apparent RA of date of ref < 0 => compute
C      DAPP0    D        Apparent RA of date of ref < -90 => compute
C      FRFROM   D        Velocity ref channel freq of current ref,
C                        <= 0.0 => compute please
C      REPDEL   R        Sum of shifts
C      REPMAX   R(2)     Min, Max of shifts
C      COUNTD   I        Number in sum
C   Output:
C      WORK     R(*)     Work buffer (>= 8192)
C      IRET     I        Error code, = 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, VISNUM, COUNTD, IRET, CNTMSG, CNTLSG
      INTEGER DIR
      REAL      RPARMS(*), VIS(*), XPARM(*), WORK(*), REPDEL, REPMAX(2)
      LOGICAL   WUVCMP
      DOUBLE PRECISION RAPP0, DAPP0, FRFROM, OBSPOS(3)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IANT1, IANT2, I, IPOL, IFRQ, INDEX, ISB, FINDEX,
     *   TIT(3), NFREQ, INCSU, INCIFU, INCFU, OLDSOU, NNIF, SUBARR,
     *   GAMMA, IDUM1, IDUM2, ISMTH, NDAY, IIYEAR, LIMMSG
      DOUBLE PRECISION FINT, DOPVEL, VELTOT, FREQTO, DELI, LRAPP, LDAPP,
     *   XRA, XDEC, JD, EQUIN, UT, DTIME, DXANT, DYANT, DZANT, VSUN,
     *   LRAPPF, LDAPPF, EUT, EDELI, JDA, ARRLON
      LOGICAL   DOINTP, ALLFLG, VLA
      REAL   VISTMP(2,MAXCHA), TMPWT(MAXCHA), INTWTS(MAXCHA), TS, UTT,
     *   TEMP, POLAR(2)
      CHARACTER TC*1, HDATE*8
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DAPM.INC'
      SAVE LRAPP, LDAPP, FREQTO, LRAPPF, LDAPPF, EDELI, EUT, OLDSOU
      DATA LRAPP, LDAPP, FREQTO, EDELI, EUT /5 * -1000.0D0/
      DATA LIMMSG /10/
      DATA OBSPOS, POLAR /3 * 0.0D0, 2 * 0.0/
C-----------------------------------------------------------------------
      VLA = .TRUE.
C                                       Close down stuff
      IF (VISNUM.EQ.-1) THEN
         IF (USEAP) CALL QRLSE
         IRET = 0
         GO TO 999
         END IF
C                                       Initialize stuff
      NFREQ = CATBLK(KINAX+JLOCF)
      IF (VISNUM.EQ.1) THEN
C                                       Do we use PSAP FFT
         POWRTO = .FALSE.
         DO 10 GAMMA = 1,15
            IF ((2**GAMMA).EQ.NFREQ) THEN
               POWRTO = .TRUE.
               NXTTWO = NFREQ
               END IF
 10         CONTINUE
C                                       If not, is it a prime number
         IF (.NOT.POWRTO) CALL ISPRIM (NFREQ, PRIME, NXTTWO)
         USEAP = .FALSE.
         IF (POWRTO .OR. PRIME) USEAP = .TRUE.
C                                       Size of array for AP
         IF (USEAP) THEN
            NCMPLX = NXTTWO * 2 * 2
C                                       Init. AP
C                                       default size plenty
            IDUM1 = 5 * 1024
            CALL QINIT (IDUM1, IDUM2, APNUM)
            IF ((APNUM.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               IRET = 10
               MSGTXT = 'DSHIFT: FAILED TO GET ANY AP MEMORY'
               GO TO 990
               END IF
            APBEG = 0
            APTYPE = 2
            NROLL = -1
            NBYTES = 0
            END IF
C
         OLDSOU = -1
         REPDEL = 0.0
         REPMAX(1) = 10000.0
         REPMAX(2) = -10000.0
         COUNTD = 0
C                                       Determine year and ref.day
C                                       number of observation
         CALL GETTIM (DISKIN, CNOIN, WORK, CATBLK, IYEAR,
     *      IRDAY, IATUT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'YEAR AND DAY NUMBER'
            GO TO 990
            END IF
C                                       Set frequency info.
         NNIF = 1
C                                       Fill AN information
C                                       into common in DANS.INC
         SUBARR = 1
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, WORK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ANTENNA LOCATIONS'
            GO TO 990
            END IF
C                                       Correct station positions for
C                                       centre array offset if non-zero
C                                       go to left-hand system
         ARRLON = 0.0D0
         IF ((ABS(CNTRX).GT.1.D2) .AND. (ABS(CNTRY).GT.1.D2) .AND.
     *      (ABS(CNTRZ).GT.1.D2)) ARRLON = ATAN2 (CNTRY, CNTRX)
         DO 20 I = 1,MAXANT
            ANTX(I) = CNTRX + STNX(I)*COS(ARRLON) - STNY(I)*SIN(ARRLON)
            ANTY(I) = CNTRY + STNY(I)*COS(ARRLON) + STNX(I)*SIN(ARRLON)
            ANTY(I) = -ANTY(I)
            ANTZ(I) = CNTRZ + STNZ(I)
 20         CONTINUE
C                                       Precess ref coord to apparent
         IF ((RA.EQ.0 0D0) .AND. (DEC.EQ.0.0D0)) THEN
            RA = CATD(KDORA)
            DEC = CATD(KDODE)
            END IF
         CALL JULDAY (RDATE, JDA)
         CALL H2CHR (8, 1, CATH(KHDOB), HDATE)
         CALL JULDAY (HDATE, JD)
         IF (ABS(JD-JDA).GT.10.0D0) THEN
            WRITE (MSGTXT,1020) HDATE, RDATE
            CALL MSGWRT (7)
            END IF
         JD = JD + RPARMS(ILOCT+1)
         EQUIN = CATR(KREPO)
         IF (ABS(XPARM(2)-2.0).LT.0.5) THEN
            RA = RAPP0
            DEC = DAPP0
            END IF
         XRA = RA * DG2RAD
         XDEC = DEC * DG2RAD
         DIR = 1
         CALL JPRECS (JD, EQUIN, 0.1D0, DIR, .FALSE., OBSPOS, POLAR,
     *      XRA, XDEC, RAPP0, DAPP0)
C
C         CALL PRECES (JD, EQUIN, 0.1D0, XRA, XDEC, RAPP0, DAPP0, .TRUE.,
C     *      .FALSE., .FALSE., 0.0D0, 0.0D0, 0.0D0, .FALSE.)
         RAPP0 = RAPP0 * RAD2DG
         DAPP0 = DAPP0 * RAD2DG
         IF (ABS(XPARM(2)-1.0).LT.0.5) THEN
            LRAPP = RPARMS(1+ILOCU) - RA + RAPP0
            LDAPP = RPARMS(1+ILOCV) - DEC + DAPP0
         ELSE
            LRAPP = RAPP0
            LDAPP = DAPP0
            END IF
         END IF
C
      INCSU = INCS
      INCIFU = INCIF
      INCFU = INCF
      IF (WUVCMP) THEN
         INCSU = INCS * 3
         INCIFU = INCIF * 3
         INCFU = INCF * 3
         END IF
C                                       Determine time
      UT = RPARMS(ILOCT+1) - (IATUT/86400.D0)
C                                       Antenna numbers
      IF (ILOCB.GE.0) THEN
         IANT1 = RPARMS(ILOCB+1) / 256 + 0.1
         IANT2 = RPARMS(ILOCB+1) - 256 * IANT1 + 0.1
      ELSE
         IANT1 = RPARMS(ILOCA1+1) + 0.1
         IANT2 = RPARMS(ILOCA2+1) + 0.1
         END IF
      IF (IANT1.NE.IANT2) THEN
         WRITE (MSGTXT,1021) IANT1, IANT2
         IRET = 8
         GO TO 990
         END IF
C                                       Get basic freq. parms
      CALL GETFRQ (OLDSOU, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FREQ./VEL. PARMS'
         GO TO 990
         END IF
C                                       Determine reference velocity/fr
      IIYEAR = IYEAR
      NDAY = IRDAY + UT
      DTIME = UT - (NDAY-IRDAY)
      DXANT = ANTX(IANT1)
      DYANT = ANTY(IANT1)
      DZANT = ANTZ(IANT1)
C                                       Determine reference velocity/fr

      IF (FRFROM.LE.0.0D0) THEN
         IF (ABS(XPARM(2)-1.0).LT.0.5) THEN
            IF (ABS(XPARM(1)).LT.0.5) THEN
               LRAPP = RPARMS(1+ILOCU) - RA + RAPP0
               LDAPP = RPARMS(1+ILOCV) - DEC + DAPP0
               END IF
         ELSE
            LRAPP = RAPP0
            LDAPP = DAPP0
            END IF
         LRAPPF = LRAPP
         LDAPPF = LDAPP
         XRA = LRAPP * DG2RAD
         XDEC = LDAPP * DG2RAD
         CALL DOPLR (XRA, XDEC, IIYEAR, NDAY, DTIME, DXANT, DYANT,
     *      DZANT, VSUN, DOPVEL)
         VELTOT = CATD(KDARV) + DOPVEL
         IF (HELIO) VELTOT = VELTOT - VSUN
         IF (.NOT.RADIO) THEN
            FRFROM = RSTFRQ(1) / (1.D0 + VELTOT / VELITE)
         ELSE
            FRFROM = RSTFRQ(1) * (1.D0 - VELTOT / VELITE)
            END IF
         END IF
C                                       Determine current
      XRA = RPARMS(1+ILOCU) - RA + RAPP0
      XDEC = RPARMS(1+ILOCV) - DEC + DAPP0
      IF ((XRA.NE.LRAPP) .OR. (XDEC.NE.LDAPP) .OR. (FREQTO.LE.0.0)) THEN
         LRAPP = XRA
         LDAPP = XDEC
         XRA = LRAPP * DG2RAD
         XDEC = LDAPP * DG2RAD
         CALL DOPLR (XRA, XDEC, IIYEAR, NDAY, DTIME, DXANT, DYANT,
     *      DZANT, VSUN, DOPVEL)
         VELTOT = CATD(KDARV) + DOPVEL
         IF (HELIO) VELTOT = VELTOT - VSUN
         IF (.NOT.RADIO) THEN
            FREQTO = RSTFRQ(1) / (1.D0 + VELTOT / VELITE)
         ELSE
            FREQTO = RSTFRQ(1) * (1.D0 - VELTOT / VELITE)
            END IF
         END IF
C                                       Required pixel shift
      FINT = CFINC(1) * CSBAND(1)
      DELI = (FRFROM - FREQTO) / FINT
      REPDEL = REPDEL + DELI
      COUNTD = COUNTD + 1
      TEMP = DELI
      REPMAX(1) = MIN (REPMAX(1), TEMP)
      REPMAX(2) = MAX (REPMAX(2), TEMP)
C                                       If shift too large, flag
      IF (ABS(DELI).GT.XPARM(4)) THEN
         IRET = -1
         GO TO 999
         END IF
C                                       If shift too large, give warning
      IF (ABS(DELI).GT.XPARM(3)) THEN
         IF (((EDELI.NE.DELI) .OR. (EUT.NE.UT)) .AND.
     *      (CNTLSG.LE.LIMMSG)) THEN
            UTT = UT
            CALL TFDHMS (UTT, 1, TC, TIT, TS)
            WRITE (MSGTXT,1030) TIT, TS, DELI
            IF (MSGTXT(27:27).EQ.' ') MSGTXT(27:27) = '0'
            CALL MSGWRT (6)
            WRITE (MSGTXT,1031) LRAPPF, LDAPPF, LRAPP, LDAPP
            CALL MSGWRT (6)
            CNTLSG = CNTLSG + 1
         ELSE IF (CNTLSG.EQ.LIMMSG+1) THEN
            MSGTXT = 'Warning: further consecutive warnings suppressed'
            CALL MSGWRT (6)
            CNTLSG = CNTLSG + 1
            END IF
         CNTMSG = CNTMSG + 1
         EDELI = DELI
         EUT = UT
      ELSE
         CNTLSG = 0
         END IF
C                                       Loop and shift
      DO 100 IPOL = 1,NCOR
C                                       Copy data to temp array.
         DOINTP = .FALSE.
         INDEX = 1 + (IPOL-1) * INCSU
         DO 40 IFRQ = 1,NFREQ
            FINDEX = INDEX + (IFRQ-1) * INCFU
            VISTMP(1,IFRQ) = VIS(FINDEX)
            VISTMP(2,IFRQ) = VIS(FINDEX+1)
            TMPWT(IFRQ)    = VIS(FINDEX+2)
            IF (TMPWT(IFRQ).LE.0.0) DOINTP = .TRUE.
 40         CONTINUE
C                                       Deal with flagged spectral data
         ALLFLG = .FALSE.
         IF (DOINTP) CALL SPINTP (NFREQ, VISTMP, TMPWT, INTWTS, ALLFLG)
         IF (ALLFLG) THEN
            DO 50 IFRQ = 1,NFREQ
               FINDEX = INDEX + (IFRQ-1) * INCFU
               VIS(FINDEX+2) = -ABS(VIS(FINDEX+2))
 50            CONTINUE
C                                       Shift it!
         ELSE
            ISB = CSBAND(1)
            ISMTH = 0
            CALL ACSHFT (VISTMP, ISB, NFREQ, DELI, WORK, ISMTH)
C                                       Copy data back to vis array
            DO 60 IFRQ = 1,NFREQ
               FINDEX = INDEX + (IFRQ-1) * INCFU
               VIS(FINDEX) = VISTMP(1,IFRQ)
               VIS(FINDEX+1) = VISTMP(2,IFRQ)
               VIS(FINDEX+2) = TMPWT(IFRQ)
 60            CONTINUE
             END IF
 100      CONTINUE
      GO TO 999
C                                       Write Error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DSHIFT: ERROR',I4,' DETERMINING ',A)
 1020 FORMAT ('WARNING: HEADER DATE ',A,', AN FILE DATE ',A,' DIFFER')
 1021 FORMAT ('DSHIFT: BASELINE',I3,'-',I2,' NOT SINGLEDISH')
 1030 FORMAT ('WARNING: Time =',I4,'/',2(I2.2,':'),F4.1,
     *   ' channel shift =',F15.3)
 1031 FORMAT (6X,'Ref RA/Dec',2F10.5,' current',2F10.5)
      END
      SUBROUTINE SDVLHI
C-----------------------------------------------------------------------
C   SDVLHI copies and updates history file.  It also copies any tables
C   extension files.
C    Input from common:
C     DISOUT    I    Output file disk number
C     CNOOUT    I    Output file catalog slot number.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(5)*2,LINE*72
      INTEGER   IERR, LUN1, LUN2, NONOT
      REAL      BUFFH(1024)
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'SDVEL.INC'
      EQUIVALENCE (BUFFER(1025), BUFFH)
      DATA LUN1, LUN2 /28,29/
      DATA NONOT, NOTTYP /2, 'CH','FQ','  ','  ','  '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISOUT, CNOIN,
     *   CNOOUT, CATOUT, BUFF1, BUFFH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFFH,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL H2CHR (12, KHIMNO, CATOH(KHIMN), NAMOUT)
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, CATOUT(KIIMS), DISOUT, LUN2,
     *   BUFFH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Input flagging
      IF (FGVER.GT.0) THEN
         WRITE (LINE,2001) TSKNAM, FGVER
         CALL HIADD (LUN2, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Time range
      CALL HITIME (TIME1, TIME2, LUN2, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Excessive shift flagging
      IF (CNTFLG.GT.0) THEN
         WRITE (LINE,2002) TSKNAM, CNTFLG, XPARM(4)
         CALL HIADD (LUN2, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (CNTMSG.GT.0) THEN
         WRITE (LINE,2003) TSKNAM, CNTMSG, XPARM(3)
         CALL HIADD (LUN2, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      LINE = TSKNAM
      IF (XPARM(1).LE.0.0) THEN
         LINE(7:) = '/ One reference velocity at center coordinate'
      ELSE IF (XPARM(1).EQ.1.0) THEN
         LINE(7:) = '/ One reference velocity at first coordinate'
      ELSE IF (XPARM(1).EQ.2.0) THEN
         LINE(7:) = '/ New reference velocity at each off scan'
      ELSE IF (XPARM(1).EQ.3.0) THEN
         LINE(7:) = '/ New reference velocity at each scan'
      ELSE
         LINE(7:) = '/ REFERENCE VELOCITY SETTING NOT PROPERLY DEFINED'
         END IF
      CALL HIADD (LUN2, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFFH, IERR)
      IF (DOFLAG) THEN
         NONOT = NONOT + 1
         NOTTYP(NONOT) = 'FG'
         MSGTXT = 'Flag table applied - not copied to output'
         CALL MSGWRT (3)
         MSGTXT = 'If spectral flagging requested, spectra will have'
         CALL MSGWRT (3)
         MSGTXT = 'been interpolated before shifting'
         CALL MSGWRT (3)
         END IF
C
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISOUT, CNOIN,
     *   CNOOUT, CATOUT, BUFF1, BUFFH, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISOUT, CNOOUT, CATOUT, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDVLHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2001 FORMAT (A6,'/ Edited using FG table version',I3)
 2002 FORMAT (A6,'/ Flagged',I6,' shifts >',F8.2,' channels - ',
     *   'data deleted')
 2003 FORMAT (A6,'/ Found',I8,' shifts >',F8.2,' channels - ',
     *   'data shifted')
      END
      SUBROUTINE CVFLAG (RPARM, VISIN, DROP, IERR)
C-----------------------------------------------------------------------
C   Flags data specified in flagging table
C   Inputs:
C      RPARM(*)   R    Random parameter array
C      VISIN(3,*)   R    Visibility array
C   Inputs from include DSEL.INC:
C      CURSOU     I    Current source number
C      NUMFLG     I    Number of flagging entries.
C      TMFLST     R    Time of last visibility for which flagging
C                      was checked.
C      FLGSOU(*)  I    Source id numbers to flag, 0=all.
C      FLGANT(*)  I    Antenna numbers to flag, 0=all.
C      FLGBAS(*)  I    Baseline (A1*32768+A2) numbers to flag, 0=all.
C      FLGSUB(*)  I    Subarray numbers to flag, 0=all.
C      FLGFQD(*)  I    Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF(*)  I    First IF to flag.
C      FLGEIF(*)  I    Highest IF to flag.
C      FLGBCH(*)  I    First channel to flag.
C      FLGECH(*)  I    Highest channel to flag.
C      FLGPOL(4,*)L    Flags for the polarizations, should correspond
C                      to selected polarization types.
C   Output:
C      RPARM(*)   R    Random parameter array
C      VISIN(3,*)   R    Visibility array
C      DROP       L    True if data all flagged.
C      IERR       I    Return code, 0=OK, else CVNXFG error number.
C-----------------------------------------------------------------------
      INTEGER   IERR, IFLAG, KBASE, A1, A2, FLGA, SUBA, JIF, JCHAN,
     *   JPOLN, LIMF1, LIMF2, LIMC1, LIMC2, INDEX, STADD, IPOLPT
      LOGICAL   DROP, GOOD
      REAL      TIME, SUM, RPARM(*), VISIN(3,*)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'SDVEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      DROP = .FALSE.
C                                       Check if new time
      TIME = RPARM(1+ILOCT)
      IF (TMFLST.LT.TIME) CALL CVNXFG (TIME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check if there are current flags
      IF (NUMFLG.LE.0) GO TO 999
C                                       Loop thru flagging criteria
      IF (ILOCB.GE.0) THEN
         KBASE = RPARM(1+ILOCB) + 0.1
         A1 = KBASE / 256
         A2 = KBASE - 256 * A1
         SUBA = (RPARM(1+ILOCB) - KBASE) * 100.0 + 1.5
      ELSE
         A1 = RPARM(1+ILOCA1) + 0.1
         A2 = RPARM(1+ILOCA2) + 0.1
         SUBA = RPARM(1+ILOCSA) + 0.1
         END IF
      KBASE = 32768 * A1 + A2
      DO 500 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF (.NOT.TIMORD) THEN
            IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *         GO TO 500
            END IF
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.A1) .AND. (FLGA.NE.A2))
     *      GO TO 500
C                                       Check baseline
         IF ((FLGBAS(IFLAG).NE.0) .AND. (FLGBAS(IFLAG).NE.KBASE))
     *      GO TO 500
C                                       Check subarray
         IF ((FLGSUB(IFLAG).NE.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 500
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
         LIMC1 = FLGBCH(IFLAG)
         LIMC2 = FLGECH(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(FKCOR0) - 1
         DO 400 JPOLN = 1,FKNCOR
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
               STADD = (JPOLN-1) * FKNCS + 1
C                                       Loop over IF
               DO 300 JIF = LIMF1,LIMF2
                  INDEX = STADD + (JIF-1) * FKNCIF + (LIMC1-1) * FKNCF
                  IF (LIMC1.EQ.LIMC2) THEN
C                                       Single channel
                     VISIN(3,INDEX) = - ABS (VISIN(3,INDEX))
                  ELSE
C                                       Loop over channel
                     DO 200 JCHAN = LIMC1,LIMC2
C                                       Flag
                        VISIN(3,INDEX) = - ABS (VISIN(3,INDEX))
                        INDEX = INDEX + FKNCF
 200                    CONTINUE
                     END IF
 300              CONTINUE
               END IF
 400        CONTINUE
 500     CONTINUE
C                                       Check if data all bad
      GOOD = .FALSE.
C                                       Loop over polarizations
      DO 520 JPOLN = 1,FKNCOR
         INDEX = 1 + (JPOLN-1) * FKNCS
C                                       Multiple channels
         SUM = 0.0
         DO 510 JCHAN = 1,NUMFRQ
            SUM = SUM + MAX (0.0, VISIN(3,INDEX))
            INDEX = INDEX + FKNCF
 510        CONTINUE
         GOOD = GOOD .OR. (SUM.GT.0.0)
 520     CONTINUE
      DROP = .NOT.GOOD
C
 999  RETURN
      END
      SUBROUTINE CVNXFG (TIME, IERR)
C-----------------------------------------------------------------------
C   Updates flagging tables in common fron an FG table.
C   Inputs:
C      TIME         R    Current time (days) for flag entries
C   Inputs from common /CFMINF/(INCLUDEs C/DSEL.INC):
C      NUMFLG       I    number of current FLAG entries.
C      FGKOLS(MAXFGC) I    The column pointer array in order, SOURCE,
C                        SUBARRAY, FREQID, ANTS, TIMERANG, IFS, CHANS,
C                        PFLAGS, REASON
C      FGNUMV(MAXFGC) I    Element count for each column
C      IFGRNO       I    Current FLAG file record.
C   Output to common /CFMINF/:
C      NUMFLG     I    Number of flagging entries.
C      TMFLST     R    Time of last visibility for which flagging
C                      was checked.
C      FLGSOU(*)  I    Source id numbers to flag, 0=all.
C      FLGANT(*)  I    Antenna numbers to flag, 0=all.
C      FLGBAS(*)  I    Baseline (A1*32768+A2) numbers to flag, 0=all.
C      FLGSUB(*)  I    Subarray numbers to flag, 0=all.
C      FLGFQD(*)  I    Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF(*)  I    First IF to flag.
C      FLGEIF(*)  I    Highest IF to flag.
C      FLGBCH(*)  I    First channel to flag.
C      FLGECH(*)  I    Highest channel to flag.
C      FLGPOL(4,*)L    Flags for the polarizations, should correspond
C                      to selected polarization types.
C      FLGTST(*)  R    Start time of flag.
C      FLGTND(*)  R    End time of flag.
C   Output:
C      IERR       I    Return code, 0=OK, else TABIO error number.
C-----------------------------------------------------------------------
      REAL      TIME
      INTEGER   IERR
C
      INTEGER   NDROP, LIMIT, RECI(30), MXFLG, SOUKOL, SUBKOL, FRQKOL,
     *   ANTKOL, TIMKOL, IFKOL, CHKOL, POLKOL, REAKOL, A1, A2, IT, I4,
     *   NFGREC, I, LIMIT4
      REAL      RECORD(31)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'SDVEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (RECORD, RECI)
      EQUIVALENCE (FGKOLS(1), SOUKOL), (FGKOLS(2), SUBKOL),
     *   (FGKOLS(3), FRQKOL), (FGKOLS(4), ANTKOL), (FGKOLS(5),TIMKOL),
     *   (FGKOLS(6), IFKOL),  (FGKOLS(7), CHKOL), (FGKOLS(8), POLKOL),
     *   (FGKOLS(9), REAKOL)
      DATA I4 /4/
C-----------------------------------------------------------------------
      IERR = 0
      MXFLG = MAXFLG
      TMFLST = TIME
C                                       Check if any flags expired.
 10   NDROP = 0
C                                       Find highest number expired flag
      IF ((NUMFLG.GT.0) .AND. (TIMORD)) THEN
         DO 20 I = 1,NUMFLG
            IF (FLGTND(I).LT.TIME) NDROP = I
 20         CONTINUE
         END IF
C                                       Compress, dropping flag.
      IF (NDROP.GT.0) THEN
         IF (NDROP.LT.NUMFLG) THEN
            LIMIT = NDROP + 1
            DO 150 I = LIMIT,NUMFLG
               IT = I - 1
               FLGTST(IT) = FLGTST(I)
               FLGTND(IT) = FLGTND(I)
               FLGSOU(IT) = FLGSOU(I)
               FLGANT(IT) = FLGANT(I)
               FLGFQD(IT) = FLGFQD(I)
               FLGBAS(IT) = FLGBAS(I)
               FLGSUB(IT) = FLGSUB(I)
               FLGBIF(IT) = FLGBIF(I)
               FLGEIF(IT) = FLGEIF(I)
               FLGBCH(IT) = FLGBCH(I)
               FLGECH(IT) = FLGECH(I)
               FLGPOL(1,IT) = FLGPOL(1,I)
               FLGPOL(2,IT) = FLGPOL(2,I)
               FLGPOL(3,IT) = FLGPOL(3,I)
               FLGPOL(4,IT) = FLGPOL(4,I)
 150           CONTINUE
            END IF
         NUMFLG = NUMFLG - 1
         GO TO 10
         END IF
C                                       Find next valid flag.
      NFGREC = FGBUFF(5)
C                                       Loop through records
C                                       Check if list exhausted
      IF (IFGRNO.GT.NFGREC) GO TO 999
 310  LIMIT4 = IFGRNO
      DO 360 I = LIMIT4,NFGREC
         IFGRNO = I
         IERR = 1
C                                       Read record.
         CALL TABIO ('READ', 0, IFGRNO, RECORD, FGBUFF, IERR)
C                                       Check if flagged
         IF (IERR.LT.0) GO TO 360
C                                       Check error
         IF (IERR.GT.0) GO TO 999
C                                       Check time.
         IF (TIME.LT.RECORD(TIMKOL)) GO TO 999
         IF (TIME.LE.RECORD(TIMKOL+1)) GO TO 500
 360     CONTINUE
C                                       No flags - bail out.
      IERR = 0
      GO TO 999
C                                       Next entry
 500  NUMFLG = NUMFLG + 1
C                                       Check if too big
      IERR = 0
      IF (NUMFLG.GT.MXFLG) THEN
         IERR = 0
         WRITE (MSGTXT,1500) MXFLG
         CALL MSGWRT (8)
         IF (TIMORD) THEN
            MSGTXT = 'USE UVCOP TO APPLY THE FG TABLE'
         ELSE
            MSGTXT = 'SORT TO TIME ORDER TO APPLY THE FG TABLE'
            END IF
         GO TO 990
         END IF
C                                       Fill in tables
      FLGTST(NUMFLG) = RECORD(TIMKOL)
      FLGTND(NUMFLG) = RECORD(TIMKOL+1)
      FLGSOU(NUMFLG) = RECI(SOUKOL)
      FLGFQD(NUMFLG) = RECI(FRQKOL)
      A1 = MIN (RECI(ANTKOL), RECI(ANTKOL+1))
      A2 = MAX (RECI(ANTKOL), RECI(ANTKOL+1))
      IF (A1.LE.0) THEN
         FLGANT(NUMFLG) = A2
         FLGBAS(NUMFLG) = 0
      ELSE
         FLGANT(NUMFLG) = RECI(ANTKOL)
         FLGBAS(NUMFLG) = A1*32768 + A2
         END IF
      FLGSUB(NUMFLG) = RECI(SUBKOL)
      FLGBIF(NUMFLG) = RECI(IFKOL)
      FLGEIF(NUMFLG) = RECI(IFKOL+1)
      IF (FLGBIF(NUMFLG).LE.0) FLGBIF(NUMFLG) = 1
      IF (FLGEIF(NUMFLG).LE.0) THEN
         IF (JLOCIF.GT.0) FLGEIF(NUMFLG) = CATBLK (KINAX+JLOCIF)
         IF (JLOCIF.LE.0) FLGEIF(NUMFLG) = 1
         END IF
      FLGBCH(NUMFLG) = RECI(CHKOL)
      FLGECH(NUMFLG) = MIN (CATBLK(KINAX+JLOCF), RECI(CHKOL+1))
      IF (FLGBCH(NUMFLG).LE.0) FLGBCH(NUMFLG) = 1
      IF (FLGECH(NUMFLG).LE.0) FLGECH(NUMFLG) = CATBLK (KINAX+JLOCF)
      CALL LG2BIT (I4, FLGPOL(1,NUMFLG), RECI(POLKOL), -1)
C                                       Increment flag counter
      IFGRNO = IFGRNO + 1
C                                       Loop back for next
      IF (IFGRNO.LE.NFGREC) GO TO 310
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('TOO MANY FLAGS AT SAME TIME (>',I4,')')
      END
      DOUBLE PRECISION FUNCTION TAI (UTC)
C-----------------------------------------------------------------------
C  Increment to be applied to Coordinated Universal Time UTC to give
C  International Atomic Time TAI
C
C  Inputs:
C     UTC      D      UTC as a modified JD (JD-2400000.5)
C
C  Result:  TAI-UTC in seconds
C
C  Pre 1972 January 1 a fixed value of 10 sec is returned.
C
C     :-----------------------------------------:
C     :                                         :
C     :                IMPORTANT                :
C     :                                         :
C     :  This routine must be updated on each   :
C     :     occasion that a leap second is      :
C     :                announced                :
C     :                                         :
C     :  Latest leap second:  1990 January 1    :
C     :                                         :
C     :-----------------------------------------:
C
C  P.T.Wallace   Starlink   21 November 1989
C-----------------------------------------------------------------------
      DOUBLE PRECISION UTC
      DOUBLE PRECISION DT
C-----------------------------------------------------------------------
      DT = 10D0
C                                         1972 July 1
      IF (UTC.GE.41498D0) DT=11D0
C                                         1973 January 1
      IF (UTC.GE.41683D0) DT=12D0
C                                         1974 January 1
      IF (UTC.GE.42048D0) DT=13D0
C                                         1975 January 1
      IF (UTC.GE.42413D0) DT=14D0
C                                         1976 January 1
      IF (UTC.GE.42778D0) DT=15D0
C                                         1977 January 1
      IF (UTC.GE.43144D0) DT=16D0
C                                         1978 January 1
      IF (UTC.GE.43509D0) DT=17D0
C                                         1979 January 1
      IF (UTC.GE.43874D0) DT=18D0
C                                         1980 January 1
      IF (UTC.GE.44239D0) DT=19D0
C                                         1981 July 1
      IF (UTC.GE.44786D0) DT=20D0
C                                         1982 July 1
      IF (UTC.GE.45151D0) DT=21D0
C                                         1983 July 1
      IF (UTC.GE.45516D0) DT=22D0
C                                         1985 July 1
      IF (UTC.GE.46247D0) DT=23D0
C                                         1988 January 1
      IF (UTC.GE.47161D0) DT=24D0
C                                         1990 January 1
      IF (UTC.GE.47892D0) DT=25D0
C
      TAI=DT
      RETURN
      END
      SUBROUTINE GETFRQ (OLDSOU, IRET)
C-----------------------------------------------------------------------
C   Routine to get all freq, vel type parameters needed for the
C   shifting routine  This is a restricted version for single-dish, 1
C   IF, single-source data sets.
C   Inputs:
C      OLDSOU   I
C   Input in common:
C      CATBLK   I(256)   Catalog header - UVPGET called
C   Outputs in common:
C      REFFRQ   D(NIF)   Freq. (Hz) of ref. pixel - same one to which
C                        VEL refers.
C      RSTFRQ   D(NIF)   Rest freq. (Hz) of the transition in each IF
C      VEL      D(NIF)   Desired velocity of the reference pixel of each
C                        IF (m/s)
C      HELIO    L        .TRUE. if velocity reference frame is
C                        heliocentric/barycentric.
C      RADIO    L        .TRUE. if velocity follows radio definition.
C      SOUDUN   I(*)     > 0 if source already dealt with
C   Output:
C      IRET              I        Error: = 0 => all OK
C-----------------------------------------------------------------------
      INTEGER   OLDSOU, IRET
C
      INTEGER   ALTAX, I, ITRIM
      REAL      RTEMP
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCVL.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Set base ref. freq.
      IF (OLDSOU.GT.0) GO TO 999
      OLDSOU = 1
C                                       Get frequency info.
      CFOFF(1) = 0.0D0
      CSBAND(1) = 1
      CFINC(1) = CATR(KRCIC+JLOCF)
C                                       Extract information from
C                                       alternate reference pixels
      ALTAX = CATBLK (KIALT)
C                                       Check if vel info available
      HELIO = .FALSE.
      RADIO = .FALSE.
      IF (ALTAX.GT.256) THEN
         RADIO = .TRUE.
         ALTAX = ALTAX - 256
         END IF
      IF (ALTAX.EQ.2) HELIO = .TRUE.
C                                       Inform user
      IF (HELIO) THEN
         MSGTXT = 'Velocity type is heliocentric; definition is'
      ELSE
         MSGTXT = 'Velocity type is LSR; definition is'
         END IF
      I = ITRIM (MSGTXT) + 2
      IF (RADIO) THEN
         MSGTXT(I:) = 'Radio'
      ELSE
         MSGTXT(I:) = 'Optical'
         END IF
      CALL MSGWRT (4)
C                                       Determine freq. of ref pixel,
      REFFRQ(1) = CATD(KDCRV+JLOCF) + CFOFF(1)
C                                       For single source files, there
C                                       can be only one rest frequency
C                                       for all IFs (nowhere to store
C                                       more than one).
C
C                                       Determine rest freq. of line
C                                       from user or header and
C                                       replicate to all IFs
      RSTFRQ(1) = CATD(KDRST)
C                                       Inform user
      RTEMP = RSTFRQ(1) / 1.0E6
      IF (RSTFRQ(1).LE.0.1D0) THEN
         WRITE (MSGTXT,1000) RTEMP
         IRET = 1
         GO TO 990
      ELSE
         WRITE (MSGTXT,1001) RTEMP
         CALL MSGWRT (4)
         END IF
C                                       Find from header, a velocity
C                                       (REFVEL) and the pixel to which
C                                       it refers (ALTRFP)
      RTEMP = CATD(KDARV) / 1.0E3
      WRITE (MSGTXT,1005) RTEMP, CATR(KRARP)
      CALL MSGWRT (4)
      VEL(1) = CATD(KDARV)
      GO TO 999
C                                       Write error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Rest frequency = ',F16.6,' MHz: ILLEGAL')
 1001 FORMAT ('Rest frequency = ',F16.6,' MHz')
 1005 FORMAT ('Velocity is',F11.3,' km/s at pixel',F8.2)
      END
