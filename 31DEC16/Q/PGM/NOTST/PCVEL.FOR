LOCAL INCLUDE 'PCVEL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
C                                      Character declerations
      CHARACTER  NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, SHFSRC(30)*16,
     *   PRESOU(300)*16, FOTYPE*2, OPTYPE*4, INFILE*48
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, SHFSRC, PRESOU,
     *   FOTYPE, OPTYPE, INFILE
C                                      Input parameters
      HOLLERITH  XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2),
     *   XSOUR(4,30), XOPTYP, XINFIL(12)
      REAL       XSIN, XDISIN, XQUAL, XSOUT, XDSOUT, XTIME(8), XBAND,
     *   XFREQ, XFQID, XSUBA, XFLAG, XDOBND, XBPVER, XGUSE, APARM(10),
     *   XBADD(10), PTIME(2)
      INTEGER   SEQIN, CNOIN, DISKIN, SEQOUT, CNOOUT, DISOUT, GUSE,
     *   UVBFSZ, ANTPRM, VTYPE
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDSOUT, XSOUR, XQUAL, XTIME, XBAND, XFREQ, XFQID, XSUBA,
     *   XFLAG, XDOBND, XBPVER, XGUSE, APARM, XOPTYP, XINFIL, XBADD
C                                      Buffers
      REAL      BUFFER(UVBFSS), BUFF1(512), BUFFC(3*MAXCIF),
     *   BUFF2(UVBFSS)
      COMMON /CMBUFF/ BUFFER, BUFFC, BUFF2, BUFF1
C                                      File Control
      INTEGER   ICHLUN, ISLUN, NDECOM, DECOM(2,MAXIF*4), LLOCWT, LLOCSC
      COMMON /FILCON/ ICHLUN, ISLUN, NDECOM, DECOM, LLOCWT, LLOCSC
C                                      Misc. parameters
      INTEGER   CVLSOU, OLDSOU, NFREQ, NCSOU, CSUWAN(XSTBSZ),
     *   CSUWTB(XSTBSZ), KSEQO, NUMFRQ, INCFU, INCIFU, INCSU, NSOURC,
     *   FONUMR, FONUMA, FONIF, FONANT, FOVER, NWARN
      LOGICAL   SINGLE, DOCSOU, SAMNO, WUVCMP, SUPRMS, EVN
C                                       planet velocities
      DOUBLE PRECISION PVTIME(5000)
      REAL      PVVEL(5000)
      INTEGER   NPVVEL
      COMMON /OPARM/ PVTIME, PVVEL, NPVVEL, KSEQO, NUMFRQ, CVLSOU,
     *   OLDSOU, NFREQ, NCSOU, CSUWAN, CSUWTB, INCFU, INCIFU, INCSU,
     *   NSOURC, SINGLE, DOCSOU, SAMNO, WUVCMP, SUPRMS, EVN, FONUMR,
     *   FONUMA, FONIF, FONANT, FOVER, ANTPRM, PTIME, SEQIN, CNOIN,
     *   DISKIN, SEQOUT, CNOOUT, DISOUT, GUSE, UVBFSZ, VTYPE, NWARN
C                                       NX stuff
      INTEGER   MAXNX
      PARAMETER (MAXNX = 10000)
      INTEGER   NXVER, NXLUN, NXVISN(2,MAXNX), NUMNX, NXSOU(MAXNX)
      REAL      NXTIM(2,MAXNX)
      LOGICAL   INDXT
      COMMON /PCVELNX/ NXVER, NXLUN, NXVISN, NXSOU, INDXT, NUMNX, NXTIM
C                                       FQ stuff
      DOUBLE PRECISION FOFF(MAXIF)
      REAL      FINC(MAXIF)
      INTEGER   ISBAND(MAXIF), OLDFQI, CURFQI
      LOGICAL   FQEXIS, MULTFQ
      COMMON /FQSTUF/ FOFF, FINC, ISBAND, OLDFQI, CURFQI, FQEXIS, MULTFQ
C                                       O/P catalogue
      INTEGER   CATOUT(256)
      HOLLERITH CATOH(256)
      DOUBLE PRECISION CATOD(128)
      REAL      CATOR(256)
      COMMON /OUTHDR/ CATOUT
      EQUIVALENCE (CATOUT, CATOH, CATOR, CATOD)
C                                       Flag table info
      INTEGER FKNCOR, FKNCF, FKNCIF, FKNCS, FKCOR0
C                                       FLAG table info
      COMMON /PCVELFG/ FKNCOR, FKNCF, FKNCIF, FKNCS, FKCOR0
LOCAL END
      PROGRAM PCVEL
C-----------------------------------------------------------------------
C! PCVEL shifts spectral line data to a specified velocity wrt planet
C# Calibration Spectral UV VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 2013, 2015
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
C   PCVEL will shift spectral line data to a specified velocity.  The
C   program should be used only on uncalibrated data - since if your
C   data needs shifting it is pointless to have calibrated it. The
C   program is capable of applying the bandpass correction but will
C   refuse to do any  calibration.
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
C      SOURCES....Source list.
C      TIMERANG...Time range of the data to be shifted
C      SUBARRAY...Subarray number to shift. 0=>all.
C      FLAGVER....Specifies the version of the flagging table.
C      DOBAND.....If true correct data for bandpass
C      BPVER......The BP table to apply
C      BADDISK....Disks to avoid for scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, FODATA(2), NWORDS, I, IWORDS
      REAL      FRDATA(2)
      LONGINT   PFODAT
      EQUIVALENCE (FODATA, FRDATA)
      INCLUDE 'PCVEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGM /'PCVEL'/
C-----------------------------------------------------------------------
C                                       Get input parameters
C                                       and open O/P file.
      CALL PCVELI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       allocate freq offset table
      IF (FONUMR.LE.0) THEN
         MSGTXT = 'UNABLE TO FIND FO/CL/NX TABLE'
         CALL MSGWRT (8)
         MSGTXT = 'FREQUENCY OFFSETS USED WILL NOT BE RECORDED'
         CALL MSGWRT (8)
      ELSE
         IWORDS = (5 + 3 * FONIF)
         NWORDS = (FONUMR * IWORDS - 1) / 1024 + 5
         CALL ZMEMRY ('GET ', PRGM, NWORDS, FODATA, PFODAT, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET MEMORY FOR FREQ OFFSETS'
            CALL MSGWRT (8)
            MSGTXT = 'FREQUENCY OFFSETS WILL NOT BE READ OR RECORDED'
            CALL MSGWRT (8)
            FONUMR = 0
         ELSE
            CALL FILL (1024*NWORDS, 0, FODATA(1+PFODAT))
            END IF
         END IF
C                                       Read, shift and write
      CALL CVSHFT (IWORDS, FODATA(1+PFODAT), FRDATA(1+PFODAT), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Update history
      CALL PCVELH (IWORDS, FODATA(1+PFODAT), FRDATA(1+PFODAT))
      IF (FONUMR.GT.0) CALL ZMEMRY ('FREE', PRGM, NWORDS, FODATA,
     *   PFODAT, I)
      IF (NWARN.GT.0) THEN
         WRITE (MSGTXT,1000) NWARN
         CALL MSGWRT (8)
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT (I10,' warnings were issued, first 100 printed')
      END
      SUBROUTINE PCVELI (PRGN, IRET)
C-----------------------------------------------------------------------
C   PCVELI gets the input parms for PCVEL, finds the input file, opens
C   the output file and performs some checks on the validity of the
C   PCVEL operation.
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
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH CATH(256)
      CHARACTER STAT*4, PRGN*6, VELTYP*8, VELDEF*8, UTYPE*2,
     *   COLCL(2)*24
      INTEGER  JERR, NUMHIS, NPARM, IROUND, IERR, I, INOGRP, IDUM2, NIF,
     *   SUKOLS(MAXSUC), SUNUMV(MAXSUC), CLKEY(2,2), MAXSOU, SOUCUR,
     *   LUNFQ, FQVER, CATSAV(256), KEY(2,2), KEYSUB(2,2), NUMFQE,
     *   SUFQID, NREAD, TVER, NKEY, KOLSCL(2), IDUM4
      REAL      FKEY(2,2), RDUM
      LOGICAL   T, F, TABLE, EXIST, FITASC, MATCH
      INCLUDE 'PCVEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA T, F /.TRUE.,.FALSE./
      DATA KEY  /5,0,1,0/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA COLCL /'TIME', 'ANTENNA NO.'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      NUMHIS = 0
      OLDFQI = -1
      MULTFQ = .FALSE.
      ISVLBA = .FALSE.
      EVN = .FALSE.
      NWARN = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 183
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      IF (OPTYPE.EQ.' ') THEN
         VTYPE = 1
      ELSE IF (OPTYPE.EQ.'GEOC') THEN
         VTYPE = 2
      ELSE IF (OPTYPE.EQ.'LSR') THEN
         VTYPE = 3
      ELSE IF ((OPTYPE.EQ.'HELI') .OR. (OPTYPE.EQ.'SUN')) THEN
         VTYPE = 4
      ELSE
         MSGTXT = 'OPTYPE ' // OPTYPE // ' NOT RECOGNIZED'
         GO TO 990
         END IF
C                                       Obtain disc parms
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISOUT = IROUND (XDSOUT)
      ANTPRM = IROUND (APARM(10))
C                                       Other parms
      SUPRMS = APARM(8).GT.0
      GUSE = IROUND (XGUSE)
      IF (GUSE.LE.0) GUSE = 1
C                                       Timerange
      IF ((XTIME(1)+XTIME(2)+XTIME(3)+XTIME(4)) .EQ.0.0)
     *   XTIME(1)=-1.0E6
      IF ((XTIME(5)+XTIME(6)+XTIME(7)+XTIME(8)) .EQ.0.0)
     *   XTIME(5)=1.0E6
C                                       Set time range.
      PTIME(1) = XTIME(1) + XTIME(2) / 24. + XTIME(3) / (24. * 60.) +
     *   XTIME(4) / (24. * 60. * 60.)
      PTIME(2) = XTIME(5) + XTIME(6) / 24. + XTIME(7) / (24. * 60.) +
     *   XTIME(8) / (24. * 60. * 60.)

C                                       Get CATBLK from old file.
      CNOIN = 1
C                                       Check file exists
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       If it does extract the
C                                       CATBLK
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      CALL H2CHR (8, 1, CATH(KHOBJ), PRESOU(1))
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Compressed data?
      WUVCMP = F
      IF (CATBLK(KINAX).EQ.1) WUVCMP = T
C                                       BADDISK
      DO 10 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 10      CONTINUE
C                                       Put selection criteria into
C                                       correct common.
      DO 30 I = 1, 30
         CALL H2CHR (16, 1, XSOUR(1,I), SHFSRC(I))
 30      CONTINUE
      SELQUA = IROUND (XQUAL)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      FGVER = IROUND (XFLAG)
      DOFLAG = FGVER.GE.0
C                                       Set LUN's
C                                       UV LUN
      IULUN = 26
C                                       SU table LUN
      ISLUN = 27
C                                       FG table LUN
      IFLUN = 30
C                                       BP table LUN
      LUNSBP = 43
      IPLUN = 42
C                                       CH table LUN
      ICHLUN = 44
C                                       NX table LUN
      NXLUN = 46
C                                       Subarray to shift
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUNFQ = 45
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUNFQ, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1120)
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                       Check if single source file
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, ISLUN, BUFF1, TABLE, EXIST,
     *   FITASC, IERR)
      SINGLE = (.NOT.EXIST) .OR. (.NOT.TABLE) .OR. (ILOCSU.LT.0)
C                                       If single give warning message.
      IF (SINGLE) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (6)
         WRITE (MSGTXT,1020)
         CALL MSGWRT (6)
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
C                                       Determine if this is a
C                                       dataset with multiple FQ
C                                       entries.
      CALL ISTAB ('CH', DISKIN, CNOIN, 1, LUNFQ, FQBUFF, TABLE,
     *   EXIST, FITASC, IRET)
      IF (EXIST) THEN
         MULTFQ = .FALSE.
         FQEXIS = .TRUE.
      ELSE
         CALL ISTAB ('FQ', DISKIN, CNOIN, 1, LUNFQ, FQBUFF,
     *      TABLE, FQEXIS, FITASC, IRET)
         IF (FQEXIS) THEN
            FQVER = 1
            CALL FQINI ('READ', FQBUFF, DISKIN, CNOIN, FQVER,
     *         CATBLK, LUNFQ, IFQRNO, FQKOLS, FQNUMV, NIF, IRET)
            IF (IRET.NE.0) GO TO 999
            NUMFQE = FQBUFF(5)
            IF (NUMFQE.GT.1) MULTFQ = .TRUE.
C                                       Close FQ table
            CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, JERR)
            END IF
         END IF
      FQEXIS = (FQEXIS) .AND. (ILOCFQ.GE.0)
C                                       Get pointers from CATBLK
C                                       for the input/output file
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      CCNO = 1
      FRW(2) = 3
      CALL UVCREA (DISOUT, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
      CALL COPY (256, CATBLK, CATOUT)
      CALL COPY (256, CATSAV, CATBLK)
C                                       copy keywords
      CALL KEYCOP (DISKIN, CNOIN, DISOUT, CCNO, IRET)
C                                       Set output CNO
      CNOOUT = CCNO
C                                       Fill AN information
C                                       into common in D/CANS.INC
      CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1070) IRET
         GO TO 990
         END IF
C                                       VLBA ?
      ISVLBA = ANAME(1:4).EQ.'VLBA'
      EVN  = ANAME(1:4).EQ.'EVN '
      IF (ISVLBA) THEN
         MSGTXT = 'Array name in AN table is VLBA'
         CALL MSGWRT (4)
         MSGTXT = 'Will assume this is data from the VLBA correlator'
         CALL MSGWRT (4)
         MSGTXT = 'and that it is all fringe-rotated to Earth Center'
         CALL MSGWRT (4)
         MSGTXT = 'If incorrect, abort and change array name keyword'
         CALL MSGWRT (4)
         END IF
C
      NFREQ = CATBLK (KINAX + JLOCF)
      UVBFSZ = UVBFSS * 2
C                                       Open source table
      IF (.NOT.SINGLE)
     *   CALL SOUINI ('READ', BUFF1, DISKIN, CNOIN, 1, CATBLK, ISLUN,
     *      INOGRP, VELTYP, VELDEF, SUFQID, SOUCUR, SUKOLS, SUNUMV,
     *      IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1080) IRET
         GO TO 990
         END IF
      MAXSOU = BUFF1(5)
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
         CALL FGREFM (DISKIN, CNOIN, FGVER, CATBLK, IFLUN, IRET)
         CALL FLGINI ('READ', FGBUFF, DISKIN, CNOIN, FGVER, CATBLK,
     *      IFLUN, IFGRNO, FGKOLS, FGNUMV, IRET)
         MSGSUP = 0
         IF (IRET.NE.0) THEN
            DOFLAG = F
C                                       Sort to time order.
         ELSE IF (FGBUFF(43).NE.KEY(1,1)) THEN
            CALL TABIO ('CLOS', 0, NREAD, BUFFER, FGBUFF, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL TABSRT (DISKIN, CNOIN, 'FG', FGVER, FGVER, KEY, KEYSUB,
     *         FKEY, FGBUFF, CATBLK, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Re initialize.
            CALL FLGINI ('READ', FGBUFF, DISKIN, CNOIN, FGVER, CATBLK,
     *         IFLUN, IFGRNO, FGKOLS, FGNUMV, IRET)
            END IF
         END IF
      IF (DOFLAG) THEN
         WRITE (MSGTXT,1035) FGVER
      ELSE
         MSGTXT = 'Doing no flagging on the UV data'
         END IF
      CALL MSGWRT (3)
C                                       Sort CL/FO table if necessary
      FOTYPE = 'FO'
      CALL FNDEXT (FOTYPE, CATBLK, I)
      IF (I.LE.0) THEN
         FOTYPE = 'CL'
         CALL FNDEXT (FOTYPE, CATBLK, I)
         IF (I.LE.0) FOTYPE = ' '
         END IF
      ICXLUN = 49
      FONUMR = 0
      FONIF = 0
      FONANT = 0
      IF (FOTYPE.NE.' ') THEN
         IF (FOTYPE.EQ.'CL') THEN
            CALL CALINI ('READ', XLBUFF, DISKIN, CNOIN, GUSE, CATBLK,
     *         ICXLUN, IXLRNO, XLKOLS, XLNUMV, FONANT, IDUM2, FONIF,
     *         IDUM4, RDUM, IRET)
         ELSE
            CALL FOINI ('READ', XLBUFF, DISKIN, CNOIN, GUSE, CATBLK,
     *         ICXLUN, IXLRNO, XLKOLS, XLNUMV, FONANT, FONIF, IRET)
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET, FOTYPE
            GO TO 990
            END IF
         FONUMR = XLBUFF(5)
         NKEY = 2
         CALL FNDCOL (NKEY, COLCL, 24, .TRUE., XLBUFF, KOLSCL, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) THEN
            WRITE (MSGTXT,1110) IRET, FOTYPE
            GO TO 990
            END IF
C                                       Close table
         CALL TABIO ('CLOS', 0, IXLRNO, XLBUFF, XLBUFF, IRET)
C                                       force sort in any case
         CLKEY(1,1) = KOLSCL(1)
         CLKEY(2,1) = 0
         CLKEY(1,2) = KOLSCL(2)
         CLKEY(2,2) = 0
C                                       Sort
         CALL TABSRT (DISKIN, CNOIN, FOTYPE, GUSE, GUSE, CLKEY, KEYSUB,
     *      FKEY, XLBUFF, CATBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1090) IRET, FOTYPE
            GO TO 990
            END IF
         END IF
C                                       *** Need to initialize some
C                                       remaining variables in DSEL.INC
C                                       so that std. BP routines can
C                                       be used even though PCVEL does
C                                       not call UVGET. UVGET('INIT')
C                                       functions are performed by
C                                       PCVELI due to mulit-source
C                                       output format written by PCVEL.
C
C                                       First set freq. selection
      BCHAN = 1
      ECHAN = NFREQ
      BIF = 1
      EIF = 1
      IF (JLOCIF.GE.0) EIF = CATBLK(KINAX+JLOCIF)
      IF (FONIF.LE.0) FONIF = EIF
      IF (FONANT.LE.0) FONANT = NSTNS
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
C                                       Init. BP file; may call TABSRT
      IF (DOBAND.GT.0) THEN
         CALL BPASET (IRET)
         IF (IRET.NE.0) GO TO 999
         CALL COPY (256, CATUV, CATBLK)
         END IF
C                                       Read NX table and set up
C                                       index information
      CALL NXSET (IRET)
      IF ((FONUMR.LE.0) .AND. (INDXT)) FONUMR = NUMNX * FONANT * 4
C                                       get planet extra velocities
      CALL PLANET (IRET)
      GO TO 999
C                                       Write the error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCVELI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('WARNING: PCVEL may produce incorrect answers on files',
     *   ' that')
 1020 FORMAT ('have, at any time, been SPLIT; see HELP ')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('PCVELI: using flag table version',I5,' on the data')
 1040 FORMAT ('PCVELI: ERROR',I3,' COPYING CATBLK')
 1050 FORMAT ('PCVELI: ERROR',I3,' CREATING OUTPUT FILE')
 1070 FORMAT ('PCVELI: ERROR',I3,' OBTAINING ANTENNA INFORMATION')
 1080 FORMAT ('PCVELI: ERROR',I3,' OPENING SU TABLE')
 1090 FORMAT ('PCVELI: ERROR',I3,' SORTING ',A,' TABLE')
 1100 FORMAT ('PCVELI: ERROR',I3,' OPENING ',A,' TABLE')
 1110 FORMAT ('PCVELI: ERROR',I3,' FINDING COLUMNS IN ',A,' TABLE')
 1120 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE PLANET (IRET)
C-----------------------------------------------------------------------
C   PLANET reads the input text file converting times and saving
C   time/velocity data in large array
C   Outputs:
C      IRET   I   error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   TLUN, TIND, JJ, J, JTRIM, KBP, KBPLIM
      CHARACTER INLINE*80
      DOUBLE PRECISION X, TIME
      INCLUDE 'PCVEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       open text file
      TLUN = 3
      CALL ZTXOPN ('READ', TLUN, TIND, INFILE, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT TEXT FILE'
         TIND = 0
         GO TO 990
         END IF
      NPVVEL = 0
C                                       read loop
 20   CALL ZTXIO ('READ', TLUN, TIND, INLINE, IRET)
      IF (IRET.EQ.2) THEN
         CALL ZTXCLS (TLUN, TIND, IRET)
         TIND = 0
      ELSE IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING INPUT TEXT FILE'
         GO TO 990
C                                       parse data
      ELSE
         J = JTRIM (INLINE)
         IF ((J.EQ.0) .OR. (INLINE(:1).EQ.';') .OR. (INLINE(:1).EQ.'#'))
     *      GO TO 20
         JJ = J
         J = INDEX (INLINE, ':')
C                                       time sexagesimal
         IF (J.GT.0) THEN
            KBP = 1
            KBPLIM = J-1
            CALL GETNUM (INLINE, KBPLIM, KBP, X)
            IF (X.EQ.DBLANK) GO TO 900
            TIME = X / 24.0D0
            KBP = J + 1
            J = INDEX (INLINE(KBP:), ':')
            KBPLIM = KBP + J - 1
            CALL GETNUM (INLINE, KBPLIM, KBP, X)
            IF (X.EQ.DBLANK) GO TO 900
            TIME = TIME + X / (24.0D0 * 60.0D0)
            KBP = KBPLIM + 2
            KBPLIM = JJ
            CALL GETNUM (INLINE, KBPLIM, KBP, X)
            IF (X.EQ.DBLANK) GO TO 900
            TIME = TIME + X / (24.0D0 * 60.0D0 * 60.0D0)
C                                       time decimal days
         ELSE
            KBP = 1
            KBPLIM = JJ
            CALL GETNUM (INLINE, KBPLIM, KBP, X)
            IF (X.EQ.DBLANK) GO TO 900
            TIME =  X
            END IF
         CALL GETNUM (INLINE, KBPLIM, KBP, X)
         IF (X.EQ.DBLANK) GO TO 900
         NPVVEL = NPVVEL + 1
         PVTIME(NPVVEL) = TIME
         PVVEL(NPVVEL) = X * 1.0D3
         IF ((NPVVEL.GT.1) .AND. (PVTIME(NPVVEL).LE.PVTIME(NPVVEL-1)))
     *      THEN
            WRITE (MSGTXT,1010) TIME, PVTIME(NPVVEL-1), NPVVEL
            IRET = 8
            GO TO 990
            END IF
         GO TO 20
         END IF
C
      GO TO 999
C                                       bad format
 900  MSGTXT = 'BAD PARSING RESULT ON:'
      CALL MSGWRT (8)
      MSGTXT = INLINE
      IRET = 10
C
 990  CALL MSGWRT (8)
      IF (TIND.GT.0) CALL ZTXCLS (TLUN, TIND, KBP)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PLANET: ERROR',I5,' ON ',A)
 1010 FORMAT ('PLANET: TIME',F10.7,' LESS THAN',F10.7,' AT',I5)
      END
      SUBROUTINE CVSHFT (IWORDS, FODATI, FODATR, IRET)
C-----------------------------------------------------------------------
C   CVSHFT reads the UV data, shifts it to the new velocity/frequency
C   and writes it to the O/P file.
C   Outputs:
C      IRET   I   Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   IWORDS, FODATI(IWORDS,*), IRET
      REAL      FODATR(IWORDS,*)
C
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH CATH(256)
      CHARACTER NAME*48, BNDCOD(MAXIF)*8
      INTEGER   VISOFF, JBUFSZ, LUN, FIND, BIND, LENBU, INIO, NIO,
     *   LRECO, NNIF, NOPOL, IROUND, NPERC, BO, I, XCOUNT, NCORI,
     *   CVER, ONXRNO, JNXRNO, SBCHAN, SECHAN, J, LDATA, KBIND, MXANT,
     *   TIT1(4), TIT2(4), IAVG, JERR, ISUB, IBIND, NCOPY, IA1, IA2,
     *   VISNUM, COUNTD(MAXANT,MAXIF), FQID, RNXRET
      LOGICAL   T, F, DOSHFT, DROP
      REAL      VISIN(3*MAXCIF), WORK(3*MAXCIF), TIME, RPARM(20),
     *   REPDEL(MAXANT,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'PCVEL.INC'
      INCLUDE 'INCS:DCVL.INC'
      LOGICAL   WASHIF(MAXNX)
      EQUIVALENCE (CATBLK, CATH)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, BO /40,1/
      DATA MXANT /MAXANT/
C-----------------------------------------------------------------------
C                                       Set lengths of input axes.
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      NOPOL = CATBLK(KINAX+JLOCS)
      NUMFRQ = CATBLK(KINAX+JLOCF)
      CVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, CVER, CATBLK, ICHLUN,
     *   CNNIF, CFOFF, CSBAND, CFINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       fill offset table
      IF (FONUMR.GT.0) THEN
         CALL FOFILL (IWORDS, FODATI, FODATR, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
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
         CALL CMPARM (1, NUMIF, 1, NUMFRQ, SBCHAN, SECHAN, NDECOM,
     *      DECOM)
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
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      CALL UVINIT ('READ', IULUN, IUFIND, NVIS, VISOFF, LREC, LENBU,
     *   JBUFSZ, BUFF2, BO, IBIND, IRET)
C                                       Open output file.
      CALL ZPHFIL ('UV', DISOUT, CNOOUT, 1, NAME, IRET)
      CALL ZOPEN (LUN, FIND, DISOUT, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Init output vis file for write
      CALL UVINIT ('WRIT', LUN, FIND, CATBLK(KIGCN), VISOFF,
     *   LRECO, LENBU, JBUFSZ, BUFFER, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IF (WUVCMP) KBIND = BIND
      CALL RNXGET (DISKIN, CNOIN, CATBLK)
      CALL RNXINI (DISOUT, CNOOUT, CATOUT, RNXRET)
      CALL RFILL (MXANT*MAXIF, 0.0, REPDEL)
      CALL FILL (MXANT*MAXIF, 0, COUNTD)
C                                       Fill in source numbers
C                                       to be shifted
      CALL CVSOUF (IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1900) IRET
         GO TO 990
         END IF
C                                       Do the shift
      XCOUNT = 0
      NPERC = 10
      SAMNO = .FALSE.
C                                       Loop over visibilities
      ONXRNO = 0
      JNXRNO = 1
      VISNUM = 1
      DO 100 I = 1,NVIS
C                                       Read input data
         CALL UVDISK ('READ', IULUN, IUFIND, BUFF2, INIO, IBIND, IRET)
         IF (IRET.EQ.4) GO TO 110
         IF (IRET.NE.0) GO TO 999
         CALL RCOPY (NRPARM, BUFF2(IBIND), RPARM)
         IF (WUVCMP) THEN
            CALL ZUVXPN (NCORI, BUFF2(IBIND+NRPARM),
     *         BUFF2(IBIND+LLOCWT), VISIN)
         ELSE
            CALL RCOPY (NCOPY, BUFF2(IBIND+NRPARM), VISIN)
            END IF
C                                       Scan boundaries
         IF (INDXT) THEN
            IF (I.GT.NXVISN(2,JNXRNO)) JNXRNO = JNXRNO + 1
            END IF
C                                       Check timerange
         TIME = RPARM(ILOCT+1)
         IF ((TIME.LT.PTIME(1)) .OR. (TIME.GT.PTIME(2))) GO TO 100
C                                       Check FQID
         IF (ILOCFQ.GE.0) THEN
            FQID = RPARM(ILOCFQ+1) + 0.01
            IF (FQID.NE.FRQSEL) GO TO 100
            END IF
C                                       check subarray
         IF (SUBARR.GT.0) THEN
            IF (ILOCB.GE.0) THEN
               ISUB = RPARM(ILOCB+1) + 0.01
               ISUB = 1.5 + 100.0 * (RPARM(ILOCB+1) - ISUB)
            ELSE
               ISUB = RPARM(ILOCSA+1) + 0.01
               END IF
            IF (ISUB.NE.SUBARR) GO TO 100
            END IF
C                                       Do we shift this source
C                                       or copy it as is.
         IF (SINGLE) THEN
           CVLSOU = 1
           DOSHFT = .TRUE.
           WASHIF(JNXRNO) = .TRUE.
         ELSE
           CVLSOU = IROUND (RPARM(ILOCSU+1))
           WASHIF(JNXRNO) = DOSHFT
           DOSHFT = .FALSE.
           DO 46 J = 1,NCSOU
             IF (CVLSOU.EQ.CSUWAN(J)) DOSHFT = .TRUE.
 46          CONTINUE
           END IF
C                                       Flagging ?
         IF (DOFLAG) THEN
            CALL CVFLAG (RPARM, VISIN, DROP, IRET)
C                                       Remove fully flagged samples
            IF (DROP) GO TO 100
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Bandpass correction
         IF (DOBAND.GT.0) THEN
            IF (ILOCB.GE.0) THEN
               IA1 = RPARM(ILOCB+1) / 256 + 0.1
               IA2 = RPARM(ILOCB+1) - 256 * IA1 + 0.1
            ELSE
               IA1 = RPARM(ILOCA1+1) + 0.1
               IA2 = RPARM(ILOCA2+1) + 0.1
               END IF
C                                       *** Need to set some DSEL.INC
C                                       variables so that std. BP
C                                       routines can be used. PCVEL
C                                       does not use a std. UVGET.
            CURSOU = CVLSOU
            CALL COPY (256, CATBLK, CATUV)
C                                       Now apply BP correction.
            CALL DATBND (TIME, IA1, IA2, VISIN, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL COPY (256, CATUV, CATBLK)
            END IF
C                                       Message every scan
         IF ((JNXRNO.NE.(ONXRNO+1)) .AND. (JNXRNO.GT.0) .AND.
     *      INDXT) THEN
            ONXRNO = ONXRNO + 1
            CALL TODHMS (NXTIM(1,ONXRNO), TIT1)
            CALL TODHMS (NXTIM(2,ONXRNO), TIT2)
            IF (WASHIF(ONXRNO)) THEN
               WRITE (MSGTXT,1100) ONXRNO, PRESOU(NXSOU(ONXRNO)),
     *            TIT1, TIT2
               CALL MSGWRT (3)
               DO 50 IAVG = 1, MXANT
                  DO 49 ISUB = 1, NUMIF
                     IF (COUNTD(IAVG,ISUB).GT.0) THEN
                        REPDEL(IAVG,ISUB) =
     *                     REPDEL(IAVG,ISUB)/COUNTD(IAVG,ISUB)
                        WRITE (MSGTXT,1105) IAVG, ISUB,
     *                     REPDEL(IAVG,ISUB)
                        CALL MSGWRT (3)
                        END IF
 49                  CONTINUE
 50               CONTINUE
               CALL RFILL (MXANT*MAXIF, 0.0, REPDEL)
               CALL FILL (MXANT*MAXIF, 0, COUNTD)
            ELSE
               WRITE (MSGTXT,1110) ONXRNO, PRESOU(NXSOU(ONXRNO)),
     *            TIT1, TIT2
               CALL MSGWRT (3)
               END IF
            END IF
C                                       Shift desired sources
         IF (DOSHFT) THEN
            CALL DSHIFT (VTYPE, DISKIN, CNOIN, RPARM, VISIN, WORK,
     *         IWORDS, FODATI, FODATR, FONUMA, FONIF, EVN, CVLSOU,
     *         APARM, SUPRMS, PRESOU, WUVCMP, FRQSEL, SUBARR, VISNUM,
     *         REPDEL, COUNTD, ANTPRM, NWARN, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,2040) IRET
               GO TO 990
               END IF
            VISNUM = VISNUM + 1
            END IF
C
         CALL RCOPY (NRPARM, RPARM, BUFFER(BIND))
         LDATA = LREC - NRPARM
         IF (WUVCMP) THEN
            LDATA = LDATA * 3
            CALL RCOPY (LDATA, VISIN, BUFFC)
         ELSE
            CALL RCOPY (LDATA, VISIN, BUFFER(BIND+NRPARM))
            END IF
C                                       Roll the AP?
         IF (USEAP) THEN
            CALL QROLL (NROLL, WORK, NBYTES, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,2050) IRET
               GO TO 990
               END IF
            END IF
C                                       update NX table
         CALL RNXUPD (RPARM, RNXRET)
C                                       Write new
         NIO = 1
         XCOUNT = XCOUNT + 1
         IF (WUVCMP) THEN
            DO 90 J = 1, NDECOM
               CALL ZUVPAK (DECOM(1,J), BUFFC, BUFFER(BIND+LLOCWT),
     *            BUFFER(BIND+NRPARM))
 90            CONTINUE
            CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, KBIND, IRET)
            BIND = KBIND
         ELSE
            CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IRET)
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1090) IRET
            GO TO 990
            END IF
C
 100     CONTINUE
C                                       If FINISH shut down output
 110  IF (XCOUNT.EQ.NVIS) SAMNO = .TRUE.
      NVIS = XCOUNT
C                                       Flush output
      NIO = 0
      IF (WUVCMP) THEN
         CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, KBIND, IRET)
      ELSE
         CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) IRET
         GO TO 990
         END IF
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
C                                       Final message
      IF (INDXT) THEN
         ONXRNO = ONXRNO + 1
         CALL TODHMS (NXTIM(1,ONXRNO), TIT1)
         CALL TODHMS (NXTIM(2,ONXRNO), TIT2)
         IF (WASHIF(ONXRNO)) THEN
            WRITE (MSGTXT,1100) ONXRNO, PRESOU(NXSOU(ONXRNO)), TIT1,
     *         TIT2
            CALL MSGWRT (3)
            DO 115 IAVG = 1, MXANT
               DO 114 ISUB = 1,10
                  IF (COUNTD(IAVG,ISUB).GT.0) REPDEL(IAVG,ISUB) =
     *               REPDEL(IAVG,ISUB)/COUNTD(IAVG,ISUB)
                  IF (COUNTD(IAVG,ISUB).GT.0) THEN
                     WRITE (MSGTXT,1105) IAVG, ISUB, REPDEL(IAVG,ISUB)
                     CALL MSGWRT (3)
                  END IF
 114              CONTINUE
 115           CONTINUE
         ELSE
            WRITE (MSGTXT,1110) ONXRNO, PRESOU(NXSOU(ONXRNO)), TIT1,
     *         TIT2
            CALL MSGWRT (3)
            END IF
         END IF
C                                       Compress output file.
      NVIS = XCOUNT + VISOFF
      CALL UCMPRS (NVIS, DISOUT, CNOOUT, LUN, CATOUT, IRET)
C                                       If single source file update
C                                       alternate header values.
      IF (SINGLE) THEN
         IF (APARM(3).EQ.0.0) CATOUT(KIALT) = 1
         IF (APARM(3).EQ.1.0) CATOUT(KIALT) = 2
         IF (APARM(4).EQ.1.0) CATOUT(KIALT) = CATOUT(KIALT) + 256
         IF ((APARM(5)+APARM(6)).GT.0.0) THEN
            CATOD(KDRST) = APARM(5)
            CATOD(KDRST) = CATOD(KDRST) + APARM(6)
            END IF
         CATOR(KRARP) = APARM(2)
         CATOD(KDARV) = APARM(1)
         END IF
C                                       Copy relevant portion of IF
C                                       table to output.
      IF (JLOCIF.GT.0) THEN
C                                       leave this to fool Intel compiler
         MSGTXT = 'AT CHNDAT'
C         CALL MSGWRT (8)
C                                       Re-read old, incase the arrays
C                                       have been fiddled with.
         CVER = 1
         CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, CVER, CATBLK,
     *      ICHLUN, NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Fixup offsets
         DO 150 I = 1, NUMIF
            FOFF(I) = FOFF(I) - FOFF(1)
 150        CONTINUE
C                                       Rewrite new
         CVER = 1
         CALL CHNDAT ('WRIT', BUFFER, DISOUT, CNOOUT, CVER, CATOUT,
     *      ICHLUN, NUMIF, FOFF(1), ISBAND(1), FINC(1), BNDCOD, FRQSEL,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Close files
      CALL ZCLOSE (LUN, FIND, IRET)
      CALL ZCLOSE (IULUN, IUFIND, IRET)
C                                       Close CL table
      IF (.NOT.SINGLE) THEN
C                                       leave this to fool Intel compiler
         MSGTXT = 'AT DSHIFT'
C         CALL MSGWRT (8)
         VISNUM = -1
         CALL DSHIFT (VTYPE, DISKIN, CNOIN, RPARM, VISIN, WORK, IWORDS,
     *      FODATI, FODATR, FONUMA, FONIF, EVN, CVLSOU, APARM, SUPRMS,
     *      PRESOU, WUVCMP, FRQSEL, SUBARR, VISNUM, REPDEL, COUNTD,
     *      ANTPRM, NWARN, IRET)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CVSHFT: ERROR',I5,' OPENING OUTPUT FILE')
 1010 FORMAT ('CVSHFT: ERROR',I5,' OPENING INPUT FILE')
 1020 FORMAT ('CVSHFT: ERROR',I5,' INIT. OUTPUT FILE')
 1090 FORMAT ('CVSHFT: ERROR',I5,' WRITING OUTPUT FILE')
 1100 FORMAT ('Scan ',I4,' Source ',A8,1X,I3,'/',3I3.2,' - ',
     *   I3,'/',3I3.2,' Shifting')
 1105 FORMAT (5X,'Ant: ',I3,' IF#: ',I2,' Average shift = ',F12.5)
 1110 FORMAT ('Scan ',I4,' Source ',A8,1X,I3,'/',3I3.2,' - ',
     *   I3,'/',3I3.2,' No shift')
 1900 FORMAT ('CVSHFT: ERROR',I3,' RETURNED FROM CVSOUF')
 2040 FORMAT ('CVSHFT: ERROR',I3,' RETURNED FROM DSHIFT')
 2050 FORMAT ('CVSHFT: ERROR',I3,' WHILE RELOADING AP')
      END
      SUBROUTINE PCVELH (IWORDS, FODATI, FODATR)
C-----------------------------------------------------------------------
C   PCVELH copies and updates history file.  It also copies any tables
C   extension files.
C    Input from common:
C     DISOUT    I    Output file disk number
C     CNOOUT    I    Output file catalog slot number.
C-----------------------------------------------------------------------
      INTEGER   IWORDS, FODATI(IWORDS,*)
      REAL      FODATR(IWORDS,*)
C
      CHARACTER NOTTYP(5)*2,LINE*72, UTYPE*2, ANTCOM(3)*20
      INTEGER   IERR, LUN1, LUN2, NONOT
      REAL      BUFFH(1024)
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PCVEL.INC'
      EQUIVALENCE (BUFFER(1025), BUFFH)
      DATA LUN1, LUN2 /28,29/
      DATA NONOT, NOTTYP /3, 'CH','FQ','NX','  ','  '/
      DATA T /.TRUE./
      DATA ANTCOM /'Earth center', 'Low antenna number',
     *   'High antenna number'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISOUT, CNOIN, CNOOUT, CATOUT,
     *   BUFF1, BUFFH, IERR)
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
C                                       TIMERANG
      WRITE (LINE,2000) TSKNAM, XTIME
      CALL HIADD (LUN2, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Flagging
      WRITE (LINE,2001) TSKNAM, FGVER
      IF (FGVER.GT.0) THEN
         CALL HIADD (LUN2, LINE, BUFFH, IERR)
         END IF
      IF (IERR.NE.0) GO TO 200
C                                       Bandpass
      IF (DOBAND.GT.0) THEN
C                                       Table
         WRITE (LINE,2002) TSKNAM, BPVER
         CALL HIADD (LUN2, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Method
         WRITE (LINE,2003) TSKNAM, DOBAND
         CALL HIADD (LUN2, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Velocities & ref. pixels
      IF (SINGLE) THEN
         WRITE (LINE,2004) TSKNAM, APARM(1), APARM(2)
         CALL HIADD (LUN2, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 200
      ELSE
         WRITE (LINE,2005) TSKNAM
         CALL HIADD (LUN2, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       forced telescope
      IF ((ANTPRM.GE.1) .AND. (ANTPRM.LE.3)) THEN
         WRITE (LINE,2006) TSKNAM, ANTPRM, ANTCOM(ANTPRM)
         CALL HIADD (LUN2, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFFH, IERR)
C                                       Copy tables
      IF (DOBAND.GT.0) THEN
         NONOT = NONOT + 1
         NOTTYP(NONOT) = 'BP'
         MSGTXT = 'Bandpass table applied - not copied to output'
         CALL MSGWRT (3)
         END IF
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
C                                       write out updated shifts
      CALL FOUPDT (IWORDS, FODATI, FODATR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISOUT, CNOOUT, CATOUT, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clear status
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISOUT, CNOOUT, NAMOUT, CLAOUT,
     *   CATOUT(KIIMS), UTYPE, NLUSER, 'CLWR', BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCVELH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'Timerang=',3F4.0,F6.3,3F4.0,F6.3)
 2001 FORMAT (A6,'/ Edited using FG table version',I3)
 2002 FORMAT (A6,'BPVER =',I3,' / BP table')
 2003 FORMAT (A6,'DOBAND =',I2,' / BP method')
 2004 FORMAT (A6,'/ Velocity: ',F8.2,' Ref. pixel: ',F5.1)
 2005 FORMAT (A6,'/ Used velocities in SU table')
 2006 FORMAT (A6,'APARM(10)=',I2,' / Vel Ref location ',A)
      END
      SUBROUTINE FOUPDT (IWORDS, FODATI, FODATR)
C-----------------------------------------------------------------------
C   FOUPDT updates a CL or FO table with new shifts or writes a new FO
C   table
C   Inputs:
C      IWORDS   I        First axis size
C      FODATI   I(*<*)   FO data array - I form
C      FODATR   R(*<*)   FO data array - R form
C-----------------------------------------------------------------------
      INTEGER   IWORDS, FODATI(IWORDS,*)
      REAL      FODATR(IWORDS,*)
C
      INCLUDE 'PCVEL.INC'
      INTEGER   I, DOPKOL, NKEY, REC2(XCLRSZ), IPOINT, NCOL, NREC,
     *   DATP(128,2), KOLS, FOKOLS(7), FONUMV(7), SOURID, ANTNO, SUBA,
     *   FREQID, IERR, VER, IFNO, LUNO
      REAL      REC4(XCLRSZ), TIMEI, DOPOFF(MAXIF)
      CHARACTER COLHED*24
      DOUBLE PRECISION REC8(XCLRSZ/2), TIME
      EQUIVALENCE (REC2, REC4, REC8)
      EQUIVALENCE (DOPKOL,XLKOLS(1))
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCVL.INC'
      DATA COLHED /'DOPPOFF'/
C-----------------------------------------------------------------------
C                                       Create an FO table from the NX
      LUNO = 79
      IF (FOTYPE.EQ.' ') THEN
         VER = 0
         CALL FOINI ('WRIT', XLBUFF, DISOUT, CNOOUT, VER, CATOUT,
     *      LUNO, ICLRNO, FOKOLS, FONUMV, FONANT, FONIF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN NEW FO TABLE'
            GO TO 980
            END IF
         DO 20 I = 1,FONUMA
            ICLRNO = I
            TIME = (FODATR(4,I) + FODATR(5,I)) / 2.0
            TIMEI = FODATR(5,I) - FODATR(4,I)
            SOURID = FODATI(2,I)
            ANTNO = FODATI(3,I)
            SUBA = SUBARR
            FREQID = FRQSEL
            DO 10 IFNO = 1,FONIF
               IF (FODATR(5+2*FONIF+IFNO,I).GT.0.5) THEN
                  DOPOFF(IFNO) = FODATR(5+FONIF+IFNO,I) /
     *               FODATR(5+2*FONIF+IFNO,I)
               ELSE
                  DOPOFF(IFNO) = FODATR(5+IFNO,I)
                  END IF
 10            CONTINUE
            CALL TABFO ('WRIT', XLBUFF, ICLRNO, FOKOLS, FONUMV, TIME,
     *         TIMEI, SOURID, ANTNO, SUBA, FREQID, DOPOFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'WRITE FO TABLE'
               GO TO 980
               END IF
 20         CONTINUE
         CALL TABFO ('CLOS', XLBUFF, ICLRNO, FOKOLS, FONUMV, TIME,
     *      TIMEI, SOURID, ANTNO, SUBA, FREQID, DOPOFF, I)
         WRITE (MSGTXT,1020) VER
         CALL MSGWRT (3)
C                                       other types: update
      ELSE
         VER = FOVER
         NKEY = 0
         NREC = 0
         NCOL = 0
         IXLRNO = 1
         CALL TABINI ('WRIT', FOTYPE, DISOUT, CNOOUT, VER, CATOUT,
     *      LUNO, NKEY, NREC, NCOL, DATP, XLBUFF, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1030) IERR, FOTYPE, VER
            GO TO 980
            END IF
         NKEY = 1
         CALL FNDCOL (NKEY, COLHED, 24, .TRUE., XLBUFF, KOLS, IERR)
         IPOINT = KOLS
         XLKOLS(1) = DATP(IPOINT,1)
         XLNUMV(1) = DATP(IPOINT,2) / 10
         DO 50 I = 1,FONUMA
            ICLRNO = FODATI(1,I)
            CALL TABIO ('READ', 0, ICLRNO, REC4, XLBUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READING ' // FOTYPE //
     *            ' TABLE'
               GO TO 980
               END IF
            DO 40 IFNO = 1,FONIF
               IF (FODATR(5+2*FONIF+IFNO,I).GT.0.5) THEN
                  DOPOFF(IFNO) = FODATR(5+FONIF+IFNO,I) /
     *               FODATR(5+2*FONIF+IFNO,I)
               ELSE
                  DOPOFF(IFNO) = FODATR(5+IFNO,I)
                  END IF
 40            CONTINUE
            CALL RCOPY (FONIF, DOPOFF, REC4(DOPKOL))
            ICLRNO = FODATI(1,I)
            CALL TABIO ('WRIT', 0, ICLRNO, REC4, XLBUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'WRITING ' // FOTYPE //
     *            ' TABLE'
               GO TO 980
               END IF
 50         CONTINUE
         CALL TABIO ('CLOS', 0, ICLRNO, REC4, XLBUFF, IERR)
         WRITE (MSGTXT,1050) FOTYPE, VER
         CALL MSGWRT (3)
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FOUPDT: ERROR',I4,' DOING ',A)
 1020 FORMAT ('Created FO table version',I4,' with frequency offsets')
 1030 FORMAT ('FOUPDT ERROR',I4,' INIT ',A,' TABLE VERSION',I4)
 1050 FORMAT ('Updated ',A,'  table version',I4,
     *   ' with frequency offsets')
      END
      SUBROUTINE CVSOUF (IERR)
C-----------------------------------------------------------------------
C   Fills in arrays of source numbers to be shifted.
C   Inputs from common /PCVELCM/
C      SHFSRC(30)  C*16  Names (16 char) of up to 30 sources, *=>all
C                        First character of name '-' => all except those
C                        specified.
C   Output:
C      IERR         I    Return code, 0=>OK, otherwise source file
C                        exists but cannot be read.
C                        1=TABIO problem, 2=no sources or calibrators
C   Output to common /PCVELCM/:
C      NCSOU        I    Number of sources included or excluded; if
C                        0 all sources are included.
C      DOCSOU       L    If .TRUE. then sources in CSUWAN are included
C                        If .FALSE. then excluded.
C      CSUWAN(*)    I    The source numbers of sources included or
C                        excluded.
C      CSUWTB(*)    I    The SoUrce table row numbers corresponding
C                        to CSUWAN.
C-----------------------------------------------------------------------
      CHARACTER  VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4, TMPNAM*16
      INTEGER   IERR, JERR, IBUFF(1024), IDSOU, SUKOLS(20),
     *   SUNUMV(20), QUAL, INOGRP, J, K, NSOU, TMPLUN, I,
     *   ISURNO, BADCNT, YSTBSZ, BBUFF(1024), SUFQID
      LOGICAL   T, F, EQUAL, TABLE, EXIST, FITASC, ALLSOU,
     *   DESLT
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'PCVEL.INC'
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION    LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA YSTBSZ /XSTBSZ/
C-----------------------------------------------------------------------
C                                       No SOURCE file
      NCSOU = 0
C                                       See if SU file exists.
      TMPLUN = 50
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, TMPLUN, IBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) GO TO 999
C                                       Open SU table
      CALL SOUINI ('READ', IBUFF, DISKIN, CNOIN, 1, CATBLK, TMPLUN,
     *   INOGRP, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *   JERR)
      IF (JERR.LE.0) GO TO 20
         WRITE (MSGTXT,1000) JERR
         GO TO 990
C                                       Get number of sources.
 20   NSOURC = IBUFF(5)
C                                       Check if empty
      IF (NSOURC.LE.0) GO TO 900
      DOCSOU = T
      ALLSOU = F
      NSOU = 0
C                                       Check if source/calib excluded
C                                       or if all are included
      DO 30 J = 1,30
C                                       Sources
         EQUAL = SHFSRC(J)(1:1).EQ.'*'
         ALLSOU = ALLSOU .OR. EQUAL
         DESLT = SHFSRC(J)(1:1).EQ.'-'
         IF (DESLT) DOCSOU = F
C                                       Find number of sources
         EQUAL = SHFSRC(J).EQ.'                '
         IF (.NOT.EQUAL) NSOU = J
C                                       Remove any minus sign
         IF (DESLT) TMPNAM = ' '
         IF (DESLT) TMPNAM(1:15) = SHFSRC(J)(2:16)
         IF (DESLT) SHFSRC(J) = TMPNAM
 30      CONTINUE
C                                       Make sure need to look at table
      ALLSOU = ALLSOU .OR. (NSOU.LE.0)
      IF (NSOU.LE.1) NSOU = 1
      BADCNT = 0
C                                       Sources
C                                       Loop through records
      DO 100 I = 1,NSOURC
         IERR = 1
C                                       Read record
         ISURNO = I
         CALL TABSOU ('READ', IBUFF, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *      PMRA, PMDEC, JERR)
C                                       Check error
         IF (JERR.LE.0) GO TO 35
         WRITE (MSGTXT,1020) JERR
         GO TO 990
 35      IERR = 0
         PRESOU(IDSOU) = SOUNAM
C                                       Remove nulls from source name
C                                       Search lists
         DO 80 J = 1,NSOU
C                                       Source list
            IF (.NOT.ALLSOU) THEN
               EQUAL = (SHFSRC(J).EQ.SOUNAM) .AND.
     *            ((SELQUA.EQ.QUAL) .OR. (SELQUA.LT.0))
               IF (.NOT.EQUAL) GO TO 80
               END IF
C                                       Redundancy check
            IF (NCSOU.GE.1) THEN
               DO 40 K = 1,NCSOU
                  IF (CSUWAN(K).EQ.IDSOU) GO TO 80
 40               CONTINUE
               END IF
C                                       Add source
            IF (NCSOU.LT.YSTBSZ) THEN
               NCSOU = NCSOU + 1
               CSUWAN(NCSOU) = IDSOU
               CSUWTB(NCSOU) = ISURNO
C                                       Overflowed array
            ELSE
               BADCNT = BADCNT + 1
               END IF
 80         CONTINUE
 100     CONTINUE
      IF (BADCNT.GT.0) THEN
         WRITE (MSGTXT,1100) BADCNT, YSTBSZ
         CALL MSGWRT (6)
         END IF
C                                       No sources found
      IF (NCSOU.LE.0) THEN
         IERR = 0
         MSGTXT = 'WARNING: no sources will be shifted'
         CALL MSGWRT (6)
         END IF
      GO TO 900
C                                       Close file
 900  IF (JERR.LE.0) CALL TABIO ('CLOS', 0, I, BBUFF, IBUFF, JERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CVSOUF: ERROR',I3,' OPENING SOURCE TABLE')
 1020 FORMAT ('CVSOUF: ERROR',I3,' READING SOURCE TABLE')
 1100 FORMAT ('CVSOUF: ',I5,' MORE SOURCES SELECTED THAN ',I6,
     *   ' ALLOWED')
      END
      SUBROUTINE CALAP (BUFF1, BUFF2, AMP, PHASE)
C-----------------------------------------------------------------------
      REAL BUFF1, BUFF2, AMP, PHASE
C-----------------------------------------------------------------------
      AMP = SQRT (BUFF1*BUFF1 + BUFF2*BUFF2)
      PHASE = 57.296 * ATAN2 (BUFF2, (BUFF1+1.0E-10))
      RETURN
      END
      SUBROUTINE NXSET (IRET)
C-----------------------------------------------------------------------
C   Routine to read the index table and set up the arrau NXVISN which
C   PCVEL uses to determine the scan boundaries.
C   Output:
C      IRET          I     0 => OK, anything else = fails
C   Output in common:
C      NXVISN(2,*)   I     1,* => first vis number of scan n
C                          2,* => last vis number of scan n
C      NXTIM(2,*)    R     start and finish times of scan n
C      NXSOU(*)      I     source of scan
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INTEGER NUMNXT, I, IDSOUR, ISUBA, VSTART, VEND, FREQID
      REAL    TIME, DTIME
      INCLUDE 'PCVEL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      CALL FNDEXT ('NX', CATBLK, NUMNXT)
      IF (NUMNXT.EQ.0) THEN
        INDXT = .FALSE.
        GO TO 999
        END IF
C
      INDXT = .TRUE.
      NXVER = 1
      CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATBLK, NXLUN,
     *   INXRNO, NXKOLS, NXNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C
      NUMNX = NXBUFF(5)
      IF (NUMNX.EQ.0) THEN
         INDXT = .FALSE.
         GO TO 999
         END IF
      IF (NUMNX.GT.MAXNX) THEN
         IRET = 1
         WRITE (MSGTXT,1010) NUMNX
         GO TO 990
         END IF
C                                       Read and load NX entries
      DO 100 I = 1, NUMNX
         INXRNO = I
         CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *      TIME, DTIME, IDSOUR, ISUBA, VSTART, VEND, FREQID,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         NXVISN(1,I) = VSTART
         NXVISN(2,I) = VEND
         NXTIM(1,I) = TIME - 0.5 * DTIME
         NXTIM(2,I) = TIME + 0.5 * DTIME
         NXSOU(I) = IDSOUR
 100     CONTINUE
C
      CALL TABIO ('CLOS', 0, INXRNO, NXBUFF, NXBUFF, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NXSET: ERROR ',I4,' OPENING NX TABLE')
 1010 FORMAT ('NXSET: ',I8,' NX ENTRIES TOO LARGE, INCREASE MAXNX')
 1020 FORMAT ('NXSET: ERROR ',I4,' READING NX TABLE')
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
     *   JPOLN, LIMF1, LIMF2, LIMC1, LIMC2, INDEX, IFADD,
     *   STADD, IPOLPT
      LOGICAL   DROP, GOOD
      REAL      TIME, SUM, BASE, RPARM(*), VISIN(3,*)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'PCVEL.INC'
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
         BASE = RPARM(1+ILOCB)
         KBASE = BASE + 0.1
         A1 = BASE / 256 + 0.1
         A2 = BASE - 256 * A1 + 0.1
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
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.CVLSOU) .AND. (FLGSOU(IFLAG).NE.0))
     *      GO TO 500
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
C                                       Check freqid.
         IF (CURFQI.GT.0) THEN
            IF ((FLGFQD(IFLAG).GT.0) .AND.
     *         (FLGFQD(IFLAG).NE.CURFQI)) GO TO 500
            END IF
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
C                                       Loop over IF
      DO 530 JIF = 1, NUMIF
         IFADD = (JIF-1) * FKNCIF + 1
C                                       Loop over polarizations
         DO 520 JPOLN = 1,FKNCOR
            INDEX = IFADD + (JPOLN-1) * FKNCS
C                                       Multiple channels
            SUM = 0.0
            DO 510 JCHAN = 1, NUMFRQ
               SUM = SUM + MAX (0.0, VISIN(3,INDEX))
               INDEX = INDEX + FKNCF
 510           CONTINUE
            GOOD = GOOD .OR. (SUM.GT.0.0)
 520        CONTINUE
 530     CONTINUE
      DROP = .NOT.GOOD
C
 999  RETURN
      END
      SUBROUTINE CVNXFG (TIME, IERR)
C-----------------------------------------------------------------------
C   Updates flagging tables in common fron an FG table.
C     Inputs:
C      TIME         R    Current time (days) for flag entries
C     Inputs from common /CFMINF/(INCLUDEs C/DSEL.INC):
C      NUMFLG       I    number of current FLAG entries.
C      FGKOLS(MAXFGC) I    The column pointer array in order, SOURCE,
C                        SUBARRAY, FREQID, ANTS, TIMERANG, IFS, CHANS,
C                        PFLAGS, REASON
C      FGNUMV(MAXFGC) I    Element count for each column
C      IFGRNO       I    Current FLAG file record.
C     Output to common /CFMINF/:
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
C      FLGTND(*)  R    End time of flag.
C     Output:
C      IERR       I    Return code, 0=OK, else TABIO error number.
C-----------------------------------------------------------------------
      REAL      TIME
      INTEGER   IERR
C
      INTEGER   J, NDROP, LIMIT, RECI(30), MXFLG, SOUKOL, SUBKOL,
     *   FRQKOL, ANTKOL, TIMKOL, IFKOL, CHKOL, POLKOL, REAKOL, A1, A2,
     *   IT, I4, NFGREC, I, LIMIT4
      REAL      RECORD(31)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'PCVEL.INC'
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
C                                       Check if list exhausted
      IF (IFGRNO.GT.NFGREC) GO TO 999
C                                       Loop through records
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
         IF (TIMORD) THEN
            IF (TIME.LT.RECORD(TIMKOL)) GO TO 999
            IF (TIME.GT.RECORD(TIMKOL+1)) GO TO 360
            END IF
C                                       Check FQ ID.
         IF (RECI(FRQKOL).GT.0) THEN
            IF ((RECI(FRQKOL).NE.CURFQI) .AND. (CURFQI.GT.0))
     *         GO TO 360
            END IF
C                                       Does source number matter?
         IF ((RECI(SOUKOL).LE.0) .OR. (NSOURC.LE.0)) GO TO 500
C                                       Search source lists
            DO 340 J = 1,NSOURC
               IF (RECI(SOUKOL).EQ.CSUWAN(J)) GO TO 350
 340           CONTINUE
C                                       No match: is desirable?
            IF (.NOT.DOCSOU) GO TO 500
            GO TO 360
C                                       Match: is that desirable
 350        IF (DOCSOU) GO TO 500
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
 1500 FORMAT ('TOO MANY FLAGS AT SAME TIME (>',I5,')')
      END
      SUBROUTINE DSHIFT (VTYPE, DISKIN, CNOIN, RPARMS, VIS, WORK,
     *   IWORDS, FODATI, FODATR, FONUMA, FONIF, EVN, CVLSOU, APARM,
     *   SUPRMS, PRESOU, WUVCMP, FRQSEL, SUBARR, VISNUM, REPDEL, COUNTD,
     *   ANTPRM, NWARN, IRET)
C-----------------------------------------------------------------------
C  DSHIFT calculates the necessary channel shift and does
C  it by calling the routines TPSHFT and XCSHFT.
C
C  Input/Output:
C     DISKIN         I         Volume number
C     CNOIN          I         File catalogue number
C     RPARMS(*)      R         Random parameters
C     VIS(3,*)       R         The complex visibility + weight
C                              On output will contain the shifted
C                              data.
C     WORK(*)        R         Work buffer (>= 8192)
C     EVN            L         True if EVN data
C     CVLSOU         I         Source # being shifted
C     APARM(*)       R         User supplied vel info for single
C                              source files
C     SUPRMS         L         If true supress messages about large
C                              shifts
C     PRESOU(*)*16   C         Names of sources to be shifted.
C     WUVCMP         L         Input/output data are compressed
C     FRQSEL         I         Freq ID working on.
C     SUBARR         I         Subarray
C     VISNUM         I         Visibility number. 1 => some things need
C                              to be opened. -1 => some need to be
C                              closed.
C     IRET           I         Error code, = 0 => OK
C
C  P. Diamond ,  March 1988
C-----------------------------------------------------------------------
      INTEGER   VTYPE
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IWORDS, FODATI(IWORDS,*), FONUMA, FONIF, IRET, IANT1,
     *   IANT2, I, IPOL, IFNO, IFRQ, INDEX, ISB, FINDEX, CVLSUB, TIT(4),
     *   ITEL, IREFST, NUMIF, ISBAND(MAXIF), CVLSOU, NFREQ, INCSU,
     *   INCIFU, INCFU, DISKIN, CNOIN, OLDSOU, VISNUM, ITMP, CVER,
     *   ICHLUN, NNIF, FRQSEL, SUBARR, GAMMA, IDUM1, IDUM2, ISMTH,
     *   COUNTD(MAXANT,MAXIF), VTEL, ANTPRM, NWARN
      REAL      RPARMS(*), VIS(*), UT, WORK(*), FODATR(IWORDS,*),
     *   PIXSPC, FPIX, NPIX, APARM(*), FINC(MAXIF), REPDEL(MAXANT,MAXIF)
      DOUBLE PRECISION FINT, DOPVEL, VELTOT, FREQTO, DELI, VELSPC,
     *   FOFF(MAXIF)
      LOGICAL   DOINTP, ALLFLG, MULTI, SUPRMS, WUVCMP, CLSORT, EVN
      CHARACTER PRESOU(*)*16, BNDCOD(MAXIF)*8
C
      REAL   VISTMP(2,MAXCHA), TMPWT(MAXCHA), INTWTS(MAXCHA)
      DOUBLE PRECISION   FRQOFF(MAXIF), ARRLON
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DAPM.INC'
      SAVE FOFF, ISBAND, FINC, OLDSOU
C-----------------------------------------------------------------------
C                                       Close down stuff
      IF (VISNUM.EQ.-1) THEN
         IF (USEAP) CALL QRLSE
C        CALL TABIO ('CLOS', 0, IXLRNO, XLBUFF, XLBUFF, IRET)
         IRET = 0
         GO TO 999
         END IF
      CLSORT = .TRUE.
C
      IF (VISNUM.EQ.1) THEN
C                                       Initialize stuff
C                                       Do we use PSAP FFT
         NFREQ = CATBLK(KINAX+JLOCF)
         POWRTO = .FALSE.
         DO 10 GAMMA = 1, 15
            IF ( (2**GAMMA).EQ.NFREQ ) THEN
               POWRTO = .TRUE.
               NXTTWO = NFREQ
               END IF
   10       CONTINUE
C                                       If not is it a prime
C                                       number
         IF (.NOT. POWRTO) CALL ISPRIM (NFREQ, PRIME, NXTTWO)
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
               MSGTXT = 'DSHIFT: DID NOT GET ANY AP MEMORY'
               IRET = 10
               GO TO 990
               END IF
            APBEG = 0
            APTYPE = 2
            NROLL = -1
            NBYTES = 0
         ELSE
            MSGTXT = 'NOT USING FFT: I WILL BE SLOWWWW'
            CALL MSGWRT (6)
            MSGTXT = 'CONSIDER CHANGING THE NUMBER OF CHANNELS'
            CALL MSGWRT (6)
            END IF
C
         OLDSOU = -1
         CLSORT = .FALSE.
         CALL FILL (500, 0, SOUDUN)
         ITMP = MAXANT*MAXIF
         CALL RFILL (ITMP, 0.0, REPDEL)
         CALL FILL (ITMP, 0, COUNTD)
C                                       Determine year and ref.day
C                                       number of observation
         CALL GETTIM (DISKIN, CNOIN, WORK, CATBLK, IYEAR, IRDAY, IATUT,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
C                                       Get frequency info.
         CVER = 1
         ICHLUN = 44
         CALL CHNDAT ('READ', WORK, DISKIN, CNOIN, CVER, CATBLK,
     *      ICHLUN, NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) IRET
            GO TO 990
            END IF
C                                       Fill AN information
C                                       into common in D/CANS.INC
         IF (SUBARR.LE.0) SUBARR = 1
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, WORK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET
            GO TO 990
            END IF
C                                       Correct station positions for
C                                       centre array offset if non-zero
C                                       go to left-hand system
         ARRLON = 0.0D0
         IF ((ABS(CNTRX).GT.1.D2) .AND. (ABS(CNTRY).GT.1.D2) .AND.
     *      (ABS(CNTRZ).GT.1.D2)) ARRLON = ATAN2 (CNTRY, CNTRX)
         DO 60 I = 1, MAXANT
            ANTX(I) = CNTRX + STNX(I)*COS(ARRLON) - STNY(I)*SIN(ARRLON)
            ANTY(I) = CNTRY + STNY(I)*COS(ARRLON) + STNX(I)*SIN(ARRLON)
            ANTY(I) = -ANTY(I)
            ANTZ(I) = CNTRZ + STNZ(I)
   60       CONTINUE
         END IF
C
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      NFREQ = CATBLK(KINAX+JLOCF)
      CALL MULSDB (CATBLK, MULTI)
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
         CVLSUB = RPARMS(ILOCB+1) + 0.1
         CVLSUB = 1.5 + 100.0 * (RPARMS(ILOCB+1) - CVLSUB)
C                                       Carrying correlator ID in .001
C                                       of baseline not generally used
C                                       and almost certainly lost in
C                                       floating point accuracy anyway
         IREFST = RPARMS(ILOCB+1) + 0.1
         IREFST = 0.1 + 10.0 * ((100.0 * (RPARMS(ILOCB+1) - IREFST))
     *      - (CVLSUB - 1))
      ELSE
         IANT1 = RPARMS(ILOCA1+1) + 0.1
         IANT2 = RPARMS(ILOCA2+1) + 0.1
         CVLSUB = RPARMS(ILOCSA+1) + 0.1
         IREFST = 0
         END IF
      IF (IREFST.EQ.0) ITEL = IANT1
      IF (IREFST.GT.0) ITEL = IANT2
C                                       so replace above with a guess
      IF (EVN) THEN
         ITEL = IANT2
      ELSE
         ITEL = IANT1
         END IF
      VTEL = ITEL
      IF (ISVLBA) VTEL = 0
      IF (ANTPRM.EQ.1) THEN
         VTEL = 0
      ELSE IF (ANTPRM.EQ.2) THEN
         VTEL = IANT1
      ELSE IF (ANTPRM.EQ.3) THEN
         VTEL = IANT2
         END IF
      IF (VTEL.GT.0) ITEL = VTEL

C                                       Get basic freq. parms
      CALL GETFRQ (DISKIN, CNOIN, APARM, OLDSOU, CVLSOU, FRQSEL,
     *   CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Obtain any other frq. offsets
C                                       hiding in the CL table
      CALL FRQUPT (IWORDS, FODATI, FODATR, FONUMA, FONIF, ITEL,
     *   CVLSOU, UT, FRQOFF)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Determine full sky frequency
C                                       of reference pixel for each IF
      DO 100 I = 1,NUMIF
         OBSFRQ(I) = REFFRQ(I) + FRQOFF(I)
 100     CONTINUE
C                                       Determine Doppler velocity
      CALL CVLDOP (VTYPE, DISKIN, CNOIN, UT, VTEL, CVLSOU, OLDSOU,
     *   CVLSOU, DOPVEL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
          END IF
C                                       Loop and shift
      DO 300 IPOL = 1, NCOR
         DO 200 IFNO = 1, NUMIF
C                                       Signed frequency increment
            FINT = FINC(IFNO)
C                                       Determine source velocity
            IF (.NOT.MULTI) THEN
C                                       Frequency reference pixel
               FPIX = CATR(KRCRP+JLOCF)
C                                       Pixel of desired velocity
               NPIX = FPIX
C                                       Specified velocity
               VELSPC = DBLE(APARM(1))
C                                       Pixel of specified velocity
               PIXSPC = APARM(2)
C
               CALL VELPIX (PIXSPC, VELSPC, OBSFRQ(IFNO), FPIX,
     *            FINT, RADIO, NPIX, VEL(IFNO))
               END IF
C                                       Total velocity of
C                                       reference pixel
            VELTOT = VEL(IFNO) + DOPVEL
C                                       Frequency of reference pixel
C                                       in desired frame of reference
            IF (.NOT. RADIO) THEN
               FREQTO = RSTFRQ(IFNO) / (1.D0 + VELTOT / VELITE)
            ELSE
               FREQTO = RSTFRQ(IFNO) * (1.D0 - VELTOT / VELITE)
               END IF
C                                       Required pixel shift
            DELI = (OBSFRQ(IFNO) - FREQTO) / FINT
            REPDEL(ITEL,IFNO) = REPDEL(ITEL,IFNO) + DELI
            COUNTD(ITEL,IFNO) = COUNTD(ITEL,IFNO) + 1
            IF (IPOL.EQ.1) FRQOFF(IFNO) = FREQTO - REFFRQ(IFNO)
C                                       If shift too large, give
C                                       warning
            IF ((ABS(DELI/REAL(NFREQ)).GT.0.10) .AND.
     *         (.NOT.SUPRMS)) THEN
               NWARN = NWARN + 1
               IF (NWARN.LE.100) THEN
                  CALL TODHMS (UT, TIT)
                  WRITE (MSGTXT,1020) TIT, DELI
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1025) PRESOU(CVLSOU), IANT1, IANT2,
     *               IFNO
                  CALL MSGWRT (6)
                  END IF
               END IF
C                                       Copy data to temp array.
            DOINTP = .FALSE.
            INDEX = 1 + (IPOL-1) * INCSU + (IFNO-1) * INCIFU
            DO 120 IFRQ = 1,NFREQ
               FINDEX = INDEX + (IFRQ-1) * INCFU
               VISTMP(1,IFRQ) = VIS(FINDEX)
               VISTMP(2,IFRQ) = VIS(FINDEX+1)
               TMPWT(IFRQ)    = VIS(FINDEX+2)
               IF (TMPWT(IFRQ).LE.0.0) DOINTP = .TRUE.
 120           CONTINUE
C                                       Deal with flagged
C                                       spectral data
            ALLFLG = .FALSE.
            IF (DOINTP) CALL SPINTP (NFREQ, VISTMP, TMPWT, INTWTS,
     *         ALLFLG)
            IF (ALLFLG) THEN
               DO 125 IFRQ = 1,NFREQ
                  FINDEX = INDEX + (IFRQ-1) * INCFU
                  IF (VIS(FINDEX+2).GT.0.0) VIS(FINDEX+2) =
     *               -1.0 * VIS(FINDEX+2)
 125               CONTINUE
               GO TO 200
               END IF
C                                       Shift it!
            ISB = 1
            IF (FINC(IFNO).LT.0.0) ISB = -1
C                                       Smooth or not
            ISMTH = 0
C
            IF (IANT1.EQ.IANT2) THEN
               IF (APARM(9).GT.0.0) ISMTH = 1
               CALL ACSHFT (VISTMP, ISB, NFREQ, DELI, WORK, ISMTH)
            ELSE
               IF (APARM(9).GT.1.0) ISMTH = 1
               CALL XCSHFT (VISTMP, ISB, NFREQ, DELI, WORK, ISMTH)
               END IF
C                                       Shift weights
CC             CALL WTSHFT (TMPWT, NFREQ, DELI)
C                                       Copy data back to vis
C                                       array
            DO 140 IFRQ = 1, NFREQ
               FINDEX = INDEX + (IFRQ-1) * INCFU
               VIS(FINDEX) = VISTMP(1,IFRQ)
               VIS(FINDEX+1) = VISTMP(2,IFRQ)
               VIS(FINDEX+2) = TMPWT(IFRQ)
 140           CONTINUE
 200        CONTINUE
 300     CONTINUE
C                                       remember the offset used
      I = CURRNO(ITEL)
      IF (I.GT.0) THEN
         DO 310 IFNO = 1,FONIF
            FODATR(5+FONIF+IFNO,I) = FODATR(5+FONIF+IFNO,I) +
     *         FRQOFF(IFNO)
            FODATR(5+2*FONIF+IFNO,I) = FODATR(5+2*FONIF+IFNO,I) + 1.0
 310        CONTINUE
         END IF
      GO TO 999
C                                       Write Error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DSHIFT: ERROR',I4,' DETERMINING FREQ./VEL. PARMS')
 1010 FORMAT ('DSHIFT: ERROR',I4,' DETERMINING FULL SKY FREQ.')
 1020 FORMAT ('Warning: Time = ',I4,'/',3I3,' Channel shift = ',F15.3)
 1025 FORMAT ('For Source: ',A16,' Antennas: ',I3,'-',I3,' IF# ',I3)
 1030 FORMAT ('DSHIFT: ERROR ',I4,' DETERMINING DOPPLER VELOCITY')
 1040 FORMAT ('DSHIFT: ERROR',I3,' DETERMINING YEAR AND DAY NUMBER')
 1050 FORMAT ('DSHIFT: ERROR',I3,' GETTING FREQ. INFO. WITH CHNDAT')
 1060 FORMAT ('DSHIFT: ERROR',I3,' OBTAINING ANTENNA INFORMATION')
      END
      SUBROUTINE FOFILL (IWORDS, FODATI, FODATR, IRET)
C-----------------------------------------------------------------------
C   Reads the FO or CL table for frequency offsets or makes up one
C   from the NX table
C   Inputs:
C      IWORDS   I             Words per record
C   Outputs:
C      FODATI   I(IWORDS,*)   Integer buffer of FO data
C      FODATR   R(IWORDS,*)   Real buffer of FO data
C      IRET     I             Error code
C-----------------------------------------------------------------------
      INTEGER   IWORDS, FODATI(IWORDS,*), IRET
      REAL      FODATR(IWORDS,*)
C
      INCLUDE 'PCVEL.INC'
      INTEGER   I, J, K, L, TIMKOL, SOUKOL, ANTKOL, DOPKOL, INTKOL,
     *   SUBKOL, FQKOL, NKEY, REC2(XCLRSZ), IPOINT, NCOL, NREC,
     *   DATP(128,2), KOLS(7)
      REAL      T1, T2, REC4(XCLRSZ)
      CHARACTER COLHED(7)*24
      DOUBLE PRECISION REC8(XCLRSZ/2)
      EQUIVALENCE (REC2, REC4, REC8)
      EQUIVALENCE (TIMKOL,XLKOLS(1)),  (INTKOL,XLKOLS(2)),
     *   (SOUKOL,XLKOLS(3)), (ANTKOL,XLKOLS(4)), (SUBKOL,XLKOLS(5)),
     *   (FQKOL,XLKOLS(6)), (DOPKOL,XLKOLS(7))
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCVL.INC'
      DATA COLHED /'TIME', 'TIME INTERVAL', 'SOURCE ID', 'ANTENNA NO.',
     *   'SUBARRAY', 'FREQ ID', 'DOPPOFF'/
C-----------------------------------------------------------------------
C                                       make up from NX table
      L = 0
      IF (FOTYPE.EQ.' ') THEN
         DO 40 I = 1,NUMNX
            T2 = NXTIM(1,I)
            DO 30 J = 1,4
               T1 = T2
               T2 = T1 + (NXTIM(2,I)-NXTIM(1,I)) / 4.0
               DO 20 K = 1,FONANT
                  L = L + 1
                  FODATI(1,L) = L
                  FODATI(2,L) = NXSOU(I)
                  FODATI(3,L) = K
                  FODATR(4,L) = T1
                  FODATR(5,L) = T2
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
C                                       get from FO or CL table
      ELSE
         ICXLUN = 49
         NKEY = 0
         NREC = 0
         NCOL = 0
         IXLRNO = 1
         CALL TABINI ('READ', FOTYPE, DISKIN, CNOIN, GUSE, CATBLK,
     *      ICXLUN, NKEY, NREC, NCOL, DATP, XLBUFF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1040) IRET, FOTYPE, GUSE
            FONUMR = 0
            GO TO 990
            END IF
         FOVER = GUSE
         WRITE (MSGTXT,1041) FOTYPE, FOVER
         CALL MSGWRT (4)
C                                       Get number of scans
         FONUMR = XLBUFF(5)
C                                       Check if empty
         IF (FONUMR.LE.0) THEN
            MSGTXT = FOTYPE // ' TABLE IS EMPTY'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       get column pointers
         NKEY = 7
         CALL FNDCOL (NKEY, COLHED, 24, .TRUE., XLBUFF, KOLS, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 999
         IRET = 0
         CALL FILL (NKEY, 0, XLKOLS)
         CALL FILL (NKEY, 0, XLNUMV)
         DO 50 J = 1,NKEY
            IPOINT = KOLS(J)
            IF (IPOINT.NE.0) THEN
               XLKOLS(J) = DATP(IPOINT,1)
               XLNUMV(J) = DATP(IPOINT,2) / 10
               END IF
 50         CONTINUE
         DO 70 I = 1,FONUMR
            IXLRNO = I
            CALL TABIO ('READ', 0, IXLRNO, REC4, XLBUFF, IRET)
            IF (IRET.LT.0) GO TO 70
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1050) IRET, FOTYPE
               GO TO 990
               END IF
C                                       check subarray
            IF ((REC2(SUBKOL).GE.0) .AND. (SUBARR.GT.0) .AND.
     *         (REC2(SUBKOL).NE.SUBARR)) GO TO 70
C                                       check freqid
            IF ((REC2(FQKOL).GE.0) .AND. (FRQSEL.GT.0) .AND.
     *         (REC2(FQKOL).NE.FRQSEL)) GO TO 70
            L = L + 1
            FODATI(1,L) = I
            FODATI(2,L) = REC2(SOUKOL)
            FODATI(3,L) = REC2(ANTKOL)
            FODATR(4,L) = REC8(TIMKOL) - (REC4(INTKOL)/2.0)
            FODATR(5,L) = REC8(TIMKOL) + (REC4(INTKOL)/2.0)
            CALL RCOPY (FONIF, REC4(DOPKOL), FODATR(6,L))
 70         CONTINUE
         CALL TABIO ('CLOS', 0, IXLRNO, REC4, XLBUFF, I)
         END IF
C                                       actual number used
      FONUMA = L
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('FOFILL ERROR',I3,' OPENING ',A2,' TABLE, VERSION',I5)
 1041 FORMAT ('Using ',A,' table ',I3,' to obtain time dependent freq',
     *   ' offsets')
 1050 FORMAT ('FOFILL ERROR',I4,' READING ',A,' TABLE')
      END
      SUBROUTINE FRQUPT (IWORDS, FODATI, FODATR, FONUMA, FONIF, IANT,
     *   CVLSOU, TIME, FRQOFF)
C-----------------------------------------------------------------------
C   Routine to obtain the freq. offset (if any) stored in the incore
C   tables
C   Inputs:
C      IWORDS   I        Row size of tables
C      FODATI   I(*,*)   Integer table form
C      FODATR   R(*,*)   Real table form
C      FONUMA   I        Actual number of rows in table
C      FONIF    I        Number IFs
C      IANT     I        Antenna number
C      CVLSOU   I        Source number
C      TIME     R        Time of visibility record (days)
C   Input from common /CURCL/
C      CURXLT   D(*)     Time of current CL entry for each antenna
C      CURXLI   R(*)     Interval of current CL entry
C      CURLOO   R(*)     Current lo-offset for each antenna
C      CURRNO   I(*)     Current CL record number for each antenna
C   Outputs:
C      FRQOFF   D(*)     Freq. offset (Hz) - for each if
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IWORDS, FODATI(IWORDS,*), FONUMA, FONIF, IANT, CVLSOU
      REAL      FODATR(IWORDS,*), TIME
      DOUBLE PRECISION FRQOFF(MAXIF)
C
      INTEGER   IFNO, I, LIMIT, ITRY
      REAL      TIMLOW, TIMHI, EPS
      LOGICAL   FOUND
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCVL.INC'
C-----------------------------------------------------------------------
C                                       Set frqoff to a default
      ITRY = 0
      FOUND = .FALSE.
      EPS = 0.02 / (24.0 * 3600.0)
      DO 10 IFNO = 1,FONIF
         FRQOFF(IFNO) = 0.D0
 10      CONTINUE
      IF (FONUMA.LE.0) THEN
         CURRNO(IANT) = 0
         GO TO 999
         END IF
C                                       Do we need to look through table
      IF (CURRNO(IANT).GT.0) THEN
         IF (TIME.LE.(CURXLT(IANT) + 0.5*CURXLI(IANT))) THEN
            DO 20 IFNO = 1,FONIF
               FRQOFF(IFNO) = CURLOO(IANT,IFNO)
 20            CONTINUE
            GO TO 999
            END IF
         END IF
C                                       Read until selected time.
 30   IF (CURRNO(IANT).LE.0) THEN
         LIMIT = 1
      ELSE
         LIMIT = CURRNO(IANT)
         END IF
      DO 50 I = LIMIT,FONUMA
C                                       See if correct source
         IF (FODATI(2,I).NE.CVLSOU) GO TO 50
C                                       See if correct antenna
         IF (FODATI(3,I).NE.IANT) GO TO 50
C                                       Check time
         TIMLOW = FODATR(4,I)
         TIMHI  = FODATR(5,I)
         IF ((TIME.LE.TIMHI) .AND. (TIME.GE.TIMLOW)) THEN
C                                       Loop over IF
            DO 40 IFNO = 1,FONIF
               FRQOFF(IFNO) = FODATR(5+IFNO,I)
               CURLOO(IANT,IFNO) = FODATR(5+IFNO,I)
 40            CONTINUE
            CURXLT(IANT) = (TIMLOW + TIMHI) / 2.0
            CURXLI(IANT) = TIMHI - TIMLOW + EPS
            CURRNO(IANT) = I
            FOUND = .TRUE.
            END IF
         IF ((TIMHI+TIMLOW)/2.0.GT.TIME) GO TO 60
 50      CONTINUE
      GO TO 999
C                                       did we find it
 60   IF (CURRNO(IANT).GT.0) THEN
         IF (TIME.LE.(CURXLT(IANT) + 0.5*CURXLI(IANT))) THEN
            DO 70 IFNO = 1, FONIF
               FRQOFF(IFNO) = CURLOO(IANT,IFNO)
 70            CONTINUE
C                                       found it must be roundoff
         ELSE IF (FOUND) THEN
            DO 75 IFNO = 1, FONIF
               FRQOFF(IFNO) = CURLOO(IANT,IFNO)
 75            CONTINUE
C                                       try a second time
         ELSE
            CURRNO(IANT) = 0
            ITRY = ITRY + 1
            IF (ITRY.LT.2) GO TO 30
            END IF
         END IF
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CVLDOP (VTYPE, DISKIN, CNOIN, SUT, IANT, SOURID,
     *   OLDSOU, CVLSOU, DOPVEL, IERR)
C-----------------------------------------------------------------------
C   CVLDOP is a routine to determine the doppler velocity of a
C   particular source as seen from a particular point on the Earth's
C   surface at a particular time.  The routine does not recalculate the
C   parameters each time but interpolates over a reasonable time
C   interval updating when necessary.
C   Input:
C      DISKIN   I      Volume number
C      CNOIN    I      File catalogue number
C      SUT      R      UT of datum point (days)
C      IANT     I      Antenna number: <= 0 => Earth centric (VLBA...)
C      SOURID   I      Source number
C      IYEAR    I      Year of observation
C      IRDAY    I      Reference day of observation
C      OLDSOU   I      Previous source, if -1 no previous source
C   Input from common:
C      ANTX     D(*)   Antenna x coordinates (m)
C      ANTY     D(*)   Antenna y coordinates (m)
C      ANTZ     D(*)   Antenna z coordinates (m)
C      HELIO    L      True if in heliocentric reference frame
C   Output:
C      CVLSOU   I      Source being shifted
C      DOPVEL   R      Doppler velocity (m/s)
C      IERR     I      Error flag
C   Special version for PCVEL - does ????????
C-----------------------------------------------------------------------
      INTEGER   VTYPE, DISKIN, CNOIN, IANT, SOURID, OLDSOU, CVLSOU, IERR
      REAL      SUT
      DOUBLE PRECISION DOPVEL
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION    UT, DELTIM, VSUN, VT, DTIME, DELVEL, VL, V0,
     *   TBEGIN(MAXANT+1), TSTOP(MAXANT+1), STRTVL(MAXANT+1),
     *   STOPVL(MAXANT+1), DXANT, DYANT, DZANT
      INTEGER   NEWDOP, IIYEAR, NDAY, MXANT, ISLUN, JANT, ID(3)
      LOGICAL   DOINT, T, F
      CHARACTER CTEMP*8
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCVL.INC'
      SAVE TBEGIN, TSTOP, STRTVL, STOPVL
      DATA T /.TRUE./, F /.FALSE./
      DATA MXANT /MAXANT/
      DATA ISLUN /27/
C-----------------------------------------------------------------------
C                                       Initialization
      DELTIM = 300.0D0 / 86400.0D0
      DOINT = T
      HELIO = VTYPE.EQ.4
C                                       defend ourselves
      IF (IYEAR.LE.0) THEN
         CALL H2CHR (8, 1, CATH(KHDOB), CTEMP)
         CALL DATEST (CTEMP, ID)
         IYEAR = ID(1)
         CALL DAYNUM (ID(1), ID(3), ID(2), IRDAY)
         END IF
      IIYEAR = IYEAR
C                                       VLBA uses geocentric ref.
      IF (IANT.LE.0) THEN
         DXANT = 0.0D0
         DYANT = 0.0D0
         DZANT = 0.0D0
         JANT = 1
      ELSE
         JANT = IANT + 1
         DXANT = ANTX(IANT)
         DYANT = ANTY(IANT)
         DZANT = ANTZ(IANT)
         END IF
C                                       Time in d.p
      UT = DBLE (SUT)
C                                       Is antenna number correct
      IF (IANT.GT.MAXANT) THEN
         WRITE (MSGTXT,1000) IANT
         IERR = 1
         GO TO 990
         END IF
C                                       Source change
      IF (SOURID.NE.OLDSOU) THEN
         CALL DFILL (MXANT, 999.D0, TBEGIN)
         CALL GETSOU (SOURID, DISKIN, CNOIN, CATBLK, ISLUN, IERR)
         IF (IERR .NE. 0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         CVLSOU = SOURID
         DOINT = F
         END IF
      OLDSOU = SOURID
      OLDCSU = SOURID
C                                       Check to see if current record
C                                       falls within boundaries of last
C                                       interpolation interval.
      NEWDOP = 0
      V0 = 0.0D0
C                                       PLANET velocity
      CALL PLAVEL (UT, DOPVEL)
C                                       include Earth movement
      IF (VTYPE.GT.1) THEN
 100     IF (UT.LT.TBEGIN(JANT)) THEN
            TBEGIN(JANT) = UT - DELTIM / 5.0D0
            TSTOP(JANT)  = TBEGIN(JANT) + DELTIM
            NEWDOP = 2
            END IF
         IF (UT.GE.TSTOP(JANT)) THEN
            TBEGIN(JANT) = TBEGIN(JANT) + DELTIM
            TSTOP(JANT)  = TSTOP(JANT)  + DELTIM
            NEWDOP = NEWDOP + 1
            GO TO 100
            END IF
C                                       Setup both boundaries
C                                       Determine day number
         IF ((.NOT.DOINT) .OR. (NEWDOP.GT.1)) THEN
            NDAY = IRDAY + TBEGIN(JANT)
            DTIME = TBEGIN(JANT) - (REAL(NDAY-IRDAY))
            CALL DOPLR (RAAPP, DECAPP, IIYEAR, NDAY, DTIME, DXANT,
     *         DYANT, DZANT, VSUN, VL)
            IF (VTYPE.EQ.2) CALL DOPLR (RAAPP, DECAPP, IIYEAR, NDAY,
     *         DTIME, 0.0D0, 0.0D0, 0.0D0, DZANT, VSUN, V0)
            STRTVL(JANT) = VL - V0
C
            NDAY = IRDAY + TSTOP(JANT)
            DTIME = TSTOP(JANT) - (REAL(NDAY-IRDAY))
            CALL DOPLR (RAAPP, DECAPP, IIYEAR, NDAY, DTIME, DXANT,
     *         DYANT, DZANT, VSUN, VL)
            IF (VTYPE.EQ.2) CALL DOPLR (RAAPP, DECAPP, IIYEAR, NDAY,
     *         DTIME, 0.0D0, 0.0D0, 0.0D0, DZANT, VSUN, V0)
            STOPVL(JANT) = VL - V0
C
            DELVEL = STOPVL(JANT) - STRTVL(JANT)
C                                       Calculate new doppler vels.
C                                       at interpolation boundaries
         ELSE IF (NEWDOP.NE.0) THEN
            STRTVL(JANT) = STOPVL(JANT)
C                                       Determine day number
            NDAY = IRDAY + TSTOP(JANT)
            DTIME = TSTOP(JANT) - (REAL(NDAY-IRDAY))
            CALL DOPLR (RAAPP, DECAPP, IIYEAR, NDAY, DTIME, DXANT,
     *         DYANT, DZANT, VSUN, VT)
            IF (VTYPE.EQ.2) CALL DOPLR (RAAPP, DECAPP, IIYEAR, NDAY,
     *         DTIME, 0.0D0, 0.0D0, 0.0D0, DZANT, VSUN, V0)
            STOPVL(JANT) = VT - V0
            DELVEL = STOPVL(JANT) - STRTVL(JANT)
            END IF
C                                       velocity
         DOPVEL = DOPVEL + (UT - TBEGIN(JANT)) * DELVEL / DELTIM  +
     *      STRTVL(JANT)
         IF (HELIO) DOPVEL = DOPVEL - VSUN
         END IF
      GO TO 999
C                                       Write error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CVLDOP: ANTENNA NUMBER ',I4,' OUTSIDE RANGE')
 1010 FORMAT ('CVLDOP: ERROR ',I3,' OBTAINING SOURCE INFO.')
      END
      SUBROUTINE PLAVEL (UT, PLVEL)
C-----------------------------------------------------------------------
C   PLAVEL interpolates in the planet table
C   Inputs:
C      UT      D   Time
C   Output:
C      PLVEL   D   Planet velocity
C-----------------------------------------------------------------------
      DOUBLE PRECISION UT, PLVEL
C
      INTEGER   I
      DOUBLE PRECISION LUT, LVEL
      INCLUDE 'PCVEL.INC'
      SAVE LUT, LVEL
      DATA LUT, LVEL /-1.D4, -1.D10/
C-----------------------------------------------------------------------
      IF (UT.GT.LUT) THEN
         LUT = UT
         IF (UT.LT.PVTIME(1)) THEN
            LVEL = PVVEL(1)
         ELSE IF (UT.GT.PVTIME(NPVVEL)) THEN
            LVEL = PVVEL(NPVVEL)
         ELSE
            DO 20 I = 1,NPVVEL-1
               IF ((UT.GE.PVTIME(I)) .AND. (UT.LE.PVTIME(I+1))) THEN
                  LVEL = PVVEL(I) + (UT - PVTIME(I)) * (PVVEL(I+1) -
     *               PVVEL(I)) / (PVTIME(I+1)-PVTIME(I))
                  GO TO 900
                  END IF
 20            CONTINUE
            END IF
         END IF
C                                       return velocity
 900  PLVEL = LVEL
C
 999  RETURN
      END
