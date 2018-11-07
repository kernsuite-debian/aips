LOCAL INCLUDE 'XYDIF.INC'
      INTEGER   MAXSOU, MAXCAL
      PARAMETER (MAXSOU = 100)
      PARAMETER (MAXCAL = 30)
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XCALCO*4, LPNAME*48,
     *   LINE*132, TITL1*132,TITL2*132, SCRTCH*132, CSNAME(MAXCAL)*16
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOURC(4,30), XXCALC, XLPNAM(12)
      DOUBLE PRECISION  FREQSO(MAXIF,MAXCAL), CALFRQ(MAXCHA,4)
      REAL      XSIN, XDISIN, XTIME(8), XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XBCHAN, XECHAN, XANT(50), XUVRA(2), XSUBA, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3),
     *   DOINV, DOAPLY, DOLINE, XINTP(3), DOCRT, XBADD(10),
     *   RPARM(20), VIS(3,MAXCIF), FRQCRP, FINC(MAXIF),
     *   CALQU(2,MAXCHA,4)
      LOGICAL   ISNXT, SINGLE
      INTEGER   DISKIN, SEQIN, CNOIN, BUFFER(512), LUNP, FINDP, NACROS,
     *   IPCNT, PAGE, IDOCRT, VSOUID(MAXSOU), VCALID(MAXCAL), NVCAL,
     *   VCALKK(MAXCAL), LBCHAN, LECHAN, NCAL(4)
C
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOURC, XXCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XBIF, XEIF, XBCHAN, XECHAN, XANT,
     *   XUVRA, XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH, DOINV, DOAPLY, DOLINE, XINTP, DOCRT,
     *   XLPNAM, XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN, XSOUR, XCALCO, LPNAME, LINE, TITL1,
     *   TITL2, SCRTCH, CSNAME
      COMMON /INFOLS/ FREQSO, CALFRQ, RPARM, VIS, ISNXT, DISKIN, SEQIN,
     *   CNOIN, BUFFER, NACROS, IPCNT, PAGE, LUNP, FINDP, IDOCRT, NVCAL,
     *   SINGLE, VSOUID, VCALID, VCALKK, FRQCRP, FINC, LBCHAN, LECHAN,
     *   CALQU, NCAL
LOCAL END
      PROGRAM XYDIF
C-----------------------------------------------------------------------
C! XYDIF returns the X - Y linear polarization phase difference
C# Calibration UV VLA VLB polarization
C-----------------------------------------------------------------------
C;  Copyright (C) 2018
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
C   Task XYDIF prints data from uv data files in a variety of forms.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAME          File name to be listed.
C      INCLASS        CLASS         File class to be listed.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volumn on which file resides.
C      SOURCES        XSOUR(4,30)   Sources selected
C      CALCODE        XCALCO        Calibrator source code
C      TIMERANG       XTIME(8)      Timerange
C      BIF            BIF           IF number
C      BCHAN          BCHAN         Channel number
C      ANTENNAS       XANT(50)      Antenna numbers
C      UVRANGE        UVRANG        Range of UV in 1000's wavelengths
C      SUBARRAY       SUBARR        Subarray
C      DOCALIB        DOCAL         Calibrate?
C      GAINUSE        GAUSE         CL version to apply.
C      FLAGVER        FGVER         Flag table version
C      DOBAND                       Bandpass calibrate?
C      BPVER                        BP table to apply
C      SMOOTH                       Smoothing function
C      DOCRT          DOCRT         > 0 => use CRT, else line printer
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PRGM*6
      INTEGER   IRET, IERR, IB, IE, NIF, NWORDS, NCH, NGOOD
      REAL      PDIF, CLCORP(20), PERR, PDCOR(2), PCLCOR(3*MAXIF)
      DOUBLE PRECISION DPDCOR(2)
      LONGINT   PPDCOR, PDPDC
      LOGICAL   NOW
      EQUIVALENCE (DPDCOR, PDCOR)
      INCLUDE 'XYDIF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'XYDIF '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL XYDIN (PRGM, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 990
         END IF
C                                       return quickly?
      CALL RFILL (20, 0.0, CLCORP)
      NOW = (DOAPLY.GT.0.0) .OR. (DOLINE.GT.0.0) .OR. (NVCAL.GT.1)
      NOW = NOW .AND. RQUICK
      IF (NOW) THEN
         CALL PTPARM (20, CLCORP, BUFFER, IERR)
         CALL RELPOP (IRET, BUFFER, IERR)
         END IF
      NGOOD = 0
C                                       List data - continuum method
C                                       can do > 1 cal source
      IF (DOLINE.LE.0.0) THEN
         NIF = EIF - BIF + 1
         IB = BIF
         IE = EIF
         CALL RFILL (3*MAXIF, 0.0, PCLCOR)
         DO 10 BIF = IB,IE
            WRITE (MSGTXT,1000) BIF
            CALL MSGWRT (2)
            CALL MATXUV (PDIF, PERR, IRET)
C            IF (IRET.NE.0) GO TO 990
            IF (IRET.EQ.0) THEN
               CLCORP(BIF-IB+1) = PDIF
               PCLCOR(BIF-IB+1) = PDIF
               PCLCOR(BIF-IB+1+NIF) = PERR
               PCLCOR(BIF-IB+1+2*NIF) = PERR
               NGOOD = NGOOD + 1
               END IF
 10         CONTINUE
         DO 20 BIF = IB,IE
            WRITE (MSGTXT,1010) BIF, CLCORP(BIF-IB+1),
     *         PCLCOR(BIF-IB+1+NIF)
            CALL MSGWRT (5)
 20         CONTINUE
C                                       Close printer
         IF (IDOCRT.NE.0) CALL LPCLOS (LUNP, FINDP, IPCNT, IERR)
C                                       apply
         IF ((DOAPLY.GT.0.0) .AND. (NGOOD.GT.0)) THEN
            FRQCRP = 1.0
            CALL CLAPPL (DISKIN, CNOIN, IB, IE, CLVER, SUBARR, PCLCOR,
     *         CATUV, IRET)
            CALL RFILL (20, 0.0, CLCORP)
         ELSE
            RQUICK = .FALSE.
            END IF
C                                       spectral line
      ELSE
         NIF = EIF - BIF + 1
         NCH = ECHAN - BCHAN + 1
         NWORDS = (11 * NIF * NCH * 2 + 4095) / 1024 + 1
         CALL ZMEMRY ('GET ', 'XYDIFL', NWORDS, PDCOR, PPDCOR, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'FAILED TO GET DYNAMIC MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         PDPDC = PPDCOR / 2
         CALL XYDLIN (NCH, NIF, DPDCOR(1+PDPDC), IRET)
         IF (IRET.NE.0) GO TO 990
         IF (DOCRT.GT.0.0) CALL XYDPRT (NCH, NIF, CSNAME, BIF, LBCHAN,
     *      LECHAN, CATUV, DOCRT, LPNAME, DPDCOR(1+PDPDC))
         CALL XYDSMT (NCH, NIF, LBCHAN, LECHAN, CATUV, XINTP,
     *      DPDCOR(1+PDPDC))
         IF (DOCRT.LT.0.0) CALL XYDPRT (NCH, NIF, CSNAME, BIF, LBCHAN,
     *      LECHAN, CATUV, DOCRT, LPNAME, DPDCOR(1+PDPDC))
         IF (DOAPLY.GT.0.0) THEN
            CALL BPAPPL (DISKIN, CNOIN, BCHAN, BIF, BPVER, PDVER,
     *         SUBARR, FRQSEL, NCH, NIF, DPDCOR(1+PDPDC), CATUV, IRET)
            END IF
         CALL RFILL (20, 0.0, CLCORP)
         END IF
C                                       return adverbs
      IF (.NOT.RQUICK) CALL PTPARM (20, CLCORP, BUFFER, IERR)
C                                       History file ??
      IF (IRET.EQ.0) CALL XYDHI (NCH, NIF, BIF, EIF, CLCORP, PCLCOR)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Doing IF',I3)
 1010 FORMAT ('IF',I3,' avg XY phase',F8.3,' +-',F8.3)
      END
      SUBROUTINE XYDIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   XYDIN gets input parameters for XYDIF
C   Inputs:
C      PRGN    C*6       Program name (2 chars/word)
C   Output:
C      IRET    I         Error code: 0 => ok
C                           5 => catalog troubles
C                           8 => can't start
C   Commons:
C      /INPARM/ all input adverbs in order given by INPUTS file
C      /MAPHDR/ output file catalog header
C   See prologue comments in XYDIF for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER STAT*4, STATUS*4, UTYPE*2
      INTEGER   IROUND, NPARM, IERR, I, LUN, NIF, IIVER, J
      REAL      CATR(256), DUM
      LOGICAL   F, TABLE, EXIST, FITASC, MATCH
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISBAND(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF), BFREQ
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'XYDIF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATBLK)
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 237
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF ((IERR.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      RQUICK = RQUICK .AND. (DOCRT.LE.0.0)
      IF (IERR.NE.0) THEN
         RQUICK = .FALSE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (IRET.NE.0) GO TO 999
      IRET = 5
      IF (DOINV.GT.0.0) THEN
         DOINV = 180.0
      ELSE
         DOINV = 0.0
         END IF
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
C                                       Calcode abreviations
      IF (XCALCO(1:2).EQ.'CA') XCALCO = '*   '
      IF (XCALCO(1:1).EQ.'-')  XCALCO = '-CAL'
      SELCOD = XCALCO
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XSOURC(1,I), XSOUR(I))
 25      CONTINUE
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      IDOCRT = IROUND (DOCRT)
      IF (DOCRT.GT.0.0) IDOCRT = MAX (1, IDOCRT)
C                                       Get CATBLK.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       READ or WRIT cause trouble
      STATUS = 'REST'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STATUS, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Mark in CFILES
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      BFREQ = FREQ
      FRQCRP = CATR(KRCRP+JLOCF)
      SINGLE = ILOCSU.LT.0
C                                       Check sort order
      IF (ISORT(1:1).NE.'T') THEN
         MSGTXT = 'YOUR DATA ARE NOT IN T* ORDER, USE UVSRT'
         IRET = 1
         GO TO 990
         END IF
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 70 I = 1,30
         SOURCS(I) = XSOUR(I)
 70      CONTINUE
      CALL RCOPY (8, XTIME, TIMRNG)
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (DOLINE.GT.0.0) THEN
         LBCHAN = BCHAN
         LECHAN = ECHAN
         BCHAN = 1
         ECHAN = CATBLK(KINAX+JLOCF)
         END IF
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
         IF ((EIF-BIF.GT.19) .AND. (DOAPLY.LE.0.0) .AND.
     *      (DOLINE.LE.0.0)) THEN
            MSGTXT = 'EIF LIMITED TO BIF+19 BY CLCORPRM'
            CALL MSGWRT (7)
            EIF = BIF+19
            END IF
      ELSE
         BIF = 1
         EIF = 1
         END IF
C                                       Antennas
      DO 60 I = 1,50
         ANTENS(I) = IROUND (XANT(I))
 60      CONTINUE
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      IF (DOPOL.LE.0) THEN
         MSGTXT = '**************************************************'
         CALL MSGWRT (7)
         MSGTXT = '*** WARNING DOPOL FALSE - NOT THE NORMAL VALUE ***'
         CALL MSGWRT (7)
         MSGTXT = '**************************************************'
         CALL MSGWRT (7)
         END IF
      PDVER = IROUND (XPDVER)
      DOAPPL = F
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DO 80 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 80      CONTINUE
      IRET = 0
C                                       List U, V
      STOKES = 'IQUV'
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                       Get IF freq offset.
      LUN = 28
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
      IF (IERR.GT.0) GO TO 999
C                                       See if index table
      CALL ISTAB ('NX', DISKIN, CNOIN, 1, LUN, BUFFER, TABLE, EXIST,
     *   FITASC, IERR)
      ISNXT = (.NOT.EXIST) .OR. (IERR.NE.0)
C                                       check UV data set exists,
C                                       set variables for PCLSOU
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVGET ('CLOS', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL FILL (MAXSOU, -1, VSOUID)
      IF (SINGLE) THEN
         NVCAL = 1
         VCALID(1) = 1
         VSOUID(1) = 1
         CALL H2CHR (8, 1, CATUV(KHOBJ), CSNAME(1))
         CALL DFILL (NIF, 0.0D0, FREQSO)
      ELSE
         CALL XYDSOU (IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      DO 90 I = 1,NVCAL
         DO 85 J = 1,NUMIF
            FREQSO(J,I) = FREQSO(J,I) + BFREQ + FOFF(J)
 85         CONTINUE
 90      CONTINUE
      IF (NVCAL.LE.0) THEN
         MSGTXT = 'YOU MUST REQUEST SOME SOURCE!'
         IRET = 10
         GO TO 990
         END IF
      IF ((NVCAL.GT.1) .AND. (DOLINE.LE.0.0) .AND. (DOAPLY.LE.0.0)) THEN
         MSGTXT = 'CLCORPRM METHOD REQUIRES THERE TO BE ONLY 1 SOURCE'
         IRET = 10
         GO TO 990
         END IF
      IF (DOAPLY.LE.0.0) IRET = 0
      IF (IRET.NE.0) GO TO 999
C                                       Init printer
      PAGE = 0
      IPCNT = 980
      TITL1 = ' '
      TITL2 = ' '
      LINE = ' '
C                                       Open output device
      IF ((IDOCRT.NE.0) .AND. (DOLINE.LE.0.0)) THEN
         IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
         CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET
            IRET = 1
            GO TO 990
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XYDIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1080 FORMAT ('XYDIN: ERROR ',I3,' OPENING OUTPUT ''PRINT'' DEVICE')
      END
      SUBROUTINE XYDSOU (IRET)
C-----------------------------------------------------------------------
C   Gets source info
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'XYDIF.INC'
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   I, SUKOLS(MAXSUC), SUNUMV(MAXSUC), IDSOU, QUAL, SULUN,
     *   ISURNO, NUMSOU, LOOP, SUFQID
      DOUBLE PRECISION BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF), RAOBS,
     *   DECOBS
      REAL     FLUX(4,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA SULUN /27/
C-----------------------------------------------------------------------
      NVCAL = 0
C                                       Open source (SU) table
      CALL SOUINI ('READ', NXBUFF, DISKIN, CNOIN, 1, CATUV, SULUN,
     *   NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Check FREQID compatibility.
      IF ((SUFQID.GT.0) .AND. (FRQSEL.GT.0) .AND. (SUFQID.NE.FRQSEL))
     *   THEN
         MSGTXT = 'WARNING - POTENTIALLY FATAL ERROR'
         CALL MSGWRT (6)
         WRITE (MSGTXT,1040) SUFQID
         CALL MSGWRT (6)
         WRITE (MSGTXT,1050) FRQSEL
         CALL MSGWRT (6)
         MSGTXT = '   Suggest you rerun SETJY with the correct FREQID'
         CALL MSGWRT (6)
         IRET = 5
         GO TO 999
         END IF
C                                       Get number of sources
      NUMSOU = NXBUFF(5)
C                                       Read flux array
      DO 50 LOOP = 1,NUMSOU
         ISURNO = LOOP
         CALL TABSOU ('READ', NXBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Save calibrator fluxes
C                                       See if wanted
         IF (NSOUWD.GT.0) THEN
            DO 20 I = 1,NSOUWD
               IF (IDSOU.EQ.SOUWAN(I)) GO TO 30
 20            CONTINUE
C                                       Not wanted
            GO TO 50
            END IF
C                                       Wanted
 30      IF (NVCAL.LT.MAXCAL) THEN
            NVCAL = NVCAL + 1
            VCALID(NVCAL) = IDSOU
            CSNAME(NVCAL) = SOUNAM
            VSOUID(IDSOU) = NVCAL
            DO 35 I = 1,NUMIF
               FREQSO(I,NVCAL) = FREQO(I)
 35            CONTINUE
C                                       Too many calibrators
         ELSE
            IRET = 10
            WRITE (MSGTXT,1035) MAXCAL
            GO TO 990
            END IF
 50      CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ISURNO, NXBUFF, NXBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1070) IRET
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XYDSOU: ERROR',I3,' OPENING SOURCE TABLE')
 1010 FORMAT ('XYDSOU: ERROR',I3,' READING SOURCE TABLE')
 1040 FORMAT ('   Your calibrators have their fluxes set for FQID ',I3)
 1050 FORMAT ('   You are using them to calibrate FQID ',I3)
 1035 FORMAT ('XYDSOU: ERROR: TOO MANY CALIBRATORS, >',I4)
 1070 FORMAT ('XYDSOU: ERROR',I3,' CLOSING SOURCE TABLE')
      END
      SUBROUTINE MATXUV (PDIF, PERR, IRET)
C-----------------------------------------------------------------------
C   Gives matrix listings of scan averaged uv data.
C   Info for UVGET is set in XYDIN.
C   Output:
C      PDIF   R   R-L phase difference
C      PERR   R   Uncertainty
C      IRET   I   Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
      REAL      PDIF, PERR
C
      CHARACTER ENTRY*20, FRMAT(10)*5, FORM*5, CHLINE*132, CCODE*1,
     *   POLA1*3, POLA2*3, PART(2)*5
      INTEGER   TIME(8), NANT, NPASS, IPASS, IANTHI, IANTLO,
     *   NANTPP, IROW, ICOL, NCOLPV, COLPNT, IATY, IANT, MANT, I, J,
     *   IERR, I4TEMP, KQUAL, KANT, ISCAN, JSCAN
      LOGICAL   DONE
      REAL      CATR(256), SFACT, DT, AVGS(2,2), SCNAVG(200),
     *   SCNSIG(200)
      DOUBLE PRECISION    CATD(128), SFREQ, XV(2), SA(2), SW, W, SS(2),
     *   AVG, SIG(2)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ANTLAB(MAXANT), IROUND
      REAL      SCANV(MAXANT,MAXANT), SCNMIN
      INCLUDE 'XYDIF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA FRMAT /'(I1)','(I2)','(I3)','(I4)','(I5)','(I6)','(I7)',
     *   '(I8)','(I9)','(I10)'/
      DATA PART /'upper','lower'/
C-----------------------------------------------------------------------
C                                       Setup
      SCNMIN = 1.E10
      EIF = BIF
      IATY = 2
      ISCAN = 0
      IF (IPCNT.NE.980) IPCNT = 990
C                                       Single source default = 10 min.
      IF (ISNXT) THEN
         DT = 10.0 / 1440.0
C                                       Multisource DT default = scan
      ELSE
         DT = 1.0E10
         END IF
C                                       Number of char per col.
      NCOLPV = 4
      FORM = FRMAT(NCOLPV)
C                                       Init. vis record
      DO 10 I = 1,20
         RPARM(I) = 0.0
 10      CONTINUE
      RPARM(1) = FBLANK
C                                       Open uv data etc.
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IRET
         GO TO 990
C                                       Antenna list
 20   IANT = 1
      MANT = 1
      IF ((NANTSL.GT.0) .AND. (.NOT.DOAWNT)) MANT = NANTSL
      DO 30 I = 1,MAXANT
C                                       Search list of deselected
C                                       antennas.
         DO 25 J = 1,MANT
            IF ((.NOT.DOAWNT) .AND. (ABS(ANTENS(J)).EQ.I)) GO TO 30
 25         CONTINUE
         ANTLAB(IANT) = I
         IANT = IANT + 1
 30      CONTINUE
      IF ((NANTSL.GT.0) .AND. DOAWNT) CALL COPY (NANTSL, ANTENS, ANTLAB)
      FREQ = FREQ * 1.0D-9
C                                       first page titles
      IF ((IDOCRT.NE.0) .AND. (DOCRT.GT.-2.5)) THEN
         IF (NACROS.GE.90) THEN
            WRITE (LINE,1040) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER,
     *         BCHAN, BIF
         ELSE
            WRITE (LINE,1041) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER,
     *         BCHAN, BIF
            END IF
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         WRITE (LINE,1042) FREQ, NCOR, NVIS
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         WRITE (LINE,1043) STOKES, SUBARR
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         IF (DOCAL) THEN
            WRITE (LINE,1050) CLUSE
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         IF (DOPOL.GT.0) THEN
            LINE = 'Applying polarization corrections'
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         IF (DOBL) THEN
            WRITE (LINE,1051) BLVER
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         IF (DOFLAG) THEN
            WRITE (LINE,1052) FGVER
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         IF (DOBAND.GT.0) THEN
            WRITE (LINE,1053) BPVER
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 950
            END IF
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 950
         END IF
C                                       While data, Loop thru scans
 100  CALL SCANAV (DT, SCANV, TIME, NANT, RPARM, VIS, AVGS, IRET)
         DONE = IRET.LT.0
         IF (IRET.GT.0) GO TO 999
C                                       If calc code OK
         CCODE = CALCOD(1:1)
         IF ((XCALCO(:1).EQ.' ') .OR. (CCODE.EQ.XCALCO(:1)) .OR.
     *      ((XCALCO(:1).EQ.'*') .AND. (CCODE.NE.' ')) .OR.
     *      ((XCALCO(:1).EQ.'-') .AND. (CCODE.EQ.' ') .AND.
     *      (NANT.GT.0))) THEN
C                                       Determine number of passes
            NPASS = ((1.0 * NCOLPV * NANT) / (NACROS - NCOLPV)) + 0.999
            NANTPP = (NACROS - 5) / NCOLPV
C                                       Find max value
            CALL SCNMAX (SCANV, NANT, SFACT, AVG, SIG)
            AVG = AVG + DOINV
            IF (ISCAN.LT.200) THEN
               ISCAN = ISCAN + 1
               SCNAVG(ISCAN) = AVG
               SCNSIG(ISCAN) = SIG(1)
               SCNMIN = MIN (SCNMIN, SCNSIG(ISCAN))
               END IF
C                                       Header for scan (2 blank lines
C                                       first - first pass only)
            IF (IDOCRT.NE.0) THEN
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
               KQUAL = QUAL
               WRITE (LINE,1100) TIME, SNAME, KQUAL
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *            TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
C                                       Source info
               I = VSOUID(IDSOUR)
               SFREQ = FREQSO(BIF,I) * 1.0D-9
               IF (DOCRT.GT.-2.5) THEN
                  WRITE (LINE,1101) FLUX(1,BIF), CALCOD, SFREQ, BIF
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  END IF
C                                       Type of data listed
               WRITE (LINE,1111) SFACT, 1000.0/SFACT
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
C                                       Polarizations
               WRITE (LINE,1112) BIF
               POLA1 = 'V'
               POLA2 = 'U'
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
               DO 600 IPASS = 1,NPASS
                  IANTLO = (IPASS - 1) * NANTPP + 1
                  IANTHI = IANTLO + NANTPP - 1
                  IANTHI = MIN (IANTHI, NANT)
C                                       Section label
                  LINE = ' '
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
                  KQUAL = QUAL
                  WRITE (TITL1,1100) TIME, SNAME, KQUAL
                  TITL2 = 'Ant'
                  COLPNT = 5
                  DO 150 ICOL = IANTLO,IANTHI
                     WRITE (ENTRY,1120) ANTLAB(ICOL)
                     TITL2(COLPNT:COLPNT+NCOLPV-1) = ENTRY(:NCOLPV)
                     COLPNT = COLPNT + NCOLPV
 150                 CONTINUE
                  IF (((IPCNT.GT.3) .AND. (IPCNT.LT.(PRTMAX-1))) .OR.
     *               (DOCRT.LE.-2.5)) THEN
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, TITL2, IPCNT, PAGE,SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
                     END IF
                  KANT = NANT
                  DO 400 IROW = 1,KANT
                     IF (IROW.LE.NANT) THEN
                        WRITE (LINE,1150) ANTLAB(IROW)
                     ELSE
                        IF (IROW.EQ.NANT+1) THEN
                           WRITE (LINE,1160) POLA1
                        ELSE
                           WRITE (LINE,1170) POLA2
                           END IF
                        END IF
                     COLPNT = 5
                     DO 200 ICOL = IANTLO,IANTHI
                        IF (IROW .LE. NANT) THEN
                           IF (SCANV(IROW,ICOL).NE.FBLANK) THEN
                              I4TEMP = IROUND (SFACT * SCANV(IROW,ICOL))
                              WRITE (CHLINE,FORM) I4TEMP
                              LINE(COLPNT:COLPNT+NCOLPV-1) =
     *                           CHLINE(:NCOLPV)
                              END IF
                           END IF
                        COLPNT = COLPNT + NCOLPV
 200                    CONTINUE
C                                       Write row
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 950
 400                 CONTINUE
C                                       Blank titles
                  TITL1 = ' '
                  TITL2 = ' '
 600              CONTINUE
C                                       Give matrix avg, sigma
               WRITE (LINE,1600) AVG, SIG(1)
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 950
               DO 610 I = 1,2
                  WRITE (LINE,1601) PART(I), AVGS(1,I)*SFACT,
     *               AVGS(2,I)*SFACT
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 950
 610              CONTINUE
C                                       End If ok to print
               END IF
            END IF
C                                       end while more data - loop
         IF (.NOT.DONE) GO TO 100
C                                       get average phase difference
      IF (ISCAN.LE.0) THEN
         MSGTXT = 'No phase difference returned'
         CALL MSGWRT (6)
         PDIF = 0.0
         PERR = 0.0
      ELSE IF (ISCAN.EQ.1) THEN
         PDIF = SCNAVG(1)
         PERR = SCNSIG(1)
      ELSE
         JSCAN = 0
         SCNMIN = 3.0 * MAX (SCNMIN, 0.1)
         AVG = 0.0
         SIG(1) = 1.E8
C                                       first average
 700     SW = 0.0D0
         SA(1) = 0.0D0
         SS(1) = 0.0D0
         SA(2) = 0.0D0
         SS(2) = 0.0D0
         J = 0
         DO 720 I = 1,ISCAN
            IF ((JSCAN.EQ.0) .OR. (ABS(SCNAVG(I)-AVG).LT.SIG(1))) THEN
               W = 1.0D0
               IF (SCNSIG(I).NE.0.0) W = 1.0D0 / SCNSIG(I)**2
               J = J + 1
               SW = SW + W
               XV(1) = COS (SCNAVG(I)/RAD2DG)
               XV(2) = SIN (SCNAVG(I)/RAD2DG)
               SA(1) = SA(1) + W * XV(1)
               SA(2) = SA(2) + W * XV(2)
               SS(1) = SS(1) + W * XV(1) * XV(1)
               SS(2) = SS(2) + W * XV(2) * XV(2)
               END IF
 720        CONTINUE
         IF (SW.GT.0) THEN
            XV(1) = SA(1) / SW
            XV(2) = SA(2) / SW
C            AVG = ATAN2 (XV(2), (XV(1)+1.E-20)) * RAD2DG
            AVG = ATAN (XV(2)/(XV(1)+1.E-20)) * RAD2DG + DOINV
            SS(1) = SS(1) / SW
            SS(2) = SS(2) / SW
            SS(1) = SS(1) - XV(1)*XV(1)
            SS(2) = SS(2) - XV(2)*XV(2)
            SS(1) = MAX (0.0D0, SS(1))
            SS(2) = MAX (0.0D0, SS(2))
            SIG(1) = MAX (SS(1), SS(2))
            SIG(1) = 3.0 * SQRT (SIG(1)) * RAD2DG
            IF (J.NE.JSCAN) THEN
               IF (SIG(1).LT.SCNMIN) SIG(1) = SCNMIN
               JSCAN = J
               GO TO 700
               END IF
            SIG(1) = SIG(1) / 3.0
            IF (SIG(1).LT.SCNMIN/3.0) SIG(1) = SCNMIN/3.0
            PDIF = AVG
            PERR = SIG(1) / SQRT (SW)
         ELSE
            MSGTXT = 'ERROR IN SELF-CONSISTENT AVERAGING'
            CALL MSGWRT (6)
            MSGTXT = 'No phase difference returned'
            CALL MSGWRT (6)
            PDIF = 0.0
            PERR = 0.0
            END IF
         END IF
C                                       CRT error
 950  IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1950) IERR
         CALL MSGWRT (8)
         IRET = 1
      ELSE
         IRET = 0
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MATXUV: ERROR',I4,' DETERMINING UV-FILE PARAMETERS')
 1040 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,'    Userid =',
     *   I5,5X,'Channel =',I5,' IF =',I5)
 1041 FORMAT (A12,'.',A6,'.',I4,'  Vol=',I2,'  User=',I5,'  Channel=',
     *   I4,'  IF=',I4)
 1042 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10)
 1043 FORMAT ('Stokes = ',A4,' Subarray = ',I3)
 1050 FORMAT ('Applying calibration table ',I3)
 1051 FORMAT ('Applying baseline table ',I3)
 1052 FORMAT ('Applying flag table ',I3)
 1053 FORMAT ('Applying bandpass table ',I3)
 1100 FORMAT ('Time =',I4,'/',2(I2.2,':'),I2.2,' to',I4,'/',2(I2.2,':'),
     *   I2.2,'   Source = ',A16,':',I5.4)
 1101 FORMAT ('Flux =',F8.4,' Jy, Calcode = ',A4,', Freq =',F13.9,
     *   ' GHz, IF =',I3)
 1111 FORMAT ('Real parts scaled by',1PE8.1,' -> 1000 =',0PF10.4,' Jy')
 1112 FORMAT ('IF=',I3,4X,'V in upper right, U in lower left')
 1120 FORMAT ('--',I2,'------')
 1150 FORMAT (I3,'|',128X)
 1160 FORMAT (A3,'|',128X)
 1170 FORMAT (A3,'|',128X)
 1600 FORMAT ('Average phase corr of matrix  = ',F8.2,'(',F7.3,
     *   ')')
 1601 FORMAT ('Average of ',A,' data = ',F8.2,9X,' sigma =',F7.3)
 1950 FORMAT ('MATXUV: ERROR',I5,' DOING I/O TO TERMINAL')
      END
      SUBROUTINE SCANAV (DT, SCANV, TIME, NANT, RPARM, VIS, AVGS, IERR)
C-----------------------------------------------------------------------
C   Reads an indexed uv data base and returns scan averages of amp,
C   phase or the RMS scatter.  Needs to be initialized by a call to
C   UVGET.  The  order of the antennas returned in SCANV is defined by
C   the order in the common array ANTENS unless all antennas were
C   specified, (NANTSL = 0).
C      IF 2 polarizations were specified to UVGET then they will be
C   returned in the two halves of SCANV.
C   Inputs:
C     DT         R    Maximum length integration in days.
C   Input/Output:
C     RPARM(20)  R    Random parameter array, first record of call.
C                     (1) = 'INDE' => don't use.
C     VIS(*)     R    Visibility array, first record of call.
C   Outputs:
C     SCANV(maxant,maxant) R   The result for antennas I<J,
C                     (I,J,*) = first polarization U
C                     (J,I,*) = second polarization V
C                     Undefined values will contain 'INDE'.
C                     Note: maxant is defined in the parameter include
C                     INCS:PUVD.INC.  Third dimension used for first and
C                     second data types (e.g. amp. + rms).
C     TIME(8)    I    Time range, start, stop; days, hours, min, sec.
C     NANT       I    Highest antenna number encountered.
C                     Actually highest index in SCANV
C     AVGS(2,2)  R    Average & rms for the type done in the AMPVEC
C                     style
C     IERR       I    Return code, 0 => OK, -1 => out of data,
C                     > 0 => failed.
C   Output to common in D/CSOU.INC
C     SNAME      C    Source name (16 char)
C     QUAL       I    Source qualifier.
C     CALCOD     R    Calibrator code 4 char.
C     FLUX(4,IF) R    Total flux density I, Q, U, V pol, (Jy) each IF
C     FREQO(IF)  D    Frequency offset (Hz)
C   Related values in common:
C     NANTSL     I    Number of antennas selected, 0 > all.
C     ANTENS(*)  I    Antenna numbers in order used in SCANV.
C   Notes:  The values of MVIS, EIF and ECHAN may be changed in the
C   UVGET commons to insure that only a single visibility is returned.
C      If the end of data is encountered (IERR=-1) then UVGET is called
C   with OPCODE='CLOS'.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      REAL     DT, SCANV(MAXANT,MAXANT), RPARM(*), VIS(3,*),
     *   AVGS(2,2)
      INTEGER   TIME(8), NANT, IERR
C
      REAL      T1, T2, SQRT, CATUV4(128), PFACT, TEMP, CATR(256)
      DOUBLE PRECISION CATD(128)
      INTEGER   MXANT, I, J, IA1, IA2, JA1, JA2, KA1, KA2, SUNUM, LIMIT,
     *   LIMIT1, JERR, NNANT, ISLUN, IANT, IPOFF, SCANUM, IVIS, KVIS,
     *   NPOL, INX
      LOGICAL   ORDER, DONE1, GOTDAT
      INTEGER   ENSANT(MAXANT), NC(2)
      DOUBLE PRECISION WORK(MAXANT,MAXANT,6), COUNT(MAXANT,MAXANT,4), W,
     *   SR(2), SI(2), SW(2), SSR(2), RP, RP2, W2, WT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      EQUIVALENCE (CATUV, CATUV4)
      DATA ISLUN /26/
      DATA MXANT /MAXANT/
C-----------------------------------------------------------------------
C                                       getting both CROS and AUTO
      DOACOR = .TRUE.
      DOXCOR = .TRUE.
C                                       See if first record read
      DONE1 = RPARM(1).NE.FBLANK
      GOTDAT = .FALSE.
C                                       Polarization
      NPOL = CATBLK(KINAX+JLOCS)
      IPOFF = 3
      PFACT = -1.0
      KVIS = (LREC-NRPARM) / 3
      KVIS = KVIS / NPOL
C                                       Clear arrays
 10   ORDER = NANTSL.GT.0
      NANT = NANTSL
      IF ((NANT.LE.0) .OR. (.NOT.DOAWNT)) NANT = MXANT
      NNANT = NANTSL
      DO 50 I = 1,NANT
         DO 50 J = 1,NANT
            COUNT(I,J,1) = 0.0D0
            COUNT(I,J,2) = 0.0D0
            COUNT(I,J,3) = 0.0D0
            COUNT(I,J,4) = 0.0D0
            WORK(I,J,1) = 0.0D0
            WORK(I,J,2) = 0.0D0
            WORK(I,J,3) = 0.0D0
            WORK(I,J,4) = 0.0D0
            WORK(I,J,5) = 0.0D0
            WORK(I,J,6) = 0.0D0
            SCANV(I,J) = FBLANK
 50         CONTINUE
C                                       Get address array for antennas
      CALL FILL (MXANT, 1, ENSANT)
      NC(1) = 0
      NC(2) = 0
C                                       Antennas selected
      DO 60 I = 1,NANT
         J = ABS(ANTENS(I))
         IF (J.GT.0) ENSANT(J) = I
 60      CONTINUE
      IF (DOAWNT .OR. (NANTSL.LE.0)) GO TO 90
C                                       Antennas deselected
      IANT = 1
      DO 80 I = 1,NANT
         DO 70 J = 1,NANTSL
            IF (ABS(ANTENS(J)).EQ.I) GO TO 80
 70         CONTINUE
C                                       Add antenna I to list.
         ENSANT(I) = IANT
         IANT = IANT + 1
 80      CONTINUE
C                                       Do one IF at a time
 90   EIF = BIF
C                                       Initialize time
      T1 = 1.0E10
      T2 = 1.0E10
C                                       Save scan number (0= no index)
      SCANUM = INXRNO
C                                       Loop reading data
 100     IF (.NOT.DONE1) CALL UVGET ('READ', RPARM, VIS, IERR)
         IF (IERR.GT.0) GO TO 999
         DONE1 = .FALSE.
C                                       Check if scan done
         IF ((INXRNO.GT.SCANUM) .OR. (IERR.LT.0) .OR.
     *      (RPARM(1+ILOCT).GT.(T1+DT))) GO TO 500
C                                       Antenna numbers
         IF (ILOCB.GE.0) THEN
            JA1 = RPARM(ILOCB+1) / 256. + 0.1
            JA2 = RPARM(ILOCB+1) - JA1 * 256 + 0.1
         ELSE
            JA1 = RPARM(ILOCA1+1) + 0.1
            JA2 = RPARM(ILOCA2+1) + 0.1
            END IF
         KA1 = JA1
         KA2 = JA2
         IF (ORDER) KA1 = ENSANT(JA1)
         IF (ORDER) KA2 = ENSANT(JA2)
         IA1 = MIN (KA1, KA2)
         IA2 = MAX (KA1, KA2)
         NNANT = MAX (NNANT, IA2)
         GOTDAT = .TRUE.
C                                       Time
         T2 = RPARM(ILOCT+1)
         IF (T1.GT.1.0E5) T1 = T2
C                                       Source no.
         SUNUM = CURSOU
C                                       Vector average:
C                                       Accumulate 1 st. poln.
         INX = IPOFF
         RP = 0.0D0
         W = 0.0D0
         RP2 = 0.0D0
         W2 = 0.0D0
         DO 120 IVIS = 1,KVIS
            WT = VIS(3,INX)
            IF (WT.GT.0.0D0) THEN
               RP = RP + VIS(1,INX) * WT
               W = W + WT
               END IF
C                                       Accumulate 2 nd. poln.
            WT = VIS(3,INX+1)
            IF (WT.GT.0.0D0) THEN
               RP2 = RP2 + VIS(1,INX+1) * WT
               W2 = W2 + WT
               END IF
            INX = INX + NPOL
 120        CONTINUE
         IF ((W.GT.0.0D0) .AND. (IA1.NE.IA2)) THEN
            RP = RP / W
            COUNT(IA1,IA2,1) = COUNT(IA1,IA2,1) + W
            COUNT(IA1,IA2,3) = COUNT(IA1,IA2,3) + W**2
            WORK(IA1,IA2,1) = WORK(IA1,IA2,1) + W * RP
            WORK(IA1,IA2,2) = WORK(IA1,IA2,2) + W * RP * RP
            END IF
C                                       Accumulate 2 nd. poln.
         IF ((W2.GT.0.0D0) .AND. (IA1.NE.IA2)) THEN
            RP = RP2 / W2
            W = W2
            COUNT(IA1,IA2,2) = COUNT(IA1,IA2,2) + W
            COUNT(IA1,IA2,4) = COUNT(IA1,IA2,4) + W**2
            WORK(IA1,IA2,3) = WORK(IA1,IA2,3) + W * RP
            WORK(IA1,IA2,4) = WORK(IA1,IA2,4) + W * RP * RP
            END IF
         GO TO 100
C                                       Scan done
C                                       See if have any data.
 500  IF ((.NOT.GOTDAT) .AND. (IERR.EQ.0)) GO TO 10
      IF (.NOT.GOTDAT) GO TO 800
      NANT = NNANT
      SR(1) = 0.0D0
      SR(2) = 0.0D0
      SW(1) = 0.0D0
      SW(2) = 0.0D0
      SSR(1) = 0.0D0
      SSR(2) = 0.0D0
C                                       Vector averaging
      LIMIT = NANT - 1
      DO 560 I = 1,LIMIT
         LIMIT1 = I + 1
         DO 550 J = LIMIT1,NANT
C                                       real part - upper right
            IF (COUNT(I,J,1).GT.0.0D0) THEN
               SCANV(I,J) = WORK(I,J,1) / COUNT(I,J,1)
               SR(1) = SR(1) + WORK(I,J,1)
               SSR(1) = SSR(1) + WORK(I,J,2)
               SW(1) = SW(1) + COUNT(I,J,1)
               NC(1) = NC(1) + 1
               END IF
C                                       real part - lower left
            IF (COUNT(I,J,2).GT.0.0D0) THEN
               SCANV(J,I) = WORK(I,J,3) / COUNT(I,J,2)
               SR(2) = SR(2) + WORK(I,J,3)
               SSR(2) = SSR(2) + WORK(I,J,4)
               SW(2) = SW(2) + COUNT(I,J,2)
               NC(2) = NC(2) + 1
               END IF
 550        CONTINUE
 560     CONTINUE
C                                       average matrix averages
      CALL RFILL (4, 0.0, AVGS)
      DO 570 I = 1,2
         IF (SW(I).GT.0.0D0) THEN
            SR(I) = SR(I) / SW(I)
            SSR(I) = SSR(I) / SW(I)
            AVGS(1,I) = SR(I)
            SSR(I) = SSR(I) - SR(I)*SR(I)
            TEMP = (SR(I)*SR(I) + SI(I)*SI(I)) ** 2
            IF (SR(I).NE.0.0D0) THEN
               AVGS(2,I) = SQRT (MAX (0.0D0, SSR(I)))
               IF (NC(I).GT.1) AVGS(2,I) = AVGS(2,I) / SQRT (NC(I)-1.0)
            ELSE
               AVGS(2,I) = 999.0
               END IF
            END IF
 570     CONTINUE
C                                       average matrix averages
C                                       Get source info
      CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
C                                       Didn't find source
      IF (JERR.EQ.11) THEN
         WRITE (MSGTXT,1750) SUNUM
         CALL MSGWRT (8)
         JERR = 0
      ELSE IF (JERR.GT.0) THEN
         IERR = JERR
         WRITE (MSGTXT,1700) JERR
         GO TO 990
         END IF
C                                       Time to Days Hours Mins Secs
 800  IF (T1.LT.1000.) THEN
         CALL TODHMS (T1, TIME(1))
         CALL TODHMS (T2, TIME(5))
         END IF
C                                       If end of data, close UVGET
      IF (IERR.LT.0) CALL UVGET ('CLOS', RPARM, VIS, JERR)
      IF (JERR.NE.0) IERR = JERR
      IF ((IERR.LT.0) .AND. (.NOT.GOTDAT)) THEN
         IERR = 1
         MSGTXT = 'NO DATA FOUND'
         CALL MSGWRT (8)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1700 FORMAT ('SCANAV: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('SCANAV: SOURCE ',I3,' NOT IN SU TABLE')
      END
      SUBROUTINE SCNMAX (SCANV, NANT, SFACT, AVG, SIG)
C-----------------------------------------------------------------------
C   Routine to find the maximum, non blank value in an array and
C   determine the proper scaling factor for printing.
C   Inputs:
C      SCANV(maxant,maxant)  R    Scan values.
C      NANT                  I    Max. antenns number in scan.
C   Output:
C      SFACT                 R    Scaling factor to print values.
C      AVG                   R    Matrix average ATAN(V,U)
C      SIG(2)                R    Sigma of average, of matrix
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      REAL      SCANV(MAXANT,MAXANT), SFACT
      INTEGER   NANT
      DOUBLE PRECISION AVG, SIG(2)
C
      INTEGER   I, J, ICNT
      REAL      SQRT, TEMP, XV(2), XAVG, SMAX, SMIN
      DOUBLE PRECISION XS1(2), XS2(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Find maximum non blank and do
C                                       sums for average, sigma
      ICNT = 0
      AVG = 0.0D0
      XAVG = 0.0
      SIG(1) = 0.0D0
      SIG(2) = 0.0D0
      XV(1) = 0.0
      XV(2) = 0.0
      XS1(1) = 0.0D0
      XS1(2) = 0.0D0
      XS2(1) = 0.0D0
      XS2(2) = 0.0D0
      SFACT = 1.0
      IF (NANT.LE.0) GO TO 999
      SMAX = -1.E10
      SMIN = 1.E10
      DO 100 I = 1,NANT-1
         DO 90 J = I+1,NANT
            IF ((SCANV(I,J).NE.FBLANK) .AND. (SCANV(J,I).NE.FBLANK))
     *         THEN
               ICNT = ICNT + 1
               XV(1) = SCANV(I,J)
               XV(2) = SCANV(J,I)
               SMAX = MAX (SMAX, SCANV(I,J))
               SMAX = MAX (SMAX, SCANV(J,I))
               SMIN = MIN (SMIN, SCANV(I,J))
               SMIN = MIN (SMIN, SCANV(J,I))
               XS1(1) = XS1(1) + XV(1)
               XS1(2) = XS1(2) + XV(2)
               XS2(1) = XS2(1) + XV(1) * XV(1)
               XS2(2) = XS2(2) + XV(2) * XV(2)
               END IF
 90         CONTINUE
 100     CONTINUE
      IF (ICNT.LE.0) GO TO 999
      SMAX = MAX (SMAX, -10.0*SMIN)
      I = 4 - LOG10 (SMAX)
      SFACT = 10.0 ** I
C                                       Average and sigma
      IF (ICNT.GT.0) THEN
         XV(1) = XS1(1) / ICNT
         XV(2) = XS1(2) / ICNT
C                                       NOT ATAN2
C         AVG = ATAN2 (-XV(2), (XV(1)+1.0E-20))
         AVG = ATAN (-XV(2)/(XV(1)+1.0E-20))
         XAVG = XS1(2) / ICNT
         END IF
      IF (ICNT.GT.3) THEN
         XS2(1) = (XS2(1)/ICNT) - XV(1)*XV(1)
         XS2(2) = (XS2(2)/ICNT) - XV(2)*XV(2)
         TEMP = ((XV(1)/(XV(1)*XV(1)+XV(2)*XV(2))) ** 2) * XS2(2) +
     *      ((XV(2)/(XV(1)*XV(1)+XV(2)*XV(2))) ** 2) * XS2(1)
         IF (TEMP.LT.0.0) TEMP = 0.0
         SIG(1) = SQRT (TEMP / (ICNT - 1))
         IF (SIG(1).LE.1.0) THEN
            SIG(1) = ASIN(SIG(1))
         ELSE
            SIG(1) = 3.14159
            END IF
         SIG(2) = SQRT (TEMP)
         IF (SIG(2).LE.1.0) THEN
            SIG(2) = ASIN(SIG(2))
         ELSE
            SIG(2) = 3.14159
            END IF
         END IF
      AVG = AVG * RAD2DG
      SIG(1) = SIG(1) * RAD2DG
      SIG(2) = SIG(2) * RAD2DG
C
 999  RETURN
      END
      SUBROUTINE CLAPPL (DISK, CNO, BIF, EIF, CLVER, ANVER, CLCORP,
     *   CATBLK, IRET)
C-----------------------------------------------------------------------
C   CLAPPL applies the corrections by IF to the CL and AN tables
C   Inputs:
C      DISK     I      Disk number
C      CNO      I      Catalog number
C      BIF      I      begin IF
C      EIF      I      end IF
C      CLVER    I      Input CL table version
C      CLCORP   R(*)   corrections for BIF through EIF
C   In/out:
C      CATBLK   I(*)   Data file header
C   Outputs:
C      IRET     I      Error code in handling the tables
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, BIF, EIF, CLVER, ANVER, CATBLK(256), IRET
      REAL      CLCORP(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMCL, LUN1, LUN2, TABUF1(512), TABUF2(512), VER, LI,
     *   NUMANT, NUMPOL, NUMIF, NTERM, CLKOLS(MAXCLC), CLNUMV(MAXCLC),
     *   IDSOU, ANTNO, SUBA, FREQID, REFA(2,MAXIF), ICLRNO, NREC, IREC,
     *   IANRNO, ANKOLS(MAXANC), ANNUMV(MAXANC), NUMORB, NOPCAL, ANTNIF,
     *   ANFQID, ISTYPE, LOCS, KEYTYP, INDEX, NOSTA, MNTSTA, I
      REAL      GMMOD, DTIMEI, IFR, DOPOFF(MAXIF), ATMOS, DATMOS,
     *   MBDELY(2), CLOCK(2), DCLOCK(2), DISP(2), DDISP(2), XT, YT,
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), CF(MAXIF), SF(MAXIF), POLRXY(2), UT1UTC,
     *   DATUTC, STAXOF, DIAMAN, FWHMAN(MAXIF), POLAA, POLCA(2*MAXIF),
     *   POLAB, POLCB(2*MAXIF)
      DOUBLE PRECISION TIME, GEODLY, ARRAYC(3), GSTIA0, DEGPDY, SAFREQ,
     *   STAXYZ(3), ORBPRM(6)
      CHARACTER RDATE*8, TIMSYS*8, ANAME*8, XYZHAN*8, TFRAME*8,
     *   SOLTYP*8, CHSOL(4)*8, ANNAME*8, POLTYA*2, POLTYB*2
      HOLLERITH XSOLTY(2)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /79, 78/
      DATA CHSOL /'ORI-ELP ', 'APPROX  ', 'X-Y LIN', 'VLBI'/
C-----------------------------------------------------------------------
      CALL FNDEXT ('CL', CATBLK, NUMCL)
      IF (NUMCL.LE.0) THEN
         MSGTXT = 'CANNOT APPLY TO CL TABLE IF NONE PRESENT'
         IRET = 10
         GO TO 990
         END IF
      MSGTXT = 'Updating the CL table to a new one'
      CALL MSGWRT (5)
      DO 10 LI = BIF,EIF
         IF (CLCORP(LI-BIF+1).NE.FBLANK) THEN
            CF(LI) = COS (CLCORP(LI-BIF+1) * DG2RAD)
            SF(LI) = SIN (CLCORP(LI-BIF+1) * DG2RAD)
         ELSE
            CF(LI) = 1.0
            SF(LI) = 0.0
            END IF
 10      CONTINUE
      VER = NUMCL + 1
      IF (CLVER.LE.0) CLVER = NUMCL
      CALL CALINI ('READ', TABUF1, DISK, CNO, CLVER, CATBLK, LUN1,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT CL TABLE'
         GO TO 990
         END IF
      CALL CALINI ('WRIT', TABUF2, DISK, CNO, VER, CATBLK, LUN2, ICLRNO,
     *   CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT CL TABLE'
         GO TO 990
         END IF
      NREC = TABUF1(5)
      DO 50 IREC = 1,NREC
         ICLRNO = IREC
         CALL TABCAL ('READ', TABUF1, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *      NUMIF, TIME, DTIMEI, IDSOU, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ INPUT CL TABLE'
            GO TO 990
            END IF
         DO 20 LI = BIF,EIF
            XT = CREAL(2,LI)
            YT = CIMAG(2,LI)
            CREAL(2,LI) = XT * CF(LI) - YT * SF(LI)
            CIMAG(2,LI) = XT * SF(LI) + YT * CF(LI)
 20         CONTINUE
         ICLRNO = IREC
         CALL TABCAL ('WRIT', TABUF2, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *      NUMIF, TIME, DTIMEI, IDSOU, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT CL TABLE'
            GO TO 990
            END IF
 50      CONTINUE
      CALL TABIO ('CLOS', 0, ICLRNO, TABUF1, TABUF1, IRET)
      IF (IRET.EQ.0) CALL TABIO ('CLOS', 0, ICLRNO, TABUF2, TABUF2,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING CL TABLES'
         GO TO 990
         END IF
C                                       now the AN table
      MSGTXT = 'Updating the AN table in place'
      CALL MSGWRT (5)
      CALL ANTINI ('READ', TABUF1, DISK, CNO, ANVER, CATBLK, LUN1,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN AN FILE FOR READ'
         GO TO 990
         END IF
C                                       find solution type
      ISTYPE = 0
      MSGSUP = 32000
      CALL TABKEY ('READ', 'POLTYPE ', 1, TABUF1, LOCS, XSOLTY, KEYTYP,
     *   IRET)
      MSGSUP = 0
      IF (IRET.GT.20) THEN
         MSGTXT = 'NO PCAL SOLUTION FOUND IN AN TABLE: NONE CORRECTED'
         CALL MSGWRT (6)
      ELSE IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ SOLUTION TYPE'
         GO TO 990
      ELSE IF (IRET.EQ.0) THEN
         CALL H2CHR (8, 1, XSOLTY, SOLTYP)
         DO 110 I = 1,4
            IF (SOLTYP.EQ.CHSOL(I)) ISTYPE = I
 110        CONTINUE
         IF (ISTYPE.EQ.0) THEN
            MSGTXT = 'PCAL SOLUTION TYPE NOT RECOGNIZED: NONE CORRECTED'
            CALL MSGWRT (6)
            IRET = 21
            END IF
         END IF
      CALL TABIO ('CLOS', 0, IANRNO, TABUF1, TABUF1, I)
      IF (IRET.GT.20) THEN
         IRET = 0
         GO TO 999
         END IF
C                                       ORI-ELP modify phase differences
      IF (SOLTYP.EQ.'ORI-ELP') THEN
C                                       Fetch old phase differences
         CALL PDRGET (DISK, CNO, ANVER, LUN1, CATBLK, ANTNIF, REFA, SF,
     *      TABUF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'GETTING OLD PHASE DIFFERENCES'
            GO TO 990
            END IF
C                                       Update values (radians)
         INDEX = 1
         DO 120 LI = BIF,EIF
            IF (CLCORP(INDEX).NE.FBLANK) SF(LI) = SF(LI) +
     *         CLCORP(INDEX) * DG2RAD
            INDEX = INDEX + 1
 120        CONTINUE
C                                       Save results
         CALL PDRSET (DISK, CNO, ANVER, LUN1, CATBLK, ANTNIF, REFA, SF,
     *      TABUF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-SETTING PHASE DIFFERENCES'
            GO TO 990
            END IF
C                                       others require AN table
      ELSE
         CALL ANTINI ('WRIT', TABUF1, DISK, CNO, ANVER, CATBLK, LUN1,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN AN FILE FOR WRITE'
            GO TO 990
            END IF
         NREC = TABUF1(5)
         DO 150 IREC = 1,NREC
            IANRNO = IREC
            CALL TABAN ('READ', TABUF1, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ OLD AN TABLE'
               GO TO 990
               END IF
            INDEX = 1 + (BIF - 1) * 2
            DO 130 LI = BIF,EIF
C                                       Right hand (or X) parameters
               XT = POLCA(INDEX)
               YT = POLCA(INDEX+1)
               POLCA(INDEX) = XT * CF(LI) - YT * SF(LI)
               POLCA(INDEX+1) = YT * CF(LI) + XT * SF(LI)
C                                       Left hand (or Y) parameters
               XT = POLCB(INDEX)
               YT = POLCB(INDEX+1)
               POLCB(INDEX) = XT * CF(LI) + YT * SF(LI)
               POLCB(INDEX+1) = YT * CF(LI) - XT * SF(LI)
               INDEX = INDEX + 2
 130           CONTINUE
            IANRNO = IREC
            CALL TABAN ('WRIT', TABUF1, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'UPDATE AN TABLE'
               GO TO 990
               END IF
 150        CONTINUE
         END IF
      CALL TABIO ('CLOS', 0, IANRNO, TABUF1, TABUF1, I)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLAPPL: ERROR',I4,' ON ',A)
      END
      SUBROUTINE XYDLIN (NC, NI, PDCOR, IRET)
C-----------------------------------------------------------------------
C   XYDLIN averages the U and V Stokes data in each scan and then
C   averages the atan2(v,u) over the scans.
C   Inputs:
C      NC      I      Number spectral channels
C      NI      I      Number IFs
C      NS      I      Number sources
C   Outputs:
C      PDCOR   R(*)   Average phase (NC,NI, 1-6 averaging in scans
C                        7-11 averaging over scans)
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   NC, NI, IRET
      DOUBLE PRECISION PDCOR(NC,NI,*)
C
      INCLUDE 'XYDIF.INC'
      INTEGER   LC, LI, I, JNCF, JNCIF, JNCS, INX, SCANUM
      DOUBLE PRECISION X, Y, W, XX, YY, TEMP, WT, SU, SV, UU, VV
      DOUBLE PRECISION DBGU(128,4), DBGV(128,4), DBGA(128,4)
      REAL      DT, T1, T2
      CHARACTER MARK*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      I = NC * NI * 11
      CALL DFILL (I, 0.0D0, PDCOR)
C                                       Single source default = 10 min.
      IF (ISNXT) THEN
         DT = 10.0 / 1440.0
C                                       Multisource DT default = scan
      ELSE
         DT = 1.0E10
         END IF
      T1 = 1.E10
      T2 = 1.E10
C                                       open uv data set
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN UV DATA SET'
         GO TO 990
         END IF
      JNCIF = INCIF / 3
      JNCS = INCS / 3
      JNCF = INCF / 3
      SCANUM = INXRNO
C                                       read loop point
 20   CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ UV DATA SET'
         GO TO 990
      ELSE IF (IRET.LE.0) THEN
C                                       Check if scan done
         IF ((INXRNO.GT.SCANUM) .OR. (IRET.LT.0) .OR.
     *      (RPARM(1+ILOCT).GT.(T1+DT))) THEN

           DO 40 LI = 1,NI
               DO 30 LC = 1,NC
                  IF ((PDCOR(LC,LI,2).GT.0.0D0) .AND.
     *               (PDCOR(LC,LI,5).GT.0.0D0)) THEN
                     UU = PDCOR(LC,LI,1) / PDCOR(LC,LI,2)
                     VV = PDCOR(LC,LI,4) / PDCOR(LC,LI,5)
                     SU = PDCOR(LC,LI,3) / PDCOR(LC,LI,2) - UU*UU
                     SV = PDCOR(LC,LI,6) / PDCOR(LC,LI,5) - VV*VV
                     TEMP = UU*UU*SV + VV*VV*SU
                     IF (TEMP.GT.0.0) THEN
                        WT = 1.0D0 / TEMP
C                                       NOT ATAN2
C                        X = ATAN2 (-VV, UU)
                        X = ATAN (-VV/UU)
                        XX = COS (X)
                        YY = SIN (X)
                        DBGU(LC,LI) = UU
                        DBGV(LC,LI) = VV
                        DBGA(LC,LI) = X * RAD2DG
                        PDCOR(LC,LI,7) = PDCOR(LC,LI,7) + XX * WT
                        PDCOR(LC,LI,8) = PDCOR(LC,LI,8) + YY * WT
                        PDCOR(LC,LI,9) = PDCOR(LC,LI,9) + WT
                        PDCOR(LC,LI,10) = PDCOR(LC,LI,10) + XX * XX * WT
                        PDCOR(LC,LI,11) = PDCOR(LC,LI,11) + YY * YY * WT
                        END IF
                     END IF
 30               CONTINUE
 40            CONTINUE
C                                       end of data?
            IF (IRET.LT.0) GO TO 100
C                                       re-zero
            T1 = RPARM(1+ILOCT)
            SCANUM = INXRNO
C                                       leave 7 8 9 10 11 alone
            I = NC * NI * 6
            CALL DFILL (I, 0.0D0, PDCOR)
            END IF
C                                       sum for line work
         DO 60 LI = 1,NI
            INX = (LI - 1) * JNCIF + 2 * JNCS + 1
            DO 50 LC = 1,NC
               WT = VIS(3,INX)
               IF (WT.GT.0.0D0) THEN
                  PDCOR(LC,LI,1) = PDCOR(LC,LI,1) +
     *               VIS(1,INX) * WT
                  PDCOR(LC,LI,2) = PDCOR(LC,LI,2) +
     *               WT
                  PDCOR(LC,LI,3) = PDCOR(LC,LI,3) +
     *               (VIS(1,INX) ** 2) * WT
                  END IF
               INX = INX + JNCS
               WT = VIS(3,INX)
               IF (WT.GT.0.0D0) THEN
                  PDCOR(LC,LI,4) = PDCOR(LC,LI,4) +
     *               VIS(1,INX) * WT
                  PDCOR(LC,LI,5) = PDCOR(LC,LI,5) +
     *               WT
                  PDCOR(LC,LI,6) = PDCOR(LC,LI,6) +
     *               (VIS(1,INX) ** 2) * WT
                  END IF
               INX = INX - JNCS + JNCF
 50            CONTINUE
 60         CONTINUE
         GO TO 20
         END IF
C                                       we are done
 100  CALL UVGET ('CLOS', RPARM, VIS, IRET)
      IRET = 0
C                                       average and rms
      DO 120 LI = 1,NI
         DO 110 LC = 1,NC
            W = PDCOR(LC,LI,9)
            IF (W.GT.0.0) THEN
               X = PDCOR(LC,LI,7) / W
               Y = PDCOR(LC,LI,8) / W
               XX = PDCOR(LC,LI,10) / W
               YY = PDCOR(LC,LI,11) / W
C                                       NOT ATAN2 ?
C               PDCOR(LC,LI,1) = RAD2DG * ATAN2 (Y, X)
               PDCOR(LC,LI,1) = RAD2DG * ATAN (Y/X) + DOINV
               XX = XX - X * X
               YY = YY - Y * Y
               TEMP = (X * X + Y * Y) ** 2
               IF (TEMP.GT.0.0D0) TEMP = (X*X*YY + Y*Y*XX) / TEMP
               IF (TEMP.GT.0.0D0) THEN
                  TEMP = RAD2DG * SQRT (TEMP)
               ELSE
                  TEMP = 999.0D0
                  END IF
               PDCOR(LC,LI,2) = TEMP / SQRT (W)
               MARK = ' '
               IF ((LC.LT.LBCHAN) .OR. (LC.GT.LECHAN)) MARK = '*'
               WRITE (MSGTXT,1110) LI+BIF-1, LC, PDCOR(LC,LI,1), TEMP,
     *            MARK
               CALL MSGWRT (5)
            ELSE
               PDCOR(LC,LI,1) = DBLANK
               PDCOR(LC,LI,2) = 0.0
               END IF
 110        CONTINUE
 120     CONTINUE
      IF ((LBCHAN.GT.1) .OR. (LECHAN.LT.NC)) THEN
         MSGTXT = '* following values => will not be applied to data'
         CALL MSGWRT (5)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XYDLIN: ERROR',I4,' ON ',A)
 1110 FORMAT ('IF',I3,' ch',I6,2X,F8.2,' +-',F8.2,2X,A)
      END
      SUBROUTINE BPAPPL (DISK, CNO, BCHAN, BIF, BPVER, PDVER, SUBARR,
     *   FRQSEL, NCH, NIF, PDCOR, CATBLK, IRET)
C-----------------------------------------------------------------------
C   BPAPPL writes a new BP table based on the one used, a new CP table,
C   and updates the PD table with the corrections contained in PDCOR
C   Inputs
C      DISK     I      Disk number
C      CNO      I      Catalog number
C      BCHAN    I      Begin channel
C      BIF      I      Begin IF
C      BPVER    I      BP version in
C      SUBARR   I      Subarray
C      FRQSEL   I      Freq ID
C      NCH      I      Number channels
C      NIF      I      Number IFs
C      PDCOR    D(*)   Phase change in degrees (Nch,Nif)
C   In/Out:
C      CATBLK   I(*)   UV file header
C   Output:
C      IRET
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, BCHAN, BIF, BPVER, PDVER, SUBARR, FRQSEL,
     *   NCH, NIF, CATBLK(*), IRET
      DOUBLE PRECISION PDCOR(NCH,NIF)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMTA, LUN1, LUN2, TABUF1(512), TABUF2(512), IBPRNO,
     *   BPKOLS(MAXBPC), BPNUMV(MAXBPC), NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *   LCHAN, NUMSHF, NREC, IREC, SOURID, SUBA, ANT, FREQID, LI, LC,
     *   REFANT(2), I, JNX, VER, IPDRNO, PDKOLS(9), PDNUMV(9),
     *   PDKOLO(9), PDNUMO(9)
      DOUBLE PRECISION TIME, CHSHFT(MAXIF)
      REAL      CF, SF, LOWSHF, DELSHF, INTERV, BANDW, WEIGHT(2*MAXIF),
     *   BNDPAS(2,MAXCIF), XT, YT, VFLUX(4,MAXCIF), PHDIFF(MAXCIF)
      CHARACTER LBPTYP*8, POLTYP*8
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (BNDPAS, VFLUX)
      DATA LUN1, LUN2 /79, 78/
C-----------------------------------------------------------------------
      CALL FNDEXT ('BP', CATBLK, NUMTA)
      IF (BPVER.LE.0) BPVER = NUMTA
      VER = NUMTA + 1
C                                       modify existing BP table
      IF (BPVER.GT.0) THEN
         MSGTXT = 'Updating existing BP table, writing new one'
         CALL MSGWRT (5)
         CALL BPINI ('READ', TABUF2, DISK, CNO, BPVER, CATBLK, LUN2,
     *      IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *      LCHAN, NUMSHF, LOWSHF, DELSHF, LBPTYP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OLD BP TABLE'
            GO TO 990
            END IF
         CALL BPINI ('WRIT', TABUF1, DISK, CNO, VER, CATBLK, LUN1,
     *      IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *      LCHAN, NUMSHF, LOWSHF, DELSHF, LBPTYP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OLD BP TABLE'
            GO TO 990
            END IF
         NREC = TABUF2(5)
         DO 50 IREC = 1,NREC
            IBPRNO = IREC
            CALL TABBP ('READ', TABUF2, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *         NUMFRQ, NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, BANDW,
     *         CHSHFT, FREQID, REFANT, WEIGHT, BNDPAS, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ OLD BP TABLE'
               GO TO 990
               END IF
            IF (((SUBA.LE.0) .OR. (SUBARR.LE.0) .OR. (SUBA.EQ.SUBARR))
     *         .AND. ((FREQID.LE.0) .OR. (FRQSEL.LE.0) .OR.
     *         (FREQID.EQ.FRQSEL)) .AND. (IRET.GT.-2)) THEN
               DO 30 LI = 1,NIF
                  JNX = (LI + BIF - 2) * NUMFRQ + BCHAN + NUMFRQ*NUMIF
                  DO 20 LC = 1,NCH
                     XT = BNDPAS(1,JNX)
                     YT = BNDPAS(2,JNX)
                     IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK) .AND.
     *                  (PDCOR(LC,LI).NE.DBLANK)) THEN
                        CF = COS (PDCOR(LC,LI)*DG2RAD)
                        SF = SIN (PDCOR(LC,LI)*DG2RAD)
                        BNDPAS(1,JNX) = XT*CF - YT*SF
                        BNDPAS(2,JNX) = XT*SF + YT*CF
                        END IF
                     JNX = JNX + 1
 20                  CONTINUE
 30               CONTINUE
               END IF
            IBPRNO = IREC
            CALL TABBP ('WRIT', TABUF1, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *         NUMFRQ, NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, BANDW,
     *         CHSHFT, FREQID, REFANT, WEIGHT, BNDPAS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW BP TABLE'
               GO TO 990
               END IF
 50         CONTINUE
         CALL TABIO ('CLOS', 0, IBPRNO, TABUF2, TABUF2, LI)
C                                       create one from scratch
      ELSE
         MSGTXT = 'Writing new BP table from scratch'
         CALL MSGWRT (5)
         CALL GETANT (DISK, CNO, SUBARR, CATBLK, TABUF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ AN FILE WITH GETANT'
            GO TO 990
            END IF
         NUMANT = NSTNS
         NUMPOL = 2
         NUMIF = 1
         IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
         NUMFRQ = CATBLK(KINAX+JLOCF)
         LCHAN = 1
         NUMSHF = 3
         LOWSHF = 0.0
         DELSHF = 0.0
         LBPTYP = ' '
         CALL BPINI ('WRIT', TABUF1, DISK, CNO, VER, CATBLK, LUN1,
     *      IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *      LCHAN, NUMSHF, LOWSHF, DELSHF, LBPTYP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OLD BP TABLE'
            GO TO 990
            END IF
         TIME = 0.0D0
         INTERV = 0.0
         SOURID = 0
         SUBA = SUBARR
         FREQID = FRQSEL
         REFANT(1) = 1
         REFANT(2) = 1
         LI = NUMIF * NUMPOL * NUMFRQ
         CALL RFILL (2*MAXIF, 1.0, WEIGHT)
         DO 110 I = 1,LI
            BNDPAS(1,I) = 1.0
            BNDPAS(2,I) = 0.0
 110        CONTINUE
         DO 130 LI = 1,NIF
            JNX = (LI + BIF - 2) * NUMFRQ + BCHAN + NUMFRQ*NUMIF
            DO 120 LC = 1,NCH
               IF (PDCOR(LC,LI).NE.DBLANK) THEN
                  BNDPAS(1,JNX) = COS (PDCOR(LC,LI)*DG2RAD)
                  BNDPAS(2,JNX) = SIN (PDCOR(LC,LI)*DG2RAD)
               ELSE
                  BNDPAS(1,JNX) = FBLANK
                  BNDPAS(2,JNX) = FBLANK
                  END IF
               JNX = JNX + 1
 120           CONTINUE
 130        CONTINUE
         DO 150 ANT = 1,NSTNS
            IBPRNO = ANT
            CALL TABBP ('WRIT', TABUF1, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *         NUMFRQ, NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, BANDW,
     *         CHSHFT, FREQID, REFANT, WEIGHT, BNDPAS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW BP TABLE'
               GO TO 990
               END IF
 150        CONTINUE
         END IF
      CALL TABIO ('CLOS', 0, IBPRNO, TABUF1, TABUF1, LI)
C                                       PD update
      CALL FNDEXT ('PD', CATBLK, NUMTA)
      IF (NUMTA.LE.0) THEN
         MSGTXT = 'NO PD TABLE - HOW CAN THIS BE?'
         GO TO 990
         END IF
      MSGTXT = 'Updating existing PD table, writing new one'
      CALL MSGWRT (5)
      VER = PDVER
      CALL PDINI ('READ', TABUF2, DISK, CNO, VER, CATBLK, LUN2,
     *   IPDRNO, PDKOLS, PDNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *   POLTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OLD PD TABLE FOR READ'
         GO TO 990
         END IF
      VER = NUMTA + 1
      CALL PDINI ('WRIT', TABUF1, DISK, CNO, VER, CATBLK, LUN1,
     *   IPDRNO, PDKOLO, PDNUMO, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *   POLTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-OPEN OLD PD TABLE FOR WRITE'
         GO TO 990
         END IF
      NREC = TABUF2(5)
      DO 360 IREC = 1,NREC
         IPDRNO = IREC
         CALL TABPD ('READ', TABUF2, IPDRNO, PDKOLS, PDNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, BNDPAS,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ PD TABLE'
            GO TO 990
            END IF
         IF (((SUBA.LE.0) .OR. (SUBARR.LE.0) .OR. (SUBA.EQ.SUBARR))
     *      .AND. (IRET.GT.-3)) THEN
C                                       Just add to phase difference
            IF (POLTYP.EQ.'ORI-ELP') THEN
               DO 320 LI = 1,NIF
                  JNX = (LI + BIF - 2) * NUMFRQ + BCHAN
                  DO 310 LC = 1,NCH
                     IF (PDCOR(LC,LI).NE.DBLANK) PHDIFF(JNX) =
     *                  PHDIFF(JNX) + PDCOR(LC,LI) * DG2RAD
                     JNX = JNX + 1
 310                 CONTINUE
 320              CONTINUE
C                                       correct D terms
            ELSE
               DO 350 LI = 1,NIF
                  JNX = (LI + BIF - 2) * NUMFRQ + BCHAN
                  DO 330 LC = 1,NCH
                     XT = BNDPAS(1,JNX)
                     YT = BNDPAS(2,JNX)
                     IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK) .AND.
     *                  (PDCOR(LC,LI).NE.DBLANK)) THEN
                        CF = COS (PDCOR(LC,LI)*DG2RAD)
                        SF = SIN (PDCOR(LC,LI)*DG2RAD)
                        BNDPAS(1,JNX) = XT*CF - YT*SF
                        BNDPAS(2,JNX) = XT*SF + YT*CF
                        END IF
                     JNX = JNX + 1
 330                 CONTINUE
C                                       2nd polarization opposite
                  JNX = (LI + BIF - 2) * NUMFRQ + BCHAN +
     *               NUMFRQ * NUMIF
                  DO 340 LC = 1,NCH
                     XT = BNDPAS(1,JNX)
                     YT = BNDPAS(2,JNX)
                     IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK) .AND.
     *                  (PDCOR(LC,LI).NE.DBLANK)) THEN
                        CF = COS (PDCOR(LC,LI)*DG2RAD)
                        SF = SIN (PDCOR(LC,LI)*DG2RAD)
                        BNDPAS(1,JNX) = XT*CF + YT*SF
                        BNDPAS(2,JNX) = YT*CF - XT*SF
                        END IF
                     JNX = JNX + 1
 340                 CONTINUE
 350              CONTINUE
               END IF
            END IF
         IPDRNO = IREC
         CALL TABPD ('WRIT', TABUF1, IPDRNO, PDKOLO, PDNUMO, NUMIF,
     *      NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, BNDPAS,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-WRITE PD TABLE'
            GO TO 990
            END IF
 360     CONTINUE
      CALL TABIO ('CLOS', 0, IPDRNO, TABUF1, TABUF1, LI)
      CALL TABIO ('CLOS', 0, IPDRNO, TABUF2, TABUF2, LI)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPAPPL: ERROR',I4,' ON ',A)
      END
      SUBROUTINE XYDPRT (NC, NI, CSNAME, BIF, LBCHAN, LECHAN, CATBLK,
     *   DOCRT, LPNAME, PDCOR)
C-----------------------------------------------------------------------
C   XYDPRT writes a PLOTR text file if requested
C   Inputs:
C      NC       I       Number spectral channels
C      NI       I       Number IFs
C      CSNAME   C*(*)   Source names
C      DOCRT    R       not 0 => okay to write
C      LPNAME   C*(*)   not blank => write this file name
C      PDCOR    D(*)    Average phase (NC,NI,NS, ph/err/cnt/wrk1/wrk2)
C-----------------------------------------------------------------------
      INTEGER   NC, NI, BIF, LBCHAN, LECHAN, CATBLK(*)
      REAL      DOCRT
      DOUBLE PRECISION PDCOR(NC,NI,*)
      CHARACTER CSNAME(*)*(*), LPNAME*(*)
C
      INTEGER   LC, LI, JTRIM, TLUN, TIND, IERR, LUNTMP, INX, I, JJ, IC,
     *   LS
      CHARACTER LINE*80, CNAME*12, CCLAS*6, CNUMB*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF ((DOCRT.NE.0.0) .AND. (LPNAME.NE.' ')) THEN
         TLUN = LUNTMP (2)
         CALL ZTXOPN ('WRIT', TLUN, TIND, LPNAME, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT TEXT FILE'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL H2CHR (12, KHIMNO, CATBLK(KHIMN), CNAME)
         CALL H2CHR (6, KHIMCO, CATBLK(KHIMC), CCLAS)
         JJ = JTRIM (CNAME)
         I = JTRIM (CCLAS)
         WRITE (CNUMB,1005) CATBLK(KIIMS)
         CALL CHTRIM (CNUMB, 6, CNUMB, LS)
         WRITE (LINE,1010) CNAME(:JJ), CCLAS(:I), CNUMB(:LS)
         INX = JTRIM (LINE)
         CALL ZTXIO ('WRIT', TLUN, TIND, LINE(:INX), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE OUTPUT TEXT FILE'
            GO TO 990
            END IF
         INX = 1
         LINE = ' '
         CALL ZTXIO ('WRIT', TLUN, TIND, LINE(:INX), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE OUTPUT TEXT FILE'
            GO TO 990
            END IF
         CALL ZTXIO ('WRIT', TLUN, TIND, LINE(:INX), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE OUTPUT TEXT FILE'
            GO TO 990
            END IF
         LINE = 'Channels'
         INX = JTRIM (LINE)
         CALL ZTXIO ('WRIT', TLUN, TIND, LINE(:INX), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE OUTPUT TEXT FILE'
            GO TO 990
            END IF
         LINE = 'XY Phase'
         INX = JTRIM (LINE)
         CALL ZTXIO ('WRIT', TLUN, TIND, LINE(:INX), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE OUTPUT TEXT FILE'
            GO TO 990
            END IF
         WRITE (LINE,1015) -1, 'Phase'
         INX = JTRIM (LINE)
         CALL ZTXIO ('WRIT', TLUN, TIND, LINE(:INX), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE OUTPUT TEXT FILE'
            GO TO 990
            END IF
         WRITE (LINE,1015) -2, 'Error'
         INX = JTRIM (LINE)
         CALL ZTXIO ('WRIT', TLUN, TIND, LINE(:INX), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE OUTPUT TEXT FILE'
            GO TO 990
            END IF
         INX = 22
         DO 30 LI = 1,NI
            IC = (LI+BIF-2) * NC + LBCHAN - 1
            DO 20 LC = LBCHAN,LECHAN
               IC = IC + 1
               IF (PDCOR(LC,LI,1).NE.DBLANK) THEN
                  WRITE (LINE,1020) IC, PDCOR(LC,LI,1), 1
                  INX = JTRIM (LINE)
                  CALL ZTXIO ('WRIT', TLUN, TIND, LINE(:INX), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1000) IERR,
     *                  'WRITE OUTPUT TEXT FILE'
                     GO TO 990
                     END IF
                  END IF
 20            CONTINUE
 30         CONTINUE
         DO 50 LI = 1,NI
            IC = (LI+BIF-2) * NC + LBCHAN - 1
            DO 40 LC = LBCHAN,LECHAN
               IC = IC + 1
               IF (PDCOR(LC,LI,1).NE.DBLANK) THEN
                  WRITE (LINE,1020) IC, PDCOR(LC,LI,2), 2
                  INX = JTRIM (LINE)
                  CALL ZTXIO ('WRIT', TLUN, TIND, LINE(:INX), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1000) IERR,
     *                  'WRITE OUTPUT TEXT FILE'
                     GO TO 990
                     END IF
                  END IF
 40            CONTINUE
 50         CONTINUE
         CALL ZTXCLS (TLUN, TIND, IERR)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XYDPRT: ERROR:',I4,' ON ',A)
 1005 FORMAT (I6)
 1010 FORMAT ('Phase corrections for ',A,'.',A,'.',A)
 1015 FORMAT ('SL',I4,2X,A,1X,A)
 1020 FORMAT (I7,F11.3,I4)
      END
      SUBROUTINE XYDSMT (NC, NI, LBCHAN, LECHAN, CATBLK, XINTP, PDCOR)
C-----------------------------------------------------------------------
C   XYDSMT smooths the phases which are about to be applied
C   Inputs:
C      NC       I       Number spectral channels
C      NI       I       Number IFs
C      LBCHAN   I       Ignore solutions from channels < LBCHAN
C      LECHAN   I       Ignore colutions from channels > LECHAN
C   In/out:
C      XINTP    R(3)    Smoothing parameters
C      PDCOR    D(*)    Average phase (NC,NI,NS, ph/err/cnt/wrk1/wrk2)
C-----------------------------------------------------------------------
      INTEGER   NC, NI, LBCHAN, LECHAN, CATBLK(*)
      REAL      XINTP(3)
      DOUBLE PRECISION PDCOR(NC,NI,*)
C
      INTEGER   IT, I, N, LC, LI, PSMRAD, J1, J2, J, K
      DOUBLE PRECISION FX, X, W, WIDTHS(4), SUPS(4), PSMTAB(1024), CS,
     *   SS, WS
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA WIDTHS /4.0, 2.0, 2.0, 3.0/
      DATA SUPS /1.0, 3.0, 1.0, 4.0/
C-----------------------------------------------------------------------
C                                       type and defaults
      IT = XINTP(1) + 0.1
      IT = MAX (0, IT)
      IF (IT.GT.4) IT = 1
      XINTP(1) = IT
C                                       no smoothing, do extrapolation
      IF (IT.EQ.0) THEN
         XINTP(2) = 0.0
         XINTP(3) = 0.0
         DO 30 LI = 1,NI
            DO 10 LC = 1,LBCHAN-1
               PDCOR(LC,LI,1) = PDCOR(LBCHAN,LI,1)
               PDCOR(LC,LI,2) = PDCOR(LBCHAN,LI,2)
 10            CONTINUE
            DO 20 LC = LECHAN+1,NC
               PDCOR(LC,LI,1) = PDCOR(LECHAN,LI,1)
               PDCOR(LC,LI,2) = PDCOR(LECHAN,LI,2)
 20            CONTINUE
 30         CONTINUE
C                                       smoothing
      ELSE
         N = CATBLK(KINAX+JLOCF)
         IF ((XINTP(2).LT.0.5) .OR. (XINTP(2).GT.N/3.0)) XINTP(2) =
     *      WIDTHS(IT)
         IF ((XINTP(3).GT.4.0*SUPS(IT)*XINTP(2)) .OR.
     *      (XINTP(3).LT.XINTP(2))) XINTP(3) = XINTP(2) * SUPS(IT)
         PSMRAD = XINTP(3) / 2.0 + 0.1
         IF (PSMRAD.GT.510) THEN
            PSMRAD = 509
            XINTP(3) = 2.0* PSMRAD
            XINTP(2) = XINTP(3) / SUPS(IT)
            END IF
         N = PSMRAD + 1
         FX = 2.0 / XINTP(2)
         W = 1.0
         PSMTAB(1) = 1.0
C                                       Hanning smooth
         IF (IT.EQ.1) THEN
            DO 40 I = 2,N
               X = I - 1.0
               PSMTAB(I) = MAX (0.0D0, 1.0-FX*X)
               W = W + 2 * PSMTAB(I)
 40            CONTINUE
C                                       Gaussian smooth
         ELSE IF (IT.EQ.2) THEN
            FX = -LOG(2.0) * FX * FX
            DO 45 I = 2,N
               X = I - 1.0
               PSMTAB(I) = EXP (FX * X * X)
               W = W + 2 * PSMTAB(I)
 45            CONTINUE
C                                       Boxcar smooth
         ELSE IF (IT.EQ.3) THEN
            FX = 1.0 / FX
            DO 50 I = 2,N
               X = I - 1.0
               IF (X.LT.FX) THEN
                  PSMTAB(I) = 1.0
               ELSE IF (X.EQ.FX) THEN
                  PSMTAB(I) = 0.5
                  END IF
               W = W + 2 * PSMTAB(I)
 50            CONTINUE
C                                      Sinc smooth
         ELSE IF (IT.EQ.4) THEN
            FX = 3.14159 * FX
            DO 55 I = 2,N
               X = (I - 1.0) * FX
               PSMTAB(I) = SIN(X) / X
               W = W + 2 * PSMTAB(I)
 55            CONTINUE
            END IF
C                                       Normalize integral
         IF (W.LE.0.0) W = 1.0
         DO 60 I = 1,N
            PSMTAB(I) = PSMTAB(I) / W
 60         CONTINUE
C                                       Now smooth
         DO 100 LI = 1,NI
            DO 80 LC = 1,NC
               CS = 0.0D0
               SS = 0.0D0
               WS = 0.0D0
               J1 = MAX (LBCHAN, LC-PSMRAD)
               J2 = MIN (LECHAN, LC+PSMRAD)
               DO 70 J = J1,J2
                  K = ABS (J-LC) + 1
                  IF ((PDCOR(J,LI,1).NE.DBLANK) .AND.
     *               (PDCOR(J,LI,2).GT.0.0)) THEN
                     W = PSMTAB(K) / (PDCOR(J,LI,2) ** 2)
                     CS = CS + W * COS (PDCOR(J,LI,1)*DG2RAD)
                     SS = SS + W * SIN (PDCOR(J,LI,1)*DG2RAD)
                     WS = WS + W
                     END IF
 70               CONTINUE
               IF (WS.GT.0.0) THEN
C                  PDCOR(LC,LI,5) = ATAN2 (SS, CS) * RAD2DG
                  PDCOR(LC,LI,5) = ATAN (SS/CS) * RAD2DG
                  PDCOR(LC,LI,4) = SQRT (1.0 / WS)
               ELSE
                  PDCOR(LC,LI,5) = DBLANK
                  PDCOR(LC,LI,4) = DBLANK
                  END IF
 80            CONTINUE
            J1 = NC + 1
            J2 = 0
            DO 85 LC = 1,NC
               PDCOR(LC,LI,1) = PDCOR(LC,LI,5)
               PDCOR(LC,LI,2) = PDCOR(LC,LI,4)
               IF (PDCOR(LC,LI,1).NE.DBLANK) THEN
                  J1 = MIN (J1, LC)
                  J2 = MAX (J2, LC)
                  END IF
 85            CONTINUE
            DO 90 LC = 1,J1-1
               PDCOR(LC,LI,1) = PDCOR(J1,LI,1)
               PDCOR(LC,LI,2) = PDCOR(J1,LI,2)
 90            CONTINUE
            DO 95 LC = J2+1,NC
               PDCOR(LC,LI,1) = PDCOR(J2,LI,1)
               PDCOR(LC,LI,2) = PDCOR(J2,LI,2)
 95            CONTINUE
 100        CONTINUE
         END IF
C                                       interpolate remaining blanks
      DO 200 LI = 1,NI
         DO 190 LC = 1,NC
            IF (PDCOR(LC,LI,1).EQ.DBLANK) THEN
               J1 = -999999
               DO 110 I = LC-1,1,-1
                  IF (PDCOR(I,LI,1).NE.DBLANK) J1 = MAX (J1, I)
 110              CONTINUE
               J2 = 999999
               DO 120 I = LC+1,NC
                  IF (PDCOR(I,LI,1).NE.DBLANK) J2 = MIN (J2, I)
 120              CONTINUE
               IF ((J1.GT.0) .AND. (J2.LE.NC)) THEN
                  DO 130 I = J1+1,J2-1
                     PDCOR(I,LI,1) = ((I-J1) * PDCOR(J1,LI,1) +
     *                  (J2-I) * PDCOR(J2,LI,1)) / (J2-J1)
                     PDCOR(I,LI,2) = ((I-J1) * PDCOR(J1,LI,2) +
     *                  (J2-I) * PDCOR(J2,LI,2)) / (J2-J1)
 130                 CONTINUE
               ELSE IF (J1.GT.0) THEN
                  DO 140 I = J1+1,NC
                     PDCOR(I,LI,1) = PDCOR(J1,LI,1)
                     PDCOR(I,LI,2) = PDCOR(J1,LI,2)
 140                 CONTINUE
               ELSE IF (J2.LE.NC) THEN
                  DO 150 I = 1,J2-1
                     PDCOR(I,LI,1) = PDCOR(J2,LI,1)
                     PDCOR(I,LI,2) = PDCOR(J2,LI,2)
 150                 CONTINUE
C                                       all blanked
               ELSE
                  GO TO 200
                  END IF
               END IF
 190        CONTINUE
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE XYDHI (NCH, NIF, IB, IE, CLCORP, PCLCOR)
C-----------------------------------------------------------------------
C   writes HI records when solution is appled to data
C-----------------------------------------------------------------------
      INTEGER   NCH, NIF, IB, IE
      REAL      CLCORP(*), PCLCOR(*)
C
      INCLUDE 'XYDIF.INC'
      INTEGER   LUN, LUNTMP, TIME(3), DATE(3), IERR, BUFF2(256), I
      CHARACTER CTIME(2)*12, HILINE*72
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       skip if values are going away
      IF ((DOLINE.GT.0.0) .AND. (DOAPLY.LE.0.0)) GO TO 999
C                                       Write History.
      LUN = LUNTMP (1)
      CALL HIINIT (3)
C                                       Open old history
      CALL HIOPEN (LUN, DISKIN, CNOIN, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       calibration et al.
      CALL CALHIS (LUN, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       SPECTRAL, INTPARM
      IF (DOLINE.LE.0.0) THEN
         HILINE = TSKNAM // 'SPECTRAL = -1   / continuum mode used'
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         DO 20 I = IB,IE
            WRITE (HILINE,1015) TSKNAM, I, CLCORP(I-IB+1),
     *         PCLCOR(I-IB+1+NIF)
            CALL HIADD (LUN, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 20         CONTINUE
      ELSE
         HILINE = TSKNAM // 'SPECTRAL = +1   / spectral mode used'
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         IF (XINTP(1).EQ.0.0) THEN
            HILINE = TSKNAM // 'INTPARM = 0  / no spectral smoothing'
         ELSE
            WRITE (HILINE,1020) TSKNAM, XINTP
            END IF
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         IF ((LBCHAN.GT.1) .OR. (LECHAN.LT.NCH)) THEN
            WRITE (HILINE,1025) LBCHAN, LECHAN
            CALL HIADD (LUN, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            END IF
         END IF
      IF (DOAPLY.GT.0.0) THEN
         HILINE = TSKNAM // 'DOAPPLY = 1   / corrections applied'
     *      // ' by ' // TSKNAM
      ELSE
         HILINE = TSKNAM // 'DOAPPLY = -1   / corrections NOT applied'
     *      // ' by ' // TSKNAM
         END IF
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN, .TRUE., BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1015 FORMAT (A6,'/ IF',I3,' avg XY phase',F8.3,' +-',F8.3)
 1020 FORMAT (A6,'INTPARM=',F3.0,2(',',F5.1),3X,'/ smoothing parms')
 1025 FORMAT (A6,'BCHAN=',I5,' ECHAN=',I5,' / extrapolate solutions',
     *   ' outside')
      END
      SUBROUTINE DXYGET (VISIN, IND, IA1, IA2, VISOUT, DROP)
C-----------------------------------------------------------------------
C   Gets requested data from XY Stokes visibility record, reformatting as
C   needed.  REQUIRES setup by DGINIT to set values in common DSEL.INC.
C
C   Special version for XYDIF: parallactic angles set to zero
C
C   Inputs:
C      VISIN    R(IND,*)   Input visibility array
C      IND      I          First dimension of VISIN (CATBLK(KINAX))
C      IA1      I          Antenna 1 number
C      IA2      I          Antenna 2 number
C   Input from DSEL.INC
C      MVIS     I(8)       Number of visibilities in requested output
C                          format., # pol, BIF, EIF, BCH, ECH, JNCIF,
C                          JNCF
C      JADR     I(2,4)     Pointers to the first and second visibility
C                          input records to be used in the output
C                          record. If JADR(1,n) is negative use IABS
C                          (JADR(1,n)) and multiply the visibility by i
C                          (=SQRT(-1)).  If JADR(2,n) < 0, use IABS and
C                          construct (1) + i*(2)
C      SELFAC   R(2,4)     Factors to be multiplied by the first and
C                          second input vis's to make the output vis.
C      ALLWT    L          Flag, = .TRUE. if all visibilities must have
C                          positive weight.
C   Output:
C      VISOUT   R(3,*)     Output visibility record
C      DROP     L          .TRUE. if all data in record flaged.
C-----------------------------------------------------------------------
      INTEGER   IND, IA1, IA2
      LOGICAL   DROP
      REAL      VISIN(IND,*), VISOUT(3,*)
C
      INTEGER   I, LF, LC, LP, LFOFF, IOFF, K
      LOGICAL   GOOD, HASWT
      REAL      WT1, RV(4), IV(4), DCOS, DSIN, SCOS, SSIN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPDC.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      GOOD = .FALSE.
C                                       See if weight in data
      HASWT = IND.GE.3
C                                       Check input.
      IF (MVIS(1).LE.0) GO TO 999
C                                       zero output
      I = 3 * MVIS(2) * (MVIS(6)-MVIS(5)+1) * (MVIS(4)-MVIS(3)+1)
      CALL RFILL (I, 0.0, VISOUT)
C                                       Loop checking
C                                       and summing weights
C                                       and getting visibilities.
      I = 0
      LFOFF = (MVIS(3) - 2) * MVIS(7) + (MVIS(5) - 2) * MVIS(8)
      DO 70 LF = MVIS(3),MVIS(4)
         LFOFF = LFOFF + MVIS(7)
         IOFF = LFOFF
         DO 60 LC = MVIS(5),MVIS(6)
            IOFF = IOFF + MVIS(8)
            WT1 = 0.0
C                                       advance output pointer
C                                       in case flagged
            I = I + MVIS(2)
            IF (HASWT) THEN
               DO 10 LP = 1,4
                  IF (VISIN(3,LP+IOFF).LE.0.0) GO TO 60
                  WT1 = WT1 + VISIN(3,LP+IOFF)
 10               CONTINUE
            ELSE
               DO 11 LP = 1,4
                  IF (VISIN(1,LP+IOFF).EQ.FBLANK) GO TO 60
                  IF (VISIN(2,LP+IOFF).EQ.FBLANK) GO TO 60
                  WT1 = WT1 + 1.0
 11               CONTINUE
               END IF
C                                       reset pointer
            I = I - MVIS(2)
C                                       pick up data: note reorder
            GOOD = .TRUE.
            WT1 = WT1 / 2.0
            RV(1) = VISIN(1,IOFF+1)
            RV(4) = VISIN(1,IOFF+2)
            RV(2) = -VISIN(1,IOFF+3)
            RV(3) = -VISIN(1,IOFF+4)
            IV(1) = VISIN(2,IOFF+1)
            IV(4) = VISIN(2,IOFF+2)
            IV(2) = -VISIN(2,IOFF+3)
            IV(3) = -VISIN(2,IOFF+4)
C            DCOS = COS (PANGLE(IA1) - PANGLE(IA2))
C            DSIN = -SIN (PANGLE(IA1) - PANGLE(IA2))
C            SCOS = COS (PANGLE(IA1) + PANGLE(IA2))
C            SSIN = -SIN (PANGLE(IA1) + PANGLE(IA2))
            DCOS = 1.
            SCOS = 1.
            DSIN = 0.
            SSIN = 0.
C                                       loop through request pols
            DO 40 LP = 1,MVIS(2)
               I = I + 1
C                                       Set visibility.
               VISOUT(3,I) = WT1
               K = -SELFAC(1,LP) - 9.95
C                                       I pol
               IF (K.EQ.1) THEN
                  VISOUT(1,I) = (DCOS*RV(1) + DSIN*RV(2) - DSIN*RV(3) +
     *               DCOS*RV(4)) / 2.0
                  VISOUT(2,I) = (DCOS*IV(1) + DSIN*IV(2) - DSIN*IV(3) +
     *               DCOS*IV(4)) / 2.0
C                                       Q pol
               ELSE IF (K.EQ.2) THEN
                  VISOUT(1,I) = (SCOS*RV(1) - SSIN*RV(2) - SSIN*RV(3) -
     *               SCOS*RV(4)) / 2.0
                  VISOUT(2,I) = (SCOS*IV(1) - SSIN*IV(2) - SSIN*IV(3) -
     *               SCOS*IV(4)) / 2.0
C                                       U pol
               ELSE IF (K.EQ.3) THEN
                  VISOUT(1,I) = (SSIN*RV(1) + SCOS*RV(2) + SCOS*RV(3) -
     *               SSIN*RV(4)) / 2.0
                  VISOUT(2,I) = (SSIN*IV(1) + SCOS*IV(2) + SCOS*IV(3) -
     *               SSIN*IV(4)) / 2.0
C                                       V pol
               ELSE IF (K.EQ.4) THEN
                  VISOUT(1,I) = (-DSIN*IV(1) + DCOS*IV(2) - DCOS*IV(3) -
     *               DSIN*IV(4)) / 2.0
                  VISOUT(2,I) =  (DSIN*RV(1) - DCOS*RV(2) + DCOS*RV(3) +
     *               DSIN*RV(4)) / 2.0
                  END IF
 40            CONTINUE
 60         CONTINUE
 70      CONTINUE
C                                       See if any good data.
      DROP = .NOT. GOOD
C
 999  RETURN
      END
