      SUBROUTINE SDGET (OPCODE, RPARM, VIS, IERR)
C-----------------------------------------------------------------------
C! Reads single dish data with optional calibration and flagging
C# SDish Calibration IO-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2007, 2011
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
C   Subroutine to obtain data from a single dish data base with
C   optional application of flaging and/or calibration and/or pointing
C   information.  Reads data with a large variety of selection criteria
C   and will reformat the data as necessary.  Does many of the startup
C   operations, finds Single dish uv like data file etc., reads CATBLK
C   and updates the /UVHDR/ common to reflect the output rather than
C   input data.
C   Input:
C      OPCODE   C*4      Opcode -
C                        'INIT' => Open files Initialize I/O.
C                        'READ' => Read next specified record.
C                        'CLOS' => Close files.
C   Inputs via common /SELCAL/  (Includes DSEL,CSEL.INC)
C      UNAME    C*12     AIPS name of input file.
C      UCLAS    C*6      AIPS class of input file.
C      UDISK    R        AIPS disk of input file.
C      USEQ     R        AIPS sequence of input file.
C      SOURCS   C(30)*16 Names of up to 30 sources, '*' => all
C                        First character of name '-' => all except those
C                        specified.
C      TIMRNG   R(8)     Start day, hour, min, sec, end day, hour,
C                        min, sec.   0's => all.
C      UVRA     R(2)     Range of RA (1) and dec (2) in degrees about
C                        the value in CATBLK at time of READ call to
C                        SDGET.  0=>all.
C      STOKES   C*4      Stokes types wanted.
C                        'I','Q','U','V','R','L','IQU','IQUV'
C                        '    '=> Leave data in same form as in input.
C      BCHAN    I        First channel number selected, 1 rel. to first
C                        channel in data base.    0 => all
C      ECHAN    I        Last channel selected.   0 => all
C      BIF      I        First IF number selected, 1 rel. to first
C                        IF in data base.    0 => all
C      EIF      I        Last IF selected.   0 => all
C      DOCAL    L        If true apply calibration, else not.
C      SUBARR   I        Subarray desired, 0 => all
C      FGVER    I        FLAG file version number, if < 0 then
C                        NO flagging is applied. 0 => use highest
C                        numbered table.
C      CLUSE    I        Cal (CS) file version number to apply.
C   Output:
C      RPARM    R(*)     Random parameter array of datum.
C      VIS      R(3,*)   Regular portion of data array.
C      IERR     I        Error code: 0 => OK,
C                           -1 => end of data
C                           >0 => failed, abort process.
C   Output in common /SELCAL/: The default values will be filled in
C   if null values were specified.
C      CATBLK   I(256)   Catalog header block, describes the output
C                        data rather than input.
C      NPRMIN   I        Number or random parameters in the input data.
C      TRANSL   L        If true translate data to requested Stokes'
C      CNTREC   I(2,3)   Record counts:
C                        (1&2,1) Previously flagged (partly, fully)
C                        (1&2,2) Flagged due to gains (part, full)
C                        (1&2,3) Good selected (part, full)
C      ISCMP    L         True if input data is compressed.
C      KLOCSU   I         0-rel random parm. pointer for source in input
C                         file.
C      KLOCFQ   I         0-rel random parm. pointer for FQ id in input
C                         file.
C      KLOCIF   I         0-rel random parm. pointer for IF in input
C                         file.
C      KLOCFY   I         0-rel random parm. pointer for freq. in input
C                         file.
C      KLOCWT   I         0-rel random parm. pointer for weight in
C                         input file.
C      KLOCSC   I         0-rel random parm. pointer for scale in
C                         input file.
C   Usage notes:
C    1) Includes DSEL.INC and CSEL.INC should be declared in the main
C       program or at a level that they will not be overlaid while
C       SDGET is in use (ie. between the 'INIT' and 'CLOS' calls)
C    2) If no sorting is done SDGET uses AIPS luns 25, 28, 29 and 30
C      (1 map, 3 non map files).  If sorting is done (usually possible)
C      then 8 map and 3 non map files are used (mostly on OPCODE='INIT')
C      and LUNs 16,17,18,19,20,21,22,23,24,25, 28,29,30.
C    3) OPCODE = 'INIT' does the following:
C      - The catalgue data file is located and the catalog header
C        record is read.
C      - The index file (if any) is initialized.
C      - The flag file (if any) is initialized and sorted if necessary
C        (Must be in time order).
C      - The CS table (if any) is initialized.
C      - I/O to the input file is initialized.
C            The following LUNs may be used but will be closed on
C        return: 16, 17, 18, 19, 20, 21, 22, 23, 24
C            The following LUNs may be used but will be open on
C        return: 25 (uv data), 28 (NX table), 29 (CS table),
C                30 (FG table).
C            NO data are returned from this call.
C    4) OPCODE = 'READ' reads one  record properly selected,
C       transformed (e.g. I pol.), calibrated and edited as requested
C       in the call with OPCODE = 'INIT'
C    5) OPCODE = 'CLOS' closes all files used by SDGET which are still
C       open.  No data are returned.
C    6) If DOCAL is true then the common array CNTREC will contain the
C       counts of records which are good or fully or partly flagged
C       both previously and due to flagged gain solutions.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      REAL      RPARM(*), VIS(3,*)
      INTEGER   IERR
C
      CHARACTER STAT*4, TYPTMP*2
      HOLLERITH CATH(256)
      INTEGER   IROUND, IUBIND, JERR, KEY(2,2), NFGBUF, I, ICNO, NREAD,
     *   BO, FRSRED, MMS, MMIF, MMCH, KEYSUB(2,2)
      LOGICAL   T, F, GOTIT
      REAL      FKEY(2,2), TIMLST
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATUV, CATH)
      SAVE TYPTMP
      DATA T, F /.TRUE.,.FALSE./
      DATA KEY /4,0,1,0/,  FKEY /1.0,0.0,1.0,0.0/, KEYSUB /4*1/
      DATA BO /1/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Check OPCODE
      IF (OPCODE.EQ.'READ') GO TO 300
      IF (OPCODE.EQ.'INIT') GO TO 10
      IF (OPCODE.EQ.'CLOS') GO TO 900
C                                       Unknown OPCODE
      WRITE (MSGTXT,1000) OPCODE
      GO TO 990
C                                       INIT.
C                                       Find file.
 10   IF (NCFILE.GE.FILIST) THEN
         MSGTXT = 'SDGET: FILE LIST OVERFLOWS'
         IERR = 9
         GO TO 990
         END IF
      NFGBUF = 512
      IUCNO = 1
      IUDISK = IROUND (UDISK)
      IUSEQ = IROUND (USEQ)
      TYPTMP = '  '
      CALL CATDIR ('SRCH', IUDISK, IUCNO, UNAME, UCLAS, IUSEQ, TYPTMP,
     *   NLUSER, STAT, UBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, UNAME, UCLAS, IUSEQ, IUDISK, NLUSER
         GO TO 990
      ELSE IF ((TYPTMP.NE.'UV') .AND. (TYPTMP.NE.'SC')) THEN
         IERR = 5
         WRITE (MSGTXT,1011) IERR, UNAME, UCLAS, IUSEQ, TYPTMP
         GO TO 990
         END IF
C                                       Read CATUV and mark.
 20   CALL CATIO ('READ', IUDISK, IUCNO, CATUV, 'READ', UBUFF, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         GO TO 990
C                                       Mark in CFILES
 30   NCFILE = NCFILE + 1
      FVOL(NCFILE) = IUDISK
      FCNO(NCFILE) = IUCNO
      FRW(NCFILE) = 0
C                                       Save CFILES no. for later
      ICNO = NCFILE
C                                       Set /UVHDR/
      CALL COPY (256, CATUV, CATBLK)
      CALL UVPGET (IERR)
C                                       Interferometer data - barf.
      IF (TYPUVD.LE.0) THEN
         IERR = 6
         MSGTXT = 'SDGET: ERROR, DATA IS INTEFEROMETER TYPE DATA'
         GO TO 990
         END IF
C                                       Save reference frequency
      UVFREQ = FREQ
C                                       Save no. random parms.
      NPRMIN = NRPARM
C                                       Save source no. pointer
      KLOCSU = ILOCSU
C                                       Is FREQSEL rand parm present
      DOFQSL = ILOCFQ .GT. 0
      KLOCFQ = ILOCFQ
C                                       Save relevant pointers for
C                                       flagging
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
C                                       Set defaults for file
      UDISK = IUDISK
      USEQ = CATUV(KIIMS)
C                                       Set LUNs
      IULUN = 25
      IXLUN = 28
      ICLUN = 29
      IFLUN = 30
      IBLUN = 40
C                                       Check defaults
C                                       Flagging
      DOFLAG = FGVER.GE.0
C                                       Timerange
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
C                                       Set time range.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
C                                       RA, dec range.
      IF (ABS (UVRA(1)+UVRA(2)) .LT. 1.0E-10) THEN
         UVRA(1) = 1.0E10
         UVRA(2) = 1.0E10
         END IF
C                                       Channels
      IF (BCHAN.LE.0) BCHAN = 1
      IF (ECHAN.LE.0) ECHAN = CATUV(KINAX+JLOCF)
      IF (BCHAN.GT.CATUV(KINAX+JLOCF)) BCHAN = CATUV(KINAX+JLOCF)
      IF (ECHAN.GT.CATUV(KINAX+JLOCF)) ECHAN = CATUV(KINAX+JLOCF)
C                                       IF
      IF (BIF.LE.0) BIF = 1
      IF ((EIF.LE.0) .AND. (JLOCIF.GT.0)) EIF = CATUV(KINAX+JLOCIF)
      IF (EIF.LE.0) EIF = 1
      IF ((JLOCIF.GT.0) .AND. (BIF.GT.CATUV(KINAX+JLOCIF)))
     *   BIF = CATUV(KINAX+JLOCIF)
      IF ((JLOCIF.GT.0) .AND. (EIF.GT.CATUV(KINAX+JLOCIF)))
     *   EIF = CATUV(KINAX+JLOCIF)
C                                       Check sizes
      MMS  = MAX (1, CATBLK(KINAX+JLOCS))
      MMCH = MAX (1, CATBLK(KINAX+JLOCF))
      IF (JLOCIF.GT.0) THEN
         MMIF = MAX (1, CATBLK(KINAX+JLOCIF))
      ELSE
         MMIF = 1
         END IF
      IF ((MMCH.GT.MAXCHA) .OR. (MMIF.GT.MAXIF) .OR.
     *   (MMS*MMIF*MMCH.GT.MAXCIF)) THEN
         IERR = 1
         MSGTXT = 'SDGET:RECORDS TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
C                                       reset STOKES to match
      CALL FNDPOL (STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       See if data to be translated
      TRANSL = .NOT.((STOKES.EQ.' ') .OR.
     *   ((STOKES.EQ.'HALF') .AND. (ICOR0.LT.0)) .OR.
     *   ((STOKES.EQ.'FULL') .AND. (ICOR0.LT.0)))
C                                       Set poln. code.
      PMODE = 0
C                                       Init SOURCE file.
      CALL SOUFIL (IERR)
      TMFLST = -1.0E20
      NUMFLG = 0
      KNCOR = NCOR
      KCOR0 = ICOR0
      KNCF = INCF / CATUV(KINAX)
      KNCIF = INCIF / CATUV(KINAX)
      KNCS = INCS / CATUV(KINAX)
      UBUFSZ = UVBFSL * 2
      TIMORD = ISORT(1:1).EQ.'T'
C                                       Insist on T* Sort if calibration
C                                       or flagging being applied.
      IF ((DOCAL) .AND. (.NOT.TIMORD)) THEN
         IERR = 1
         WRITE (MSGTXT,1050) ISORT
         END IF
C                                       Init Flag file
      IF (DOFLAG) THEN
         MSGSUP = 32000
         CALL FGREFM (IUDISK, IUCNO, FGVER, CATUV, IFLUN, JERR)
         CALL FLGINI ('READ', FGBUFF, IUDISK, IUCNO, FGVER,
     *      CATUV, IFLUN, IFGRNO, FGKOLS, FGNUMV, JERR)
         MSGSUP = 0
         IF (JERR.NE.0) DOFLAG = F
         END IF
C                                       Resort if necessary.
      IF ((DOFLAG) .AND. (FGBUFF(43).NE.KEY(1,1))) THEN
C                                       Sort to time order.
         CALL TABIO ('CLOS', 0, NREAD, UBUFF, FGBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABSRT (IUDISK, IUCNO, 'FG', FGVER, FGVER, KEY, KEYSUB,
     *      FKEY, FGBUFF, CATUV, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Re initialize.
         CALL FLGINI ('READ', FGBUFF, IUDISK, IUCNO, FGVER,
     *      CATUV, IFLUN, IFGRNO, FGKOLS, FGNUMV, IERR)
         END IF
C                                       Set up data selection.
C                                       Compressed data?
      ISCMP = CATBLK(KINAX).EQ.1
C                                       Restore CATUV to /MAPHDR/ -
C                                       TABSRT will destroy contents
      CALL COPY (256, CATUV, CATBLK)
      CALL DGINIT (STOKES, BCHAN, ECHAN, BIF, EIF, MVIS, JADR, SELFAC,
     *   ALLWT, PMODE, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (MVIS(1).GT.MAXCIF) THEN
         IERR = 1
         MSGTXT = 'SDGET: VISIBILITIES TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
C                                       Init Cal file - May call TABSRT
      CALL GACSIN (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find pointers for compressed
C                                       data
      IF (ISCMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATUV(KIPCN), CATH(KHPTP), KLOCWT,
     *      JERR)
C                                       Must have this one
         IF ((JERR.NE.0) .OR. (KLOCWT.LT.0)) THEN
            IERR = 5
            MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
            GO TO 990
            END IF
         CALL AXEFND (8, 'SCALE   ', CATUV(KIPCN), CATH(KHPTP), KLOCSC,
     *      JERR)
C                                       Get data decompression pointers
         CALL CMPARM (BIF, EIF, BCHAN, ECHAN, BCHANS, ECHANS,
     *      NDECMP, DECMP)
      ELSE
         KLOCWT = -1
         KLOCSC = -1
         END IF
C                                       Restore CATUV to /MAPHDR/ -
C                                       TABSRT will destroy contents
      CALL COPY (256, CATUV, CATBLK)
C                                       Init INDEX file.
      CALL INDXIN (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open UV file
      CALL ZPHFIL (TYPTMP, IUDISK, IUCNO, 1, UFILE, IERR)
      CALL ZOPEN (IULUN, IUFIND, IUDISK, UFILE, T, F, T, IERR)
      IF (IERR.LE.0) GO TO 40
         WRITE (MSGTXT,1030) IERR
         GO TO 990
C                                       Init I/O to uvfile
 40   NREAD = LSTVIS - FSTVIS + 1
      FRSRED = FSTVIS - 1
      LRECIN = LREC
C                                       No data
      IF (NREAD.LT.1) THEN
         IERR = -1
         MSGTXT = 'SDGET: NO DATA SELECTED'
         GO TO 990
         END IF
      CALL UVINIT ('READ', IULUN, IUFIND, NREAD, FRSRED, LRECIN, 1,
     *   UBUFSZ, UBUFF, BO, IUBIND, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1045) IERR
         GO TO 990
         END IF
C                                       Correct CATBLK to output data
      CALL DGHEAD
C                                       Update /UVHDR/ common
      CALL UVPGET (JERR)
C                                       Init "last time"
      TIMLST = -1.0E10
      GO TO 999
C-----------------------------------------------------------------------
C                                       READ/select datum
C                                       Get next selected datum
 300  CALL DGETSD (RPARM, VIS, TIMLST, JERR)
C                                       Check for end of file
      IERR = JERR
      IF (JERR.EQ.4) IERR = -1
      GO TO 999
C-----------------------------------------------------------------------
C                                       Close files
C                                       data file
 900  MSGSUP = 32000
      CALL ZCLOSE (IULUN, IUFIND, IERR)
      MSGSUP = 0
C                                       INDEX file
      IF (INXRNO.GT.0) CALL TABIO ('CLOS', 0, NREAD, UBUFF, NXBUFF,
     *   IERR)
C                                       Cal file
      IF (DOCAL) CALL TABIO ('CLOS', 0, NREAD, UBUFF, CLBUFF, IERR)
C                                       FLAG file
      IF (DOFLAG) CALL TABIO ('CLOS', 0, NREAD, UBUFF, FGBUFF, IERR)
C                                       Clear status
      STAT = 'CLRD'
      CALL CATDIR ('CSTA', IUDISK, IUCNO, UNAME, UCLAS, IUSEQ, TYPTMP,
     *   NLUSER, STAT, UBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      IERR = 0
C                                       Clear /CFILES/ entry.
      GOTIT = F
      DO 910 I = 1,NCFILE
         GOTIT = GOTIT .OR.
     *      (((FVOL(I).EQ.IUDISK).AND.(FCNO(I).EQ.IUCNO)) .AND.
     *      (FRW(I).EQ.0))
         IF (.NOT.GOTIT) GO TO 910
            FVOL(I) = FVOL(I+1)
            FCNO(I) = FCNO(I+1)
            FRW(I) = FRW(I+1)
 910        CONTINUE
      IF (GOTIT) NCFILE = NCFILE - 1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDGET: UNKNOWN OPCODE: ',A4)
 1010 FORMAT ('SDGET: ERROR ',I3,' FINDING FILE:', A12,'.',A6,
     *   '.',I3,' DISK=',I2,' USER=',I5)
 1011 FORMAT ('SDGET: FOUND FILE:', A12,'.',A6,'.',I4,' TYPE ',A2,
     *   ' NOT UV OR SC')
 1020 FORMAT ('SDGET: ERROR ',I3,' READING DATA CATALOGUE HEADER')
 1030 FORMAT ('SDGET: ERROR ',I3,' OPENING UV DATA FILE')
 1040 FORMAT ()
 1045 FORMAT ('SDGET: ERROR ',I3,' INITING SINGLE DISH DATA FILE')
 1050 FORMAT ('SDGET: DATA IN WRONG SORT ORDER = ',A2,' NOT T*')
      END
