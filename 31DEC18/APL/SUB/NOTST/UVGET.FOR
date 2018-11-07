      SUBROUTINE UVGET (OPCODE, RPARM, VIS, IERR)
C-----------------------------------------------------------------------
C! Read UV data with optional calibration, editing, selection, etc.
C# UV IO-appl Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2004-2007, 2010-2012, 2015-2016, 2018
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
C   Subroutine to obtain data from a data base with optional application
C   of flaging and/or calibration information.  Reads data with a large
C   variety of selection criteria and will reformat the data as
C   necessary.  Does many of the startup operations, finds uv data file
C   etc, reads CATBLK and updates the DUVH.INC commons to reflect the
C   output rather than input data.
C      Most of the input to UVGET is through the commons in DSEL.INC;
C   the initial (default) values of these may be set using routine
C   SELINI.
C   Input:
C      OPCODE   C*4       Opcode:
C                         'INIT' => Open files Initialize I/O.
C                         'READ' => Read next specified record.
C                         'CLOS' => Close files.
C   Inputs via common /SELCAL/  (Include DSEL.INC)
C      UNAME    C*12      AIPS name of input file.
C      UCLAS    C*6       AIPS class of input file.
C      UDISK    R         AIPS disk of input file.
C      USEQ     R         AIPS sequence of input file.
C      SOURCS   C(30)*16  Names of up to 30 sources, *=>all
C                         First character of name '-' => all except
C                         those specified.
C      TIMRNG   R(8)      Start day, hour, min, sec, end day, hour,
C                         min, sec. 0's => all
C      UVRNG    R(2)      Minimum and maximum baseline lengths in
C                         1000's wavelengths. 0's => all
C      STOKES   C*4       Stokes types wanted.
C                         'I','Q','U','V','R','L','IQU','IQUV'
C                         '    '=> Leave data in same form as in input.
C      BCHAN    I         First channel number selected, 1 rel. to first
C                         channel in data base. 0 => all
C      ECHAN    I         Last channel selected. 0=>all
C      BIF      I         First IF number selected, 1 rel. to first
C                         IF in data base. 0 => all
C      EIF      I         Last IF selected. 0=>all
C      DOCAL    L         If true apply calibration, else not.
C      DOPOL    I         If true then correct for feed polarization
C                         based on antenna file info.
C      DOACOR   L         True if autocorrelations are requested.
C      DOWTCL   L         True if weight calibration wanted.
C      DOFQSL   L         True if FREQSEL random parm present (false)
C      FRQSEL   I         Default FQ table entry to select (-1)
C      SELBAN   R         Bandwidth (Hz) to select (-1.0)
C      SELFRQ   D         Frequency (Hz) to select (-1.0)
C      DOBAND   I         >0 if bandpass calibration. (-1)
C      PBPBUF   L         Pointer to memory for bandpass cal application
C      DOSMTH   I         if smoothing requested: 1 before, 1 after BP
C      SMOOTH   R(3)      Smoothing parameters (0.0s)
C      DXTIME   R         Integration time (days). Used when applying
C                         delay corrections to correct for delay error.
C      ANTENS   I(50)     List of antennas selected, 0=>all,
C                         any negative => all except those specified
C      SUBARR   I         Subarray desired, 0=>all which does not work
C                         if any calibration is applied.
C      FGVER    I         FLAG file version number, if < 0 then
C                         NO flagging is applied. 0 => use highest
C                         numbered table.
C      CLUSE    I         Cal (CL or SN) file version number to apply.
C      BLVER    I         BL Table to apply .le. 0 => none
C      BPVER    I         BP table to apply .le. 0 => none
C      INITVS   I         First visibility number to read in a single
C                         source file (default = 1).
C   Output:
C      RPARM    R(*)      Random parameter array of datum.
C      VIS      R(3,*)    Regular portion of visibility data.
C      IERR     I         Error code: 0 => OK,
C                             -1 => end of data
C                             >0 => failed, abort process.
C   Output in commons in DSEL.INC: The default values will be filled in
C   if null values were specified.
C      UVFREQ   D         Frequency corresponding to u,v,w
C      CATBLK   I(256)    Catalog header block, describes the output
C                         data rather than input.
C      NPRMIN   I         Number or random parameters in the input data.
C      TRANSL   L         If true translate data to requested Stokes'
C      CNTREC   I(2,3)    Record counts:
C                         (1&2,1) Previously flagged (partly, fully)
C                         (1&2,2) Flagged due to gains (part, full)
C                         (1&2,3) Good selected (part, full)
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
C    1) Include DSEL.INC should be declared in the main program or at a
C       level that they will not be overlaid while UVGET is in use (ie.
C       between the 'INIT' and 'CLOS' calls). SELINI can be used to
C       initialize the control variables in these commons.
C    2) If no sorting is done UVGET uses AIPS luns 25, 28, 29 and 30
C      (1 map, 3 non map files).  If sorting is done (usually possible)
C      then 8 map and 3 non map files are used (mostly on OPCODE='INIT')
C      and LUNs 16,17,18,19,20,21,22,23,24,25, 28,29,30,40,42,43,44,45.
C    3) OPCODE = 'INIT' does the following:
C      - The catalgue data file is located and the catalog header
C        record is read.
C      - The source file (if any) is read.
C      - The index file (if any) is initialized.
C      - The flag file (if any) is initialized and sorted if necessary
C        (Must be in time order).
C      - The gain table (if any) is initialized.
C      - The bandpass table (if any) is initialized
C      - The smoothing convolution table (if any) is initialized
C      - I/O to the input file is initialized.
C            The following LUNs may be used but will be closed on
C        return: 16, 17, 18, 19, 20, 21, 22, 23, 24
C            The following LUNs may be used but will be open on
C        return: 25 (uv data), 28 (NX table), 29 (CL or SN table),
C                30 (FG table), 40 (BL table), 41 (BP table).
C            NO data are returned from this call.
C    4) OPCODE = 'READ' reads one visibility record properly selected,
C       transformed (e.g. I pol.), calibrated and edited as requested
C       in the call with OPCODE = 'INIT'
C    5) OPCODE = 'CLOS' closes all files used by UVGET which are still
C       open.  No data are returned.
C    6) If DOCAL is true then the common array CNTREC will contain the
C       counts of records which are good or fully or partly flagged
C       both previously and due to flagged gain solutions.
C    7) Only one subarray can be calibrated at a time if DOPOL is true.
C       This is because the polarization information for only one
C       subarray is kept at a time.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      REAL      RPARM(*), VIS(3,*)
      INTEGER   IERR
C
      CHARACTER STAT*4, TYPTMP*2, ARNAME*8, KEYWRD*8
      INTEGER   IROUND, IUBIND, JERR, KEY(2,2), NFGBUF, I, ICNO, LENBU,
     *   NREAD, BO, VER, MMS, MMCH, MMIF, MSGSAV, NKEY, NREC, NCOL,
     *   KLOCS, KEYTYP, KEYV, NUMKEY, KEYSUB(2,2), LUDISK, LUCNO, LFGVER
      HOLLERITH CATH(256), KEYVAL(2)
      LOGICAL   T, F, GOTIT, TABLE, FQEXIS, FITASC, MATCH, DOWARN, SAME
      REAL      FKEY(2,2), TIMLST
      DOUBLE PRECISION DMULT
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DPDC.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DCHND.INC'
      EQUIVALENCE (CATUV, CATH)
      SAVE TYPTMP, DOWARN, LUDISK, LUCNO, LFGVER, TIMLST
      DATA DOWARN /.TRUE./
      DATA T, F /.TRUE.,.FALSE./
      DATA KEY  /5,0,1,0/
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
      DATA BO /1/
      DATA LUDISK, LUCNO, LFGVER /-1, -1, -100/
C-----------------------------------------------------------------------
      IERR = 1
      MSGSAV = MSGSUP
C                                       Check OPCODE
      IF (OPCODE.EQ.'CLOS') GO TO 900
      IF (OPCODE.EQ.'READ') GO TO 300
      IF (OPCODE.NE.'INIT') THEN
         WRITE (MSGTXT,1000) OPCODE
         GO TO 990
         END IF
C                                       INIT.
C                                       Find file.
      IF (NCFILE.GE.FILIST) THEN
         MSGTXT = 'UVGET: FILE LIST OVERFLOWS'
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
         WRITE (MSGTXT,1011) UNAME, UCLAS, IUSEQ, TYPTMP
         GO TO 990
         END IF
C                                       Read CATUV and mark 'READ'.
C                                       (Must be WRIT to create new
C                                       Cal table.)
      CALL CATIO ('READ', IUDISK, IUCNO, CATUV, 'READ', UBUFF, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.LT.5) THEN
            WRITE (MSGTXT,1020) IERR
            GO TO 990
         ELSE
            WRITE (MSGTXT,1021) IERR
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Mark in CFILES
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = IUDISK
      FCNO(NCFILE) = IUCNO
      FRW(NCFILE) = 0
C                                       same as last time?
      SAME = (IUDISK.EQ.LUDISK) .AND. (IUCNO.EQ.LUCNO)
      LUDISK = IUDISK
      LUCNO = IUCNO
C                                       Save CFILES no. for later
      ICNO = NCFILE
C                                       Set /UVHDR/
      CALL COPY (256, CATUV, CATBLK)
      CALL UVPGET (IERR)
C                                       Save frequency of u,v,w
      UVFREQ = FREQ
C                                       Save no. random parms.
      NPRMIN = NRPARM
C                                       Save source no. pointer
      KLOCSU = ILOCSU
C                                       Is FREQSEL rand parm present
      DOFQSL = ILOCFQ.GT.0
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
      IPLUN = 42
      LUNSBP = 43
      IQLUN = 44
      IANLUN = 45
C                                       be friendly
C                                       Is there an FQ table
      IF (DOFQSL) THEN
         VER = 1
         CALL ISTAB ('FQ', IUDISK, IUCNO, VER, IQLUN, FQBUFF,
     *      TABLE, FQEXIS, FITASC, IERR)
         IF (.NOT.FQEXIS) THEN
            SELBAN = -1.0
            SELFRQ = -1.D0
            FRQSEL = -1
            GO TO 35
C                                       ISTAB reads 1st record
         ELSE
            IF (FQBUFF(5).LE.1) FRQSEL = 1
            END IF
         END IF
C                                       Check defaults
      IF ((SELBAN.LE.0.0) .AND. (SELFRQ.LE.0.D0)) THEN
         IF (DOFQSL .AND. (FRQSEL.LE.0)) THEN
            WRITE (MSGTXT,1070) FRQSEL
            IERR = 1
            GO TO 990
            END IF
         END IF
C                                       Frequency selection criteria
      IF ((SELBAN.GT.0.0) .OR. (SELFRQ.GT.0.D0)) THEN
C                                       Do SELBAN/SELFRQ match
C                                       FQ entries?
         VER = 1
         CALL FQMATC (IUDISK, IUCNO, CATUV, IQLUN, SELBAN, SELFRQ,
     *      MATCH, FRQSEL, IERR)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1060)
            IERR = 1
            GO TO 990
            END IF
         END IF
C                                       Flagging
 35   CALL FNDEXT ('FG', CATBLK, VER)
      DOFLAG = (FGVER.GE.0) .AND. (VER.GT.0)
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
C                                       UV range
      DOUVRA = (UVRNG(1).GT.0.0) .OR. (UVRNG(2).GT.0.0)
      IF (UVRNG(2).LE.0.0) UVRNG(2) = 1.0E10
      UVRA(1) = 1.0E6 * UVRNG(1) * UVRNG(1)
      UVRA(2) = 1.0E6 * UVRNG(2) * UVRNG(2)
      UVRA(1) = UVRA(1) - 1.0E-10
C                                       Channels
      IF (BCHAN.LE.0) BCHAN = 1
      IF (ECHAN.LE.0) ECHAN = CATUV(KINAX+JLOCF)
      IF (BCHAN.GT.CATUV(KINAX+JLOCF)) BCHAN = CATUV(KINAX+JLOCF)
      IF (ECHAN.GT.CATUV(KINAX+JLOCF)) ECHAN = CATUV(KINAX+JLOCF)
      BCHANS = BCHAN
      ECHANS = ECHAN
C                                       IF
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = MAX (1, MIN (BIF, CATUV(KINAX+JLOCIF)))
         IF (EIF.LT.BIF) EIF = CATUV(KINAX+JLOCIF)
         EIF = MAX (BIF, MIN (EIF, CATUV(KINAX+JLOCIF)))
         END IF
C                                       uvrange needs FQ info now
      CHNIF = 1
      FOFF(1) = 0.0D0
      IF (DOUVRA) THEN
         VER = 1
         CALL CHNDAT ('READ', FQBUFF, IUDISK, IUCNO, VER, CATUV, IQLUN,
     *      CHNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
         IF ((IERR.NE.0) .AND. (EIF.GT.1)) THEN
            MSGTXT = 'UVRANGE APPLIED AT HEADER FREQUENCY ONLY'
            CALL MSGWRT (7)
            END IF
         END IF
C                                       Check sizes
      MMS  = MAX (1, CATBLK(KINAX+JLOCS))
      MMCH = MAX (1, CATBLK(KINAX+JLOCF))
      IF (JLOCIF.GT.0) THEN
         MMIF = MAX (1, CATBLK(KINAX+JLOCIF))
      ELSE
         MMIF = 1
         END IF
      IF ((MMCH.GT.MAXCHA) .OR. (MMIF.GT.MAXIF) .OR.
     *   (3*MMS*MMIF*MMCH.GT.UVBFSS)) THEN
         IERR = 1
         MSGTXT = 'UVGET: VISIBILITIES TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
C                                       Convert STOKES depending on
C                                       present data types
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
      IF ( IERR .NE. 0 ) GO TO 999
C                                       check on BL table
      CALL FNDEXT ('BL', CATBLK, VER)
      IF (VER.LE.0) BLVER = -1
      DOBL = BLVER.GE.0
C                                       Insist on TB Sort if calibration
C                                       or flagging being applied.
      TIMORD = ISORT(1:1).EQ.'T'
      IF (DOCAL .OR. DOBL .OR. (DOBAND.GT.1)) THEN
         IF (.NOT.TIMORD) THEN
            IERR = 1
            WRITE (MSGTXT,1050) ISORT
            GO TO 990
            END IF
         END IF
      IF (DOCAL .OR. DOBL .OR. (DOPOL.GT.0) .OR. (DOBAND.GT.0)) THEN
         CALL FNDEXT ('AN', CATBLK, VER)
         VER = MAX (1, VER)
         IF (VER.LE.1) SUBARR = 1
         IF ((SUBARR.LT.1) .OR. (SUBARR.GT.VER)) THEN
            IERR = 1
            WRITE (MSGTXT,1051) SUBARR
            GO TO 990
            END IF
         CALL TABINI ('READ', 'AN', IUDISK, IUCNO, SUBARR, CATBLK,
     *      IFLUN, NKEY, NREC, NCOL, UBUFF, FGBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1052) SUBARR, IERR
            GO TO 990
            END IF
         KEYWRD = 'ARRNAM'
         CALL TABKEY ('READ', KEYWRD, 1, FGBUFF, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1053) SUBARR, IERR
            CALL MSGWRT (7)
         ELSE
            CALL H2CHR (8, 1, KEYVAL, ARNAME)
            IF (ARNAME.EQ.'EVLA') THEN
               NUMKEY = 1
               MSGSUP = 32000
               CALL CATKEY ('READ', IUDISK, IUCNO, 'CROSSPOW', NUMKEY,
     *            KLOCS, KEYV, KEYTYP, UBUFF, IERR)
               MSGSUP = MSGSAV
               IF (IERR.NE.0) KEYV = 1
               IF (KEYV.GE.1) DOWTCL = .FALSE.
               END IF
            END IF
         CALL TABIO ('CLOS', 0, 1, UBUFF, FGBUFF, IERR)
         END IF
C                                       Init Flag file
      TMFLST = -1.0E20
      NUMFLG = 0
      KNCOR = NCOR
      KCOR0 = ICOR0
      KNCF = INCF / CATUV(KINAX)
      KNCIF = INCIF / CATUV(KINAX)
      KNCS = INCS / CATUV(KINAX)
      UBUFSZ = UVBFSL * 2
      IF (DOFLAG) THEN
         MSGSUP = 32000
C                                       Reformat table?
         CALL FGREFM (IUDISK, IUCNO, FGVER, CATUV, IFLUN, JERR)
         CALL FLGINI ('READ', FGBUFF, IUDISK, IUCNO, FGVER, CATUV,
     *      IFLUN, IFGRNO, FGKOLS, FGNUMV, JERR)
         MSGSUP = MSGSAV
         IF (JERR.NE.0) DOFLAG = F
C                                       Resort if necessary.
         IF (DOFLAG .AND. (FGBUFF(43).NE.KEY(1,1))) THEN
C                                       Sort to time order.
            CALL TABIO ('CLOS', 0, NREAD, UBUFF, FGBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABSRT (IUDISK, IUCNO, 'FG', FGVER, FGVER, KEY, KEYSUB,
     *         FKEY, FGBUFF, CATUV, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Re initialize.
            CALL FLGINI ('READ', FGBUFF, IUDISK, IUCNO, FGVER, CATUV,
     *         IFLUN, IFGRNO, FGKOLS, FGNUMV, IERR)
            END IF
         END IF
      IF (((.NOT.SAME) .OR. (LFGVER.NE.FGVER)) .AND. (FGVER.GT.-99))
     *   THEN
         IF (DOFLAG) THEN
            WRITE (MSGTXT,1035) FGVER
         ELSE
            MSGTXT = 'UVGET: doing no flagging this time'
            END IF
         CALL MSGWRT (3)
         LFGVER = FGVER
         END IF
C                                       Set up data selection.
C                                       Restore CATUV to /MAPHDR/ -
C                                       TABSRT will destroy contents
      CALL COPY (256, CATUV, CATBLK)
      CALL DGINIT (STOKES, BCHAN, ECHAN, BIF, EIF, MVIS, JADR, SELFAC,
     *   ALLWT, PMODE, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (MVIS(1).GT.MAXCIF) THEN
         IERR = 1
         MSGTXT = 'UVGET: VISIBILITIES TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
C                                       Init spectral smoothing table
      CALL SETSM (IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((ECHANS.GT.ECHAN) .OR. (BCHANS.LT.BCHAN)) THEN
         DMULT = ECHANS - BCHANS + 1.0D0
         DMULT = DMULT / (ECHAN-BCHAN+1.0D0)
         MVIS(1) = MVIS(1) * DMULT + 0.1D0
         END IF
C                                       this overflows
C     IF (ECHAN-BCHAN+1.GT.0) MVIS(1) = MVIS(1) * (ECHANS-BCHANS+1) /
C    *   (ECHAN-BCHAN+1)
C                                       Set up for polarization
C                                       correction.
      IF (DOPOL.GT.0) THEN
         CALL POLSET (IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Init BP file - May call TABSRT
      IF ((DOBAND.GT.0) .AND. (PBPBUF.EQ.0)) THEN
         CALL BPASET (IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Init Cal file - May call TABSRT
      CALL GAININ (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Compressed data?
      ISCMP = CATBLK(KINAX).EQ.1
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
      MSGSUP = 32000
      CALL INDXIN (IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 999
C                                       Warning on multi-source no NX
      IF ((INXRNO.LT.0) .AND. (KLOCSU.GE.0) .AND. (DOWARN)) THEN
         MSGTXT = '****** WARNING: NO NX TABLE IN MULTI-SOURCE FILE'
         CALL MSGWRT (7)
         MSGTXT = '******    SELECTION AND FLAGGING BY SOURCE DISABLED'
         CALL MSGWRT (7)
         MSGTXT = '******    YOU SHOULD RUN INDXR'
         CALL MSGWRT (7)
         DOWARN = .FALSE.
         END IF
C                                       Open UV file
      CALL ZPHFIL (TYPTMP, IUDISK, IUCNO, 1, UFILE, IERR)
      CALL ZOPEN (IULUN, IUFIND, IUDISK, UFILE, T, F, T, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
C                                       Init I/O to uvfile
      IF (LSTVIS.GT.CATUV(KIGCN)) THEN
         LSTVIS = CATUV(KIGCN)
         FSTVIS = MIN (FSTVIS, MAX (1, LSTVIS-1))
         END IF
      NREAD = LSTVIS - FSTVIS + 1
      FSTRED = FSTVIS - 1
      FSTRD3 = FSTRED
      RECNO3 = FSTRED
      NREAD3 = NREAD
      FSTVS3 = FSTVIS - 1
      LSTVS3 = LSTVIS
      LRECIN = LREC
      LENBU = 0
C                                       No data
      IF (NREAD.LT.1) THEN
         IERR = -1
         MSGTXT = 'UVGET: NO DATA SELECTED'
         GO TO 990
         END IF
C                                       init read
      CALL UVINIT ('READ', IULUN, IUFIND, NREAD, FSTRED, LRECIN, LENBU,
     *   UBUFSZ, UBUFF, BO, IUBIND, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1045) IERR
         GO TO 990
         END IF
      DOUVIN = T
      LENBU3 = LENBU
      FSTVIS = 0
      LSTVIS = 0
CCCC
C                                       Look out for LL only.
C      IF ((ICOR0 .EQ. -2) .AND. (STOKES .EQ. 'HALF'))
C     *   PMODE = 9
CCCC
C                                       Correct CATBLK to output data
      CALL DGHEAD
C                                       Update /UVHDR/ common
      CALL UVPGET (JERR)
C                                       Reset number of Stokes wanted.
      IF (ICOR0.EQ.-1) KNCOR = NCOR
C                                       Init "last time"
      TIMLST = -1.0E10
      GO TO 999
C-----------------------------------------------------------------------
C                                       READ/select datum
C                                       Get next selected datum
 300  CALL DATGET (RPARM, VIS, TIMLST, JERR)
C                                       Check for end of file
      IERR = JERR
      IF (JERR.EQ.4) IERR = -1
      GO TO 999
C-----------------------------------------------------------------------
C                                       Close files
C                                       UV data file
 900  MSGSUP = 32000
      CALL ZCLOSE (IULUN, IUFIND, IERR)
      MSGSUP = MSGSAV
C                                       INDEX file
      IF (INXRNO.GT.0) CALL TABIO ('CLOS', 0, NREAD, UBUFF, NXBUFF,
     *   IERR)
C                                       Cal file
      IF (DOCAL) CALL TABIO ('CLOS', 0, NREAD, UBUFF, CLBUFF, IERR)
C                                       FLAG file
      IF (DOFLAG) CALL TABIO ('CLOS', 0, NREAD, UBUFF, FGBUFF, IERR)
C                                       BL file
      IF (DOBL) CALL TABIO ('CLOS', 0, NREAD, UBUFF, BLBUFF, IERR)
C                                       de-allocate memory
      IF ((DOBAND.GT.0) .AND. (PBPBUF.NE.0)) THEN
         CALL ZMEMRY ('FRAL', 'BPASET', 0, BPBUF, PBPBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1900) IERR, 'BANDPASS'
            CALL MSGWRT (6)
            END IF
         PBPBUF = 0
C                                       BP file
         CALL TABIO ('CLOS', 0, NREAD, UBUFF, BPBUFF, IERR)
         END IF
      IF ((DOPOL.GT.0) .AND. (PPOLCL.NE.0)) THEN
         CALL ZMEMRY ('FRAL', 'POLSET', 0, DTERMS, PDTERM, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1900) IERR, 'POLARIZATION'
            CALL MSGWRT (6)
            END IF
         PDTERM = 0
         PPOLCL = 0
         END IF
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
         IF (GOTIT) THEN
            FVOL(I) = FVOL(I+1)
            FCNO(I) = FCNO(I+1)
            FRW(I) = FRW(I+1)
            END IF
 910     CONTINUE
      IF (GOTIT) NCFILE = NCFILE - 1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVGET: UNKNOWN OPCODE: ',A4)
 1010 FORMAT ('UVGET: ERROR ',I3,' FINDING FILE:', A12,'.',A6,
     *   '.',I3,' DISK=',I2,' USER=',I5)
 1011 FORMAT ('UVGET: FOUND FILE:', A12,'.',A6,'.',I4,' TYPE ',A2,
     *   ' NOT UV OR SC')
 1020 FORMAT ('UVGET: ERROR ',I3,' READING DATA CATALOG HEADER')
 1021 FORMAT ('UVGET: WARNING',I3,' WITH DATA CATALOG HEADER STATUS')
 1030 FORMAT ('UVGET: ERROR ',I3,' OPENING UV DATA FILE')
 1035 FORMAT ('UVGET: Using flag table version',I4,' to edit data')
 1045 FORMAT ('UVGET: ERROR ',I3,' INITING UV DATA FILE')
 1050 FORMAT ('UVGET: DATA IN WRONG SORT ORDER = ',A2,' NOT T*')
 1051 FORMAT ('UVGET: SUBARRAY',I5,' NOT ACCEPTABLE WHEN CALIBRATING')
 1052 FORMAT ('UVGET: SUBARRAY',I3,' ERROR',I4,' OPENING AN TABLE')
 1053 FORMAT ('UVGET: SUBARRAY',I3,' ERROR',I4,' READING ARRAY NAME')
 1060 FORMAT ('UVGET: SELBAN & SELFRQ ADVERBS DO NOT MATCH DATA')
 1070 FORMAT ('UVGET: FREQID = ',I3,' FORBIDDEN, USE USEFUL VALUE')
 1900 FORMAT ('UVGET: ERROR',I4,' FREEING ',A,' CAL MEMORY')
      END
