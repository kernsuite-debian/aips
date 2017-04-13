LOCAL INCLUDE 'NEWHD.INC'
      INTEGER   MLOCU, MLOCV, MLOCW, MLOCT, MLOCB, MLOCSU, MLOCFQ,
     *   MLOCIT, MLOCID, MLOCA1, MLOCA2, MLOCSA
      COMMON /OUTHDR/ MLOCU, MLOCV, MLOCW, MLOCT, MLOCB, MLOCSU, MLOCFQ,
     *   MLOCIT, MLOCID, MLOCA1, MLOCA2, MLOCSA
LOCAL END
LOCAL INCLUDE 'SPLAT.INC'
C                                       Local include for SPLAT
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SEQIN, CNOIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ,
     *   LRECC, NRPRMC, LLOCWT, UQUAL, NOUTCH, NCHAVG, NINCH, ORIGCH,
     *   CNTCHN(MAXIF), CHNSEL(3,20,MAXIF), PRMTRN(14), LLOCSU, CHINC,
     *   IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XXSTOK,
     *   XNAMOU(3), XCLAOU(2)
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XCALCO*4, XSTOK*4,
     *   NAMEOU*12, CLAOUT*6, HISCRD(10)*64, UCALC*4
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XANTS(50), XBAND, XFREQ,
     *   XFQID, XBIF, XEIF, XBCHAN, XECHAN, XSUBA, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3),
     *   XSOUT, XDISO, XDOUV, APARM(10), XCHNS(4,20), XCHANL, XCHINC,
     *   XSOLIN, XBADD(10), BUFF1(UVBFSS), BUFF2(UVBFSS), BUFF3(UVBFSS),
     *   SOLINT, REFCOR, AVGCH(MAXIF)
      LOGICAL   OUTCOM, ASSEMB, DOIF, DOAVG, DOSUBS, INCOMP, OUTSOU
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XXCALC, XTIME, XXSTOK, XANTS, XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XBCHAN, XECHAN, XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH, XNAMOU, XCLAOU, XSOUT, XDISO,
     *   XDOUV, APARM, XCHNS, XCHANL, XCHINC, XSOLIN, XBADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XCALCO, XSTOK, NAMEOU,
     *   CLAOUT, HISCRD, UCALC
      COMMON /MISC/ ORIGCH, SEQIN, SEQOUT, DISKIN, DISKO, CNOIN, NUMHIS,
     *   LRECC, NRPRMC, LLOCWT, OUTCOM, UQUAL, DOIF, DOAVG, DOSUBS,
     *   ASSEMB, SOLINT, INCOMP, CNTCHN, AVGCH, CHNSEL, REFCOR, PRMTRN,
     *   OUTSOU, LLOCSU, NOUTCH, NCHAVG, NINCH, CHINC
      INCLUDE 'NEWHD.INC'
LOCAL END
      PROGRAM SPLAT
C-----------------------------------------------------------------------
C! Applies calibration and split or assemble selected sources.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2016
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
C   Splits a multisource uv data set into single-source data files.
C   Or:
C   Assembles selected sources into the new multi-source file.
C   Multiple sources may be processed in a single run.
C   Calibration and editing may be optionally applied.  Fully
C   flagged data will not be copied.
C   Should also handle applying an SN table to a single source file.
C   Inputs:
C      AIPS adverb          Description.
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      SOURCES....Source list.
C      TIMERANG...Time range of the data to be copied.
C      STOKES.....Stokes type to pass.
C      BIF........First IF to copy. 0=>all.
C      EIF........Highest IF to copy. 0=>all higher than BIF
C      BCHAN......First channel to copy. 0=>all.
C      ECHAN......Highest channel to copy. 0=>all higher than BIF
C      SUBARRAY...Subarray number to copy. 0=>all.
C      DOCALIB....If true (>0) then calibrate the data.
C      GAINUSE....Version number of the Cal. table to use.
C      DOPOL......If >0 then do polarization calibration.
C      BLVER......The BL table to apply.
C      FLAGVER....Specifies the version of the flagging table.
C      DOBAND.....If true correct data for bandpass
C      BPVER......The BP table to apply
C      SMOOTH.....Smoothing function.
C      OUTCLASS...Output UV file name (class).    Standard defaults.
C                 The output file name will be the first 12
C                 characters of the name of the source.
C      OUTSEQ.....Output UV file name (seq. #).   0 => highest unique
C      OUTDISK....Disk drive # of output UV file. 0 => highest with
C                 space for the file.
C      OUTCOMP....If > 1, compress output data.
C      APARM......Control information:
C                    1 = 1 => avg. freq. in IF
C                      = 2 => avg IFs also
C                      = 3 => avg each N channels
C                    2 = Integration time (sec)
C                    3 > 0 => drop subarray info
C                    4 > 0 => calibrate weights
C                    5 = 0 => pass only xc data
C                      = 1 => pass xc/ac
C                      = 2 => pass only ac
C                    6 > 0 => add full sourcename to header
C                    7 = 0 => assemble the sources
C                      > 0 => split the sources
C      CHNSEL.....Channel selection
C      CHANNEL....Number of  chans to average together
C      SOLINT.....Time of averaging in min.
C      BADDISK....Disks to avoid for scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   NUMSOU, IRET
      LOGICAL   DOWANT
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'SPLAT.INC'
      INTEGER   SOCOUN(XSTBSZ), SULIST(XSTBSZ)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'SPLAT '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL SPLTIN (PRGM, DOWANT, NUMSOU, SULIST, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop over sources.
      CALL SPLTUV (DOWANT, NUMSOU, SULIST, SOCOUN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       copy tables
      IF (ASSEMB) CALL COPTAB (SOCOUN, IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SPLTIN (PRGN, DOWANT, NUMSOU, SULIST, IRET)
C-----------------------------------------------------------------------
C   SPLTIN gets input parameters for SPLAT, finds input file and
C   prepares the list of sources.  All selection criteria
C   except the source name is filled into the commons in D/CSEL.INC.
C   Inputs:  PRGN    C*6   Program name
C   Output:
C     DOWANT       L    If true sources listed are selected
C     NUMSOU       I    Number of sources to process, 0=>all
C     SULIST(*)    I    Source number list.
C     IRET         I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in SPLAT for more details.
C-----------------------------------------------------------------------
      HOLLERITH CATH(256)
      CHARACTER STAT*4, PRGN*6, UTYPE*2, TELTYP*8
      INTEGER   NUMSOU, SULIST(*), J, IRET
      LOGICAL   DOWANT, HADSRC
      INTEGER   NPARM, IROUND, IERR, I, LUN
      REAL      CATR(256)
      LOGICAL   MATCH
      INCLUDE 'SPLAT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATBLK, CATH)
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 318
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (12, 1, XNAMOU, NAMEOU)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
      DO 25 I = 1,50
         ANTENS(I) = IROUND (XANTS(I))
 25      CONTINUE
C                                       Get CATBLK from old file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Find telescope name
      CALL H2CHR (8, 1, CATH(KHTEL), TELTYP)
C                                       Save input header
      CALL RCOPY (256, CATBLK, CATUV)
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Input data compressed?
      INCOMP = CATBLK(KINAX).EQ.1
C                                       Output data compressed>
      OUTCOM = XDOUV.GT.0.0
C                                       If input data are compressed,
C                                       the output should be compressed
C                                       also
      IF (INCOMP) OUTCOM = .TRUE.
C                                       assemle or split?
      ASSEMB = APARM(7).EQ.0.0
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (BIF.GT.EIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      DOAVG = (APARM(1).GT.0.5) .AND. (APARM(1).LT.1.5)
      DOIF  = (APARM(1).GT.1.5) .AND. (APARM(1).LT.2.5)
      DOSUBS = (APARM(1).GT.2.5) .AND. (APARM(1).LT.3.5)
C                                       Save the original number of
C                                       channels for later use.
      ORIGCH = CATBLK(KINAX+JLOCF)
      NINCH = ECHAN - BCHAN + 1
C                                       calculate number of channels in
C                                       the output file
      NOUTCH = NINCH
      IF (DOAVG .OR. DOIF) NOUTCH = 1
C
      IF (DOSUBS) THEN
         NCHAVG = IROUND (XCHANL)
         NCHAVG = MAX (1, MIN (NCHAVG, NINCH))
         CHINC = IROUND (XCHINC)
         CHINC = MIN (CHINC, NINCH)
         IF (CHINC.LE.0) CHINC = NCHAVG
         XCHANL = NCHAVG
         XCHINC = CHINC
         NOUTCH = (NINCH + CHINC - NCHAVG) / CHINC
         I = (NOUTCH-1) * CHINC + NCHAVG
         IF (I.LT.NINCH) THEN
            J = NINCH - I
            WRITE (MSGTXT,1080) J
            CALL MSGWRT (6)
            WRITE (MSGTXT,1100)
            CALL MSGWRT (6)
            ECHAN = BCHAN + I - 1
            END IF
         WRITE (MSGTXT,1120) NOUTCH
         CALL MSGWRT (6)
         WRITE (MSGTXT,1130) NCHAVG
         CALL MSGWRT (5)
         WRITE (MSGTXT,1140) CHINC
         CALL MSGWRT (5)
      ELSE
         IF (DOAVG) THEN
            MSGTXT = 'Averaging all selected channels in frequency'
            CALL MSGWRT (5)
            END IF
         IF (DOIF) THEN
            MSGTXT = 'Averaging all selected channels in frequency'
            CALL MSGWRT (5)
            MSGTXT = 'Also averaging all selected IFs'
            CALL MSGWRT (5)
            END IF
         END IF
C                                       BADDISK
      DO 60 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 60      CONTINUE
C                                       Time of averaging in days
      SOLINT = XSOLIN / (24.0 * 60.0)
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      CALL CCOPY (30, XSOUR, SOURCS)
      CALL RCOPY (8, XTIME, TIMRNG)
      STOKES = XSTOK
C
      DOCAL = XDOCAL.GT.0.0
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      DOWTCL = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      DOACOR = .FALSE.
      DOXCOR = .TRUE.
      IF (APARM(5).EQ.1) THEN
         DOACOR = .TRUE.
         DOXCOR = .TRUE.
      ELSE IF (APARM(5).EQ.2) THEN
         DOACOR = .TRUE.
         DOXCOR = .FALSE.
         END IF
C                                        Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C
      DXTIME = APARM(2) / 86400.0
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
      SELQUA = IROUND (XQUAL)
      UQUAL = SELQUA
      SELQUA = MAX (-1, SELQUA)
      SELCOD = XCALCO
      UCALC = SELCOD
C                                       Kludge for SPLAT logic error
C                                       with multi-freqid data
      IF (((TELTYP.EQ.'ATCA') .OR. (TELTYP.EQ.'ATLBA')) .AND.
     *   (FRQSEL.GT.0) .AND. (SELQUA.LE.0)) SELQUA = FRQSEL
C                                       Get source list
      IUDISK = FVOL(1)
      IUCNO = FCNO(1)
      IXLUN = 28
      CALL SOUFIL (IRET)
      IF (IRET.NE.0) GO TO 999
      DOWANT = DOSWNT
      NUMSOU = NSOUWD
C
      CALL COPY (XSTBSZ, SOUWAN, SULIST)
C                                       Reset values in /SELCAL/
      CALL CFILL (30, ' ', SOURCS)
      NSOUWD = 0
C                                       Record whether there was
C                                       a positive source selection
C                                       before matching against FQSEL.
      HADSRC = (DOWANT) .AND. (NUMSOU.GT.0)
C                                       Remove sources not = FRQSEL
      CALL SOUNDX (FVOL(1), FCNO(1), DOWANT, NUMSOU, SULIST, IRET)
      DOSWNT = DOWANT
C                                       Check that a positive source
C                                       selection hasn't been
C                                       completely blown away.
      IF ((HADSRC) .AND. (NUMSOU.EQ.0)) THEN
         MSGTXT = 'REQUESTED SOURCES NOT FOUND: CHECK QUAL FREQID'
         IRET = 1
         GO TO 990
         END IF
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPLTIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1040 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1060 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1080 FORMAT ('Will reject ',I3,' channels from the end of')
 1100 FORMAT ('the input spectrum furthest from the LO')
 1120 FORMAT ('Output file will have ',I4,' channels/IF')
 1130 FORMAT ('Each',I3, ' channels will be averaged')
 1140 FORMAT ('Every',I3, ' channels will be output')
      END
      SUBROUTINE SPLTUV (DOWANT, NUMSOU, SULIST, SOCOUN, IRET)
C-----------------------------------------------------------------------
C   SPLTUV uses UVGET and SPLCOP to copy data into single source files,
C   or to assemble the selected sources into multiple source file
C   The history and relevant tables are also copied.
C   Input:
C     DOWANT       L    If true sources listed are selected
C     NUMSOU       I    Number of sources to process, 0=>all
C     SULIST(*)    I    Source number list.
C   Output:
C     SOCOUN(*)    I    Number of the given source occurances in the
C                       data
C     IRET         I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER NAME*12, VELTYP*8, VELDEF*8, CHVEL(4)*4
      CHARACTER TEST*16, CHTM2*2
      HOLLERITH CATH(256), CATUH(256), SNAME1(2),SNAME2(2)
      LOGICAL   DOWANT, F, DOAPPT, FINISH, NOSUB, SMILE, SINGLE, TABLE,
     *   EXIST, FITASC, ONZE, LOCAL, LOBL, UVOPEN
      INTEGER   NUMVIS, SOUCUR, MAXSOU, SLOOP, TOTREC(2,3), NUMSOU,
     *   SULIST(*), IRET, INDEX, LRECU, NUMFRQ, IERR, K, K1, K2, IOFF,
     *   SUKOLS(MAXSUC), SUNUMV(MAXSUC), SBUFF(768), I, SLUN, IDSOU,
     *   INOGRP, TMPVER, SUB, LIMS1, LIMS2, SUBTMP, CATSAV(256), OUTDSK,
     *   OUTCNO, ITEMP, SAVBND, IROUND, NCRPM, J, SUMCHN, SUFQID,
     *   DPOSAV, NBASL, NC, NPRM, DUM, ERRCNT, INHIS
      REAL      RPARM, VIS, CATR(256), OLDRP, CHWT
      DOUBLE PRECISION  BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, CATD(128), OLDFRQ, RAOBS, DECOBS
      INCLUDE 'SPLAT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   SOCOUN(*), NW(MAXIF), ISBAND(MAXIF), IIVER, CSUM, NNIF,
     *   LUN, TBIF, TEIF, RNXRET, NSLIST, NSNAME(XSTBSZ), ONLIST, JTRIM
      DOUBLE PRECISION   LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF),
     *   FOFF(MAXIF), FSUM, BWSUM
      CHARACTER SNMS(XSTBSZ)*16, TMPNAM*16, BNDCOD(MAXIF)*8
      REAL      FLUX(4,MAXIF), FINC(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD),   (CATUV, CATUH)
      SAVE ONZE
      DATA CHVEL /'LSR ','BARY','HELI','RADI'/
      DATA F /.FALSE./, ONZE/.FALSE./
      DATA LUN /18/
C-----------------------------------------------------------------------
      ERRCNT = 0
      SMILE = F
      TMPVER = CLVER
      DOAPPT = DOAPPL
      NOSUB = APARM(3).GT.0.0
      OLDRP = CATR(KRCRP+JLOCF)
      TBIF = BIF
      TEIF = EIF
      IF (DOSUBS) THEN
         CALL FILL (MAXIF, NCHAVG, CNTCHN)
         CHWT = BCHAN + (NCHAVG-1.0) / 2.0
         CALL RFILL (MAXIF, CHWT, AVGCH)
      ELSE
         I = 60 * MAXIF
         CALL FILL (I, 0, CHNSEL)
         CALL FILL (MAXIF, 0, NW)
         DO 20 J = 1,20
            K = IROUND (XCHNS(2,J))
            IF (K.LE.0) GO TO 25
            K = IROUND (XCHNS(4,J))
            IF ((K.LE.0) .OR. (K.GT.MAXIF)) THEN
               K1 = 1
               K2 = MAXIF
            ELSE
               K1 = K
               K2 = K
               END IF
            DO 15 K = K1,K2
               NW(K) = NW(K) + 1
               DO 10 I = 1,3
                  CHNSEL(I,NW(K),K) = IROUND (XCHNS(I,J))
                  IF (CHNSEL(I,NW(K),K).LT.0) CHNSEL(I,NW(K),K) = 0
 10               CONTINUE
               IF (CHNSEL(3,NW(K),K).EQ.0) CHNSEL(3,NW(K),K) = 1
 15            CONTINUE
 20         CONTINUE
C                                       If no channel selection
C                                       use BCHAN - ECHAN
 25      DO 50 K = 1,MAXIF
            IF (NW(K).LE.0) THEN
               NW(K) = 1
               CHNSEL(1,1,K) = BCHAN
               CHNSEL(2,1,K) = ECHAN
               CHNSEL(3,1,K) = 1
               END IF
            DO 30 I = 1,NW(K)
               CHNSEL(1,I,K) = MAX (BCHAN, MIN (CHNSEL(1,I,K), ECHAN))
               IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K))
     *            CHNSEL(2,I,K) = ECHAN
               CHNSEL(2,I,K) = MAX (BCHAN, MIN (CHNSEL(2,I,K), ECHAN))
 30            CONTINUE
C                                       Find average channel number
            SUMCHN = 0
            NC = 0
            DO 40 I = BCHAN,ECHAN
               CHWT = 1.0
               CALL WANTCH (CHNSEL(1,1,K), I, CHWT)
               IF (CHWT.GT.0.0) THEN
                  SUMCHN = SUMCHN + I
                  NC = NC + 1
                  END IF
 40            CONTINUE
            IF (NC.GT.0) THEN
               AVGCH(K) = SUMCHN
               AVGCH(K) = AVGCH(K) / NC
            ELSE
               AVGCH(K) = 1.0
               END IF
            CNTCHN(K) = NC
 50         CONTINUE
         END IF
C                                       Get input reference freq.
      OLDFRQ = CATD(KDCRV+JLOCF)
C                                       Find number of subarrays
      IF (SUBARR.LE.0) THEN
         CALL FNDEXT ('AN', CATUV, LIMS2)
         LIMS1 = 1
         LIMS2 = MAX (1, LIMS2)
      ELSE
         LIMS1 = SUBARR
         LIMS2 = SUBARR
         END IF
      SUBTMP = SUBARR
      DPOSAV = DOPOL
      SAVBND = DOBAND
      LOCAL = DOCAL
      LOBL = DOBL
      UVOPEN = .FALSE.
C                                       Add record about CQ table to
C                                       history file to document
C                                       delay decorrelation corrections
      SLUN = 27
      CALL ISTAB ('CQ', FVOL(1), FCNO(1), 1, SLUN, SBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      IF (EXIST.AND.TABLE) THEN
         NUMHIS = NUMHIS + 1
         HISCRD(NUMHIS) = 'CQ table present; (delay decorrelation)'
         END IF
C                                       Check if single source file
      SLUN = 27
      CALL MULSDB (CATUV, SINGLE)
      IF (SINGLE) THEN
         CALL ISTAB ('SU', FVOL(1), FCNO(1), 1, SLUN, SBUFF, TABLE,
     *      EXIST, FITASC, IERR)
         SINGLE = EXIST .AND. TABLE .AND. (IERR.EQ.0)
         END IF
      SINGLE = .NOT.SINGLE
C                                       not allow to assemble single
C                                       source file
      IF (SINGLE .AND. ASSEMB) THEN
         MSGTXT = 'Nothing to assemble because the data are ' //
     *      'single source'
         CALL MSGWRT (6)
         ASSEMB = .FALSE.
         END IF
C                                       Open source table
      IF ((.NOT.SINGLE) .AND. (.NOT.ASSEMB)) THEN
         CALL SOUINI ('READ', SBUFF, FVOL(1), FCNO(1), 1, CATUV, SLUN,
     *      INOGRP, VELTYP, VELDEF, SUFQID, SOUCUR, SUKOLS, SUNUMV,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         MAXSOU = SBUFF(5)
         DO 90 SLOOP = 1,MAXSOU
            SOUCUR = SLOOP
            CALL TABSOU ('READ', SBUFF, SOUCUR, SUKOLS, SUNUMV, IDSOU,
     *         SOURCS, SELQUA, SELCOD, FLUX, FREQO, BANDW, RAEPO,
     *         DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL,
     *         RESTFQ, PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       See if this source wanted.
            IF (NUMSOU.GT.0) THEN
               DO 55 I = 1,NUMSOU
                  IF (IDSOU.EQ.SULIST(I)) THEN
                     IF (DOWANT) GO TO 60
                     GO TO 90
                     END IF
 55               CONTINUE
               IF (DOWANT) GO TO 90
               END IF
 60         IF ((UQUAL.GE.0) .AND. (UQUAL.NE.SELQUA)) GO TO 90
            IF (UCALC.NE.' ') THEN
               IF (UCALC.EQ.'*') THEN
                  IF (SELCOD.EQ.' ') GO TO 90
               ELSE IF (UCALC.EQ.'-CAL') THEN
                  IF (SELCOD.NE.' ') GO TO 90
               ELSE
                  IF (SELCOD.NE.UCALC) GO TO 90
                  END IF
               END IF
            DO 70 I = 1,NSLIST
               IF (SOURCS(1).EQ.SNMS(I)) THEN
                  NSNAME(I) = NSNAME(I)+1
                  GO TO 90
                  END IF
 70            CONTINUE
            NSLIST = NSLIST + 1
            NSNAME(NSLIST) = 1
            SNMS(NSLIST) = SOURCS(1)
 90         CONTINUE
C                                       Setup for single source case:
      ELSE
         IRET = 0
         MAXSOU = 1
         NUMSOU = 0
         CALL CFILL (30, ' ', SOURCS)
         END IF
C                                       restore SOURCS for the source
C                                       selection in UVGET
      IF (ASSEMB) THEN
         CALL CCOPY (30, XSOUR, SOURCS)
         MAXSOU = 1
         END IF
C
C                                       Loop here over sources
      INHIS = NUMHIS
      DO 700 SLOOP = 1,MAXSOU
         IF (.NOT.ASSEMB) THEN
            SOUCUR = SLOOP
            NUMHIS = INHIS
C                                       Read source table
            IF (.NOT.SINGLE) THEN
               CALL TABSOU ('READ', SBUFF, SOUCUR, SUKOLS, SUNUMV,
     *            IDSOU, SOURCS, SELQUA, SELCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, RESTFQ, PMRA, PMDEC, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       See if this source wanted.
               IF (NUMSOU.GT.0) THEN
                  DO 120 I = 1,NUMSOU
                     IF (IDSOU.EQ.SULIST(I)) THEN
                        IF (DOWANT) GO TO 130
                        GO TO 700
                        END IF
 120                 CONTINUE
                  IF (DOWANT) GO TO 700
                  END IF
 130           IF ((UQUAL.GE.0) .AND. (UQUAL.NE.SELQUA)) GO TO 700
               IF (UCALC.NE.' ') THEN
                  IF (UCALC.EQ.'*') THEN
                     IF (SELCOD.EQ.' ') GO TO 700
                  ELSE IF (UCALC.EQ.'-CAL') THEN
                     IF (SELCOD.NE.' ') GO TO 700
                  ELSE
                     IF (SELCOD.NE.UCALC) GO TO 700
                     END IF
                  END IF
               IF ((SELCOD.EQ.' ') .AND. (.NOT.ASSEMB)) SELCOD = '-CAL'
               END IF
            END IF
         ONLIST = 1
         IF (.NOT.ASSEMB) THEN
            DO 135 I = 1,NSLIST
               IF (SOURCS(1).EQ.SNMS(I)) ONLIST = NSNAME(I)
 135           CONTINUE
            END IF
C
         SUBARR = SUBTMP
         DOPOL = -1
         DOBAND = 0
         DOCAL = .FALSE.
         DOBL = .FALSE.
         CALL UVGET ('INIT', RPARM, VIS, IERR)
         UVOPEN = .TRUE.
C                                       If a UV GET ERROR
         IF (IERR.NE.0) THEN
            IF (IERR.EQ.2) THEN
               MSGTXT = 'UVGET ERROR, CHECK THAT REQUESTED GAIN '//
     *            'TABLE EXISTS'
               CALL MSGWRT(5)
               END IF
C                                       Jump to try next source
            GO TO 650
C                                       End if UVGET Error
            END IF
C                                       Close again - only need header
C                                       for sum of subarrays.
         CALL UVGET ('CLOS', RPARM, VIS, IERR)
         IF (IERR.NE.0) GO TO 650
         DOPOL = DPOSAV
         DOBAND = SAVBND
         DOCAL = LOCAL
         DOBL = LOBL
         UVOPEN = F
         DOAPPL = F
         CLVER = CLUSE
C                                       Put new values in CATBLK.
         CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAME, CLAOUT,
     *      SEQOUT)
         CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
C                                       name of the output aips data
         IF ((ASSEMB) .OR. (SINGLE)) THEN
C                                       store inname in outname
            IF (NAMEOU.EQ.' ') THEN
               CALL CHR2H (12, NAME, KHIMNO, CATH(KHIMN))
C                                       store the given name in catalog
            ELSE
               CALL CHR2H (12, NAMEOU, KHIMNO, CATH(KHIMN))
               END IF
C                                       sources are splitted
         ELSE
C                                       Name after source.
            IF (NAMEOU.EQ.' ') THEN
               TMPNAM = SOURCS(1)
               IF (ONLIST.GT.1) THEN
                  I = JTRIM (SELCOD)
                  WRITE (TMPNAM(9:),1130) SELQUA
                  IF (I.GT.0) TMPNAM(9:8+I) = SELCOD(:I)
                  END IF
               CALL CHR2H (12, TMPNAM, KHIMNO, CATH(KHIMN))
C                                       use OUTNAME
            ELSE
               CALL CHR2H (12, NAMEOU, KHIMNO, CATH(KHIMN))
               IF (SLOOP.GT.1) SEQOUT = 0
               END IF
            END IF
         CATBLK(KIIMS) = SEQOUT
C                                       Velocity etc. info IF=BIF
C                                       Do it only for multiple source
C                                       file; otherwise keep the
C                                       original values in header
         IF (.NOT.SINGLE) THEN
            CATD(KDRST) = RESTFQ(BIF)
            CATD(KDARV) = LSRVEL(BIF)
C                                       CRP already corr for bchan
            CATR(KRARP) = CATR(KRCRP+JLOCF)
C                                       Velocity reference frame
            ITEMP = 3
            IF (VELTYP(1:4).EQ.CHVEL(1)) ITEMP = 1
            IF (VELTYP(1:4).EQ.CHVEL(2)) ITEMP = 2
            IF (VELTYP(1:4).EQ.CHVEL(3)) ITEMP = 2
            IF (VELDEF(1:4).EQ.CHVEL(4)) ITEMP = ITEMP + 256
            CATBLK(KIALT) = ITEMP
C                                       UVGET corrects for BCHAN
         ELSE
            CATR(KRARP) = CATR(KRARP)
            END IF
C                                       Update CATBLK if Averaging with
C                                       use output freq pixel as ref.
C
C                                       If averaging set CATBLK
         REFCOR = 0
         IF (DOIF) THEN
            IIVER = 1
            CALL CHNDAT ('READ', BUFF1, IUDISK, IUCNO, IIVER, CATUV,
     *         LUN, NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
            IF (IRET.NE.0) GO TO 999
            FSUM = 0.0D0
            BWSUM = 0.0D0
            CSUM = 0
            DO 160 I = TBIF,TEIF
               FSUM = FSUM + (FOFF(I) - FOFF(TBIF) +
     *            (AVGCH(I) - OLDRP) * FINC(I)) * CNTCHN(I)
               BWSUM = BWSUM + CNTCHN(I) * FINC(I)
               CSUM = CSUM + CNTCHN(I)
 160           CONTINUE
            FSUM = FSUM / CSUM
            CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + FSUM
            CATR(KRCRP+JLOCF) = 1.0
            CATR(KRCIC+JLOCF) = BWSUM
            CATBLK(KINAX+JLOCF) = 1
            CATBLK(KINAX+JLOCIF) = 1
C                                       No velocity info if averaging.
            CATD(KDRST) = 0.0
            CATD(KDARV) = 0.0
            CATR(KRARP) = 0.0
C                                       averaging spectral channels
         ELSE IF (DOAVG .OR. DOSUBS) THEN
C                                       correction of freq
            REFCOR = AVGCH(1) - OLDRP
            CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + REFCOR
     *         * CATR(KRCIC+JLOCF)
            CATR(KRCRP+JLOCF) = 1.0
C                                       No velocity info if averaging.
            CATD(KDRST) = 0.0
            CATD(KDARV) = 0.0
            CATR(KRARP) = 0.0
            IF (DOSUBS) THEN
               CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF) * CHINC
            ELSE
               CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF) * CNTCHN(1)
               END IF
            CATBLK(KINAX+JLOCF) = NOUTCH
            END IF
C                                       Make sure there is data
         IF (CATBLK(KIGCN).LE.0) GO TO 650
C                                       remove REMOVEDs
         CALL PRMSET (CATBLK, PRMTRN)
         NPRM = CATBLK(KIPCN)
C                                       force source if ASSEMB
         CALL AXEFND (8, 'SOURCE  ', NPRM, CATH(KHPTP), IOFF, I)
         OUTSOU = (ASSEMB) .AND. ((IOFF.LT.0) .OR. (I.GT.0))
         IF (OUTSOU) THEN
            LLOCSU = NPRM
            INDEX = KHPTP + 2 * LLOCSU
            CALL CHR2H (8, 'SOURCE  ', 1, CATH(INDEX))
            CATBLK(KIPCN) = CATBLK(KIPCN) + 1
            END IF
         IF (OUTCOM) THEN
            LLOCWT = NPRM
            IF (OUTSOU) LLOCWT = LLOCWT + 1
            INDEX = KHPTP + 2 * LLOCWT
            CALL CHR2H (8, 'WEIGHT  ', 1, CATH(INDEX))
            CALL CHR2H (8, 'SCALE   ', 1, CATH(INDEX+2))
            CATBLK(KINAX) = 1
            CATBLK(KIPCN) = CATBLK(KIPCN) + 2
            END IF
         CCNO = 1
         CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            IF ((.NOT.SINGLE) .AND. (SELQUA.EQ.-1)) THEN
               IF (IERR.NE.2) THEN
                  WRITE (MSGTXT,1040) IERR
                  CALL MSGWRT (6)
                  END IF
               GO TO 660
            ELSE
               IF (IERR.EQ.2) THEN
                  MSGTXT = 'CANNOT OVERWRITE OLD FILE'
                  CALL MSGWRT (6)
                  END IF
               GO TO 650
               END IF
            END IF
C                                       actual output file
         CALL UVPGET (IERR)
         MLOCU = ILOCU
         MLOCV = ILOCV
         MLOCW = ILOCW
         MLOCT = ILOCT
         MLOCB = ILOCB
         MLOCSU = ILOCSU
         MLOCFQ = ILOCFQ
         MLOCIT = ILOCIT
         MLOCID = ILOCID
         MLOCA1 = ILOCA1
         MLOCA2 = ILOCA2
         MLOCSA = ILOCSA
         LRECU = LREC
         NCRPM = NRPARM
         IF (.NOT.OUTSOU) LLOCSU = ILOCSU
C                                       Report gain table version, once
         IF ((DOCAL) .AND. (.NOT.ONZE)) THEN
            IF (SINGLE) THEN
               IF (CLVER.LE.0) CALL FNDEXT ('SN', CATUV, CLVER)
               WRITE (MSGTXT,1060) 'SN', CLVER
            ELSE
               IF (CLVER.LE.0) CALL FNDEXT ('CL', CATUV, CLVER)
               WRITE (MSGTXT,1060) 'CL', CLVER
               END IF
            CALL MSGWRT(3)
            ONZE = .TRUE.
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = CCNO
         FRW(NCFILE) = 2
         OUTDSK = DISKO
         OUTCNO = CCNO
C                                       copy keywords
         CALL KEYCOP (DISKIN, CNOIN, DISKO, CCNO, IERR)
C                                       make an index table
         CALL RNXGET (DISKIN, CNOIN, CATUV)
         CALL RNXINI (DISKO, CCNO, CATBLK, RNXRET)
C                                       Loop over subarrays.
         NUMVIS = 0
         NUMFRQ = ECHAN - BCHAN + 1
         DO 640 SUB = LIMS1,LIMS2
C                                       Read AN table to find the
C                                       number of antennas to evaluate
C                                       the number of baseline
            CALL GETANT (DISKIN, CNOIN, SUB, CATUV, BUFF1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1080) IERR, SUB
               CALL MSGWRT (8)
               GO TO 999
               END IF
C                                       cross correlation only
            NBASL = NSTNS * (NSTNS - 1) / 2
C                                       cros + auto
            IF (APARM(5).EQ.1) THEN
               NBASL = NSTNS * (NSTNS + 1) / 2
C                                       auto only
            ELSE IF (APARM(5).EQ.2) THEN
               NBASL = NSTNS
               END IF
C
            SUBARR = SUB
C                                       Save CATBLK - UVGET will modify
            CALL COPY (256, CATBLK, CATSAV)
C                                       Initialize reading data
            CALL UVGET ('INIT', RPARM, VIS, IERR)
            IF (IERR.GT.0) GO TO 645
C                                       handle missing subarray
            IF ((IERR.LT.0) .AND. (SUB.LT.LIMS2)) THEN
               MSGSUP = 32000
               CALL UVGET ('CLOS', RPARM, VIS, IERR)
               MSGSUP = 0
               CALL COPY (256, CATSAV, CATBLK)
               GO TO 640
               END IF
            UVOPEN = .TRUE.
C                                       Update sort order if necessary
            IF (NUMVIS.GT.0) CALL CHR2H (2, '**', 1, CATSAV(KITYP))
C                                       Restore CATBLK
            CALL COPY (256, CATSAV, CATBLK)
C                                       Copy data
            FINISH = SUB.EQ.LIMS2
            CALL SPLCOP (NUMVIS, TOTREC, FINISH, DOAVG, DOIF, DOSUBS,
     *         DISKO, CCNO, NCHAVG, CHINC, NOUTCH, SOLINT, NOSUB, BUFF1,
     *         JBUFSZ, FREQO, OLDFRQ, NBASL, NUMHIS, HISCRD,
     *         CHNSEL, OUTCOM, LLOCWT, OUTSOU, LLOCSU, BUFF2, BUFF3,
     *         LRECU, NCRPM,NPRM, PRMTRN, AVGCH, CNTCHN, OLDRP, SOCOUN,
     *         RNXRET, IERR)
            IF (IERR.NE.0) GO TO 645
            SMILE = .TRUE.
            UVOPEN = F
            NUMVIS = NUMVIS + NVIS
 640        CONTINUE
C                                       Add full sourcename
         IF (APARM(6).GT.0.0) THEN
            CALL CHR2H (8, SOURCS(1)(1:8),  1, SNAME1)
            CALL CHR2H (8, SOURCS(1)(9:16), 1, SNAME2)
            CALL H2CHR (8, 1, SNAME1, TEST(1:8))
            CALL H2CHR (8, 1, SNAME2, TEST(9:16))
            CALL CATKEY ('WRIT', OUTDSK, OUTCNO, 'SOURNAM1', 1, 1,
     *         SNAME1, 3, BUFF1, IERR)
            CALL CATKEY ('WRIT', OUTDSK, OUTCNO, 'SOURNAM2', 1, 1,
     *         SNAME2, 3, BUFF1, IERR)
            END IF
C                                       History
         CLVER = TMPVER
         DOAPPL = DOAPPT
         SUBARR = SUBTMP
C                                       close NX table
         CALL RNXCLS (RNXRET)
         IF (RNXRET.NE.0) THEN
            MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
            CALL MSGWRT (6)
         ELSE
            MSGTXT = 'New NX table written'
            CALL MSGWRT (2)
            END IF
C
         EXIST = SINGLE .OR. ASSEMB
         CALL SPLTHI (OUTDSK, OUTCNO, EXIST, IDSOU)
         CLVER = CLUSE
         DOAPPL = F
         NCFILE = NCFILE - 2
         GO TO 700
C                                       error after UVCREA
 645     DUM = 0
         CHTM2 = 'UV'
         CALL RNXCLS (RNXRET)
         IF (UVOPEN) THEN
            MSGSUP = 32000
            CALL UVGET ('CLOS', RPARM, VIS, I)
            MSGSUP = 0
            END IF
         UVOPEN = F
         CALL CATDIR ('CSTA', OUTDSK, OUTCNO, NAME, CLAOUT, DUM, CHTM2,
     *      DUM, 'CLWR', BUFF1, I)
         CALL MDESTR (OUTDSK, OUTCNO, CATBLK, BUFF1, J, I)
         FVOL(NCFILE) = 0
         FCNO(NCFILE) = 0
         FRW(NCFILE) = 0
         NCFILE = NCFILE - 1
C                                       Error, close input file
C                                       then resume.
C                                       Tell which source
 650     IF ((CATBLK(KIGCN).LE.0) .OR. (IERR.EQ.-1)) THEN
            MSGTXT = '** NO VISIBILITIES FOUND FOR ''' // SOURCS(1) //
     *         ''''
         ELSE
            MSGTXT = '** PROBLEM WITH SOURCE ''' // SOURCS(1) //
     *         ''''
            END IF
         CALL MSGWRT (6)
         WRITE (MSGTXT,1650) SELCOD, SELQUA
         CALL MSGWRT (6)
         ERRCNT = ERRCNT + 1
 660     IF (UVOPEN) THEN
            MSGSUP = 32000
            CALL UVGET ('CLOS', RPARM, VIS, IERR)
            MSGSUP = 0
            END IF
         UVOPEN = F
C                                       restore cal flags!
         DOPOL = DPOSAV
         DOBAND = SAVBND
         DOCAL = LOCAL
         DOBL = LOBL
 700     CONTINUE
C                                       Make sure bad files
C                                       destroyed
      IRET = 1
      IF (SMILE) IRET = 0
      IF (IRET.NE.0) THEN
         MSGTXT = 'FAILED TO SELECT VALID DATA'
         CALL MSGWRT (9)
         END IF
C                                       Close source table
         IF ((.NOT.SINGLE) .AND. (.NOT.ASSEMB))
     *      CALL TABIO ('CLOS', 1, SOUCUR, SBUFF, SBUFF, IERR)
      NCFILE = NCFILE - 1
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('ERROR',I5,' CREATING OUTPUT UV FILE')
 1060 FORMAT ('Applying ',A,' Table version ',I5)
 1080 FORMAT ('ERROR',I3,' READING  AN TABLE; SUB =', I3)
 1130 FORMAT (I4.4)
 1650 FORMAT ('**     CALCODE ''',A,'''  QUALIFIER',I5)
      END
      SUBROUTINE SPLTHI (DISK, CNO, SINGLE, IDSOU)
C-----------------------------------------------------------------------
C   SPLTHI copies and updates history file.  It also copies any tables
C   extension files.
C   Input:
C      DISK     I   Output file disk number
C      CNO      I   Output file catalog slot number.
C      SINGLE   L   F => write an output FO table
C      IDSOU    I   Restrict FO to this source number
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, IDSOU
      LOGICAL   SINGLE
C
      CHARACTER NOTTYP(25)*2, NAMOUT*12, HILINE*72, LABEL*8, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   IERR, I, LUN1, LUN2, NONOT, J, SUBA, K, NSUB, I1, I2
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128), FQOFF
      LOGICAL   T, TABLE, EXIST, FITASC
      INCLUDE 'SPLAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA LUN1, LUN2 /28,29/
      DATA T /.TRUE./
C                                       COPTAB tables
      DATA NONOT, NOTTYP /25, 'BP', 'CL', 'CD', 'CP', 'CQ', 'FG', 'GC',
     *   'IM', 'MC', 'PC', 'PD', 'SN', 'SY', 'SU', 'TY', 'WX', 'BL',
C                                       done elsewhere in SPLAT
     *   'NX', 'AN', 'FQ',
C                                       bad idea to copy/obsolete
     *   'CC', 'CH', 'AT', 'VT', 'HF'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISK, CNOIN, CNO, CATBLK, BUFF1,
     *   BUFF2, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMOUT)
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, CATBLK(KIIMS), DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TIMERANG
      CALL HITIME (TSTART, TEND, LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Stokes'
      WRITE (HILINE,1020) TSKNAM, XSTOK
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IF range
      WRITE (HILINE,1040) TSKNAM, BIF, EIF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Chan range
      WRITE (HILINE,1060) TSKNAM, BCHAN, ECHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Subarray
      WRITE (HILINE,1080) TSKNAM, SUBARR
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Flagging
      IF (FGVER.GT.0) THEN
         WRITE (HILINE,1100) TSKNAM, FGVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                        Spectral smoothing
      IF (SMOOTH(1).GT.0.5) THEN
         I1 = SMOOTH(1) + 0.5
         I2 = SMOOTH(3) + 0.5
         WRITE (HILINE,1120) TSKNAM, I1, SMOOTH(2), I2
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Compressed data
      IF (OUTCOM) THEN
         WRITE (HILINE,1160) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Calibration
C                                       Table
      IF (DOCAL) THEN
         WRITE (HILINE,1180) TSKNAM, CLUSE
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       split or assemble
      IF (APARM(7).EQ.0.0) THEN
         WRITE (HILINE,1200) TSKNAM, APARM(7)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (APARM(7).GT.0.0) THEN
         WRITE (HILINE,1220) TSKNAM, APARM(7)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Averaging
      IF (APARM(1).EQ.1.0) THEN
         WRITE (HILINE,1240) TSKNAM, APARM(1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (APARM(1).EQ.2.0) THEN
         WRITE (HILINE,1260) TSKNAM, APARM(1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (APARM(1).EQ.3.0) THEN
         WRITE (HILINE,1280) TSKNAM, APARM(1), NCHAVG, CHINC
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Channel selection
      DO 65 K = BIF,EIF
         DO 60 I = 1,10
            IF ((CHNSEL(1,I,K).GT.0) .AND.
     *         (CHNSEL(2,I,K).GT.CHNSEL(1,I,K))) THEN
               WRITE (HILINE,1140) TSKNAM, (CHNSEL(J,I,K), J = 1,3), K
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
               END IF
 60         CONTINUE
 65      CONTINUE
C                                       average time
      IF (SOLINT.GT.0.0) THEN
         WRITE (HILINE,1300) TSKNAM, XSOLIN
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Integration time
      IF (APARM(2).GT.0.0) THEN
         WRITE (HILINE,1320) TSKNAM, APARM(2)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Polzn correction
      IF (DOPOL.GT.0) THEN
         WRITE (HILINE,1340) TSKNAM, DOPOL
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       BL table
      IF (XBLVER.GE.0.0) THEN
         WRITE (HILINE,1360) TSKNAM, BLVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       BP table
      IF (XDOBND.GT.0.0) THEN
         WRITE (HILINE,1380) TSKNAM, DOBAND
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         WRITE (HILINE,1400) TSKNAM, BPVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Calibrate weights?
      IF (DOWTCL) THEN
         WRITE (HILINE,1420) TSKNAM
      ELSE
         WRITE (HILINE,1440) TSKNAM
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1460) TSKNAM
         DO 150 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 150        CONTINUE
         END IF
      NUMHIS = 0
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISK, CNOIN, CNO,
     *   CATBLK, BUFF1, BUFF2, IERR)
C                                       make FO table
      IF (.NOT.SINGLE) THEN
         IF (CLVER.LE.0) CALL FNDEXT ('CL', CATUV, CLVER)
         CALL CL2FO (DISKIN, CNOIN, CLVER, LUN1, CATUV, DISK, CNO,
     *      LUN2, CATBLK, IDSOU, BUFF1, BUFF2, IERR)
         END IF
C
      CALL FNDEXT ('AN', CATUV, NSUB)
C                                       Get freq from AN table
      SUBA = 1
      CALL GETANT (DISKIN, CNOIN, SUBA, CATUV, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1480) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
      FQOFF = FREQ - SAFREQ
      IF (FQOFF.NE.0.0D0) THEN
         MSGTXT = 'Updating FREQ in AN tables'
         CALL MSGWRT (3)
         END IF
      IF (DOPOL.GT.0) THEN
         MSGTXT = 'Resetting pol cal values in output AN tables'
         CALL MSGWRT (3)
         END IF
C                                       Reference frequency in AN table
C                                       IF selection
      DO 300 SUBA = 1,NSUB
         CALL ISTAB ('AN', DISKIN, CNOIN, SUBA, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF ((EXIST) .AND. (IERR.EQ.0)) THEN
            CALL ANSEL (DISKIN, CNOIN, DISK, CNO, SUBA, SUBA, CATUV,
     *         CATBLK, LUN1, LUN2, BIF, EIF, FQOFF, DOPOL, IBUFF1,
     *         IBUFF2, IERR)
            IF (IERR.GT.0) GO TO 999
            END IF
 300     CONTINUE
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clear status
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISK, CNO, NAMOUT, CLAOUT, CATBLK(KIIMS),
     *   UTYPE, NLUSER, 'CLWR', BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPLTHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1020 FORMAT (A6,'STOKES = ''',A4,''' / Stokes type')
 1040 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 1060 FORMAT (A6,'BCHAN =',I4,', ECHAN =',I4,'/ Chan range')
 1080 FORMAT (A6,'SUBARRAY =',I4)
 1100 FORMAT (A6,'/ Edited using FG table version',I3)
 1120 FORMAT (A6,'SMOOTH = ',I1,',',F6.1,',',I4,
     *   ' / Spectral smoothing parms')
 1140 FORMAT (A6,'Avgd: Start, Stop, Inc ',2I5,I3,'  IF=',I3)
 1160 FORMAT (A6,'Output data in compressed format')
 1180 FORMAT (A6,'GAINUSE =',I3,' / CL table')
 1200 FORMAT (A6,'APARM(7) =',F5.2, ' / => assemble selected sources')
 1220 FORMAT (A6,'APARM(7) =',F5.2, ' / => split selected sources')
 1240 FORMAT (A6,'APARM(1) =',F5.2,' / => average in freq.')
 1260 FORMAT (A6,'APARM(1) =',F5.2,' / => average IFs and channels')
 1280 FORMAT (A6,'APARM(1) =',F5.2,' / => average ',I2,' increment',
     *   I2,' channels')
 1300 FORMAT (A6,'SOLINT =',F7.3,'min  / => averaged in time')
 1320 FORMAT (A6,'APARM(2) =',F5.2,' / Integration time (sec)')
 1340 FORMAT (A6,'DOPOL = ',I2,', polarization correction made')
 1360 FORMAT (A6,'BL table ',I3,' applied to data')
 1380 FORMAT (A6,'BP correction done, DOBAND = ',I2)
 1400 FORMAT (A6,'BP correction used BP table ',I2)
 1420 FORMAT (A6,'Weights calibrated')
 1440 FORMAT (A6,'Weights not calibrated')
 1460 FORMAT (A6,' /')
 1480 FORMAT ('SPLTHI: ERROR',I3,' READING OUTPUT AN TABLE')
      END
      SUBROUTINE SPLCOP (VISOFF, TOTREC, FINISH, DOAVG, DOIF, DOSUBS,
     *   DISK, CNO, NCHAVG, CHINC, NOUTCH, SOLINT, NOSUB, BUFF1, JBUFSZ,
     *   FREQO, OLDFRQ, NBASL, NUMHIS, HISCRD, CHNSEL, OUTCOM,
     *   LLOCWT, OUTSOU, LLOCSU, BUFF2, BUFF3, LRECU, NCRPM, NPRM,
     *   PRMTRN, AVGCH, CNTCHN, OLDRP,SOCOUN, RNXRET, IRET)
C-----------------------------------------------------------------------
C   SPLCOP reads and split/assemble  data files with optional averaging
C   over the frequency and time axes.
C   Input:
C      VISOFF   I         Offset in output file
C      TOTREC   I(2,3)    Total counts of record flagging
C      FINISH   L         If True, finish the source, compress, etc.
C      DOAVG    L         If true, average frequencies in IF
C      DOIF     L         If true, average across IF's also
C      DISK     I         Output disk number.
C      CNO      I         Output catalog slot number.
C      NOSUB    L         IF True drop subarray code.
C      JBUFSZ   I         Size of BUFF1
C      FREQO    D(*)      IF Frequency offsets for source (Hz)
C      OLDFRQ   D         Reference freq. of input data.
C      CHNSEL   I(3,10)   Start, stop and incr channel number to average
C      OUTCOM   L         If TRUE write compressed data
C      LLOCWT   I         Offset of compressed weight r.p in output data
C      OUTSOU   l         Add source number to random parms
C      LLOCSU   I         Where to add it
C      LRECU    I         length of output vis record
C   Input from common:
C      INCF     I         Increment in freq. of data from UVGET
C      INCIF    I         Increment in IF of data from UVGET
C      INCS     I         Increment in Stokes' of data from UVGET
C      JLOCF    I         Offset of freq. of data from UVGET
C      JLOCIF   I         Offset of IF of data from UVGET
C      JLOCS    I         Offset of Stokes' of data from UVGET
C   Output:
C      NUMHIS   I         Number of history entries
C      HISCR    C*64      History entries
C      BUFF1    R(*)      Output I/O buffer.
C      BUFF2    R(*)      Buffer for compressed data
C      SOCOUN   I(*)      Number of the given source occurances in the
C                         data
C      RNXRET   I         NX table error code in/out
C      IRET     I         Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   VISOFF, TOTREC(2,3), DISK, CNO, NCHAVG, CHINC, NOUTCH,
     *   JBUFSZ, NBASL, NUMHIS, CHNSEL(3,20,*), LLOCWT, LLOCSU, LRECU,
     *   NCRPM, CNTCHN(*), SOCOUN(*), IRET, PRMTRN(*), NPRM, RNXRET
      LOGICAL   FINISH, DOAVG, NOSUB, DOIF, OUTCOM, OUTSOU
      REAL      SOLINT, BUFF1(*), BUFF2(*), BUFF3(*), AVGCH(*), OLDRP
      DOUBLE PRECISION FREQO(MAXIF), OLDFRQ
      CHARACTER HISCRD(*)*64
C
      HOLLERITH CATH(256)
      CHARACTER NAME*48, TELTYP*4, OUTRAN(20)*8, BNDCOD(MAXIF)*8
      LOGICAL   T, F, DOFSCL, DOSUBS, EOF
      DOUBLE PRECISION FDIFF, CATD(128), FOFF(MAXIF), FOFF1, FREQO1
      INTEGER   LUN, FIND, BIND, LENBU, NIO, JNCIF, JNCS, NCOPY, J,
     *   NUMFRQ, NNIF, NOPOL, NCORR,  BO, I, XCOUNT, BLCODE, IIVER,
     *   FREQID, JNCF, TBIF, TEIF, MXIF, ANOTA(20), NWORDS, JTRIM,
     *   ISBAND(MAXIF), ISB(MAXIF), NPR, NPD, RNXSOR
      LONGINT   IPD, IPR
      REAL      VIS(MAXCIF*3), FRSCL, CATR(256), FINC(MAXIF), DTUTC,
     *   RBUFF(2), DBUFF(2), RPARM(20), FRPARM(20), FINCIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      SAVE  ANOTA
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, BO /18,1/
      DATA MXIF /MAXIF/
C-----------------------------------------------------------------------
C                                       Get sideband info
      IIVER = 1
      CALL CHNDAT ('READ', BUFF1, IUDISK, IUCNO, IIVER, CATUV, LUN,
     *   NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      CALL H2CHR (4, 1, CATH(KHTEL), TELTYP)
      DO 10 I = 1,MXIF
         IF ((FINC(I).LT.0.0) .OR. (TELTYP.EQ.'ATCA')) THEN
            ISB(I) = -1
         ELSE
            ISB(I) = 0
            END IF
 10      CONTINUE
C                                       Set lengths of input axes.
      NUMFRQ = ECHAN - BCHAN + 1
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = EIF - BIF + 1
      TBIF = BIF
      TEIF = EIF
      NOPOL = CATBLK(KINAX+JLOCS)
C                                       Check number of frequencies.
      IF ((NUMFRQ.GT.MAXCHA) .OR. (NUMIF.GT.MAXIF) .OR.
     *   (NOPOL*NUMIF*NUMFRQ.GT.MAXCIF)) THEN
         IRET = 1
         MSGTXT = 'SPLCOP: VISIBILITIES TOO BIG FOR BUFFERS'
         GO TO 990
         END IF

C                                       Set uvw freq. scaling
      FRSCL = CATD(KDCRV+JLOCF) / (OLDFRQ + 1.0D-15)
      DOFSCL = ABS (FRSCL-1.0).GT.1.0E-6
C                                       Set output increments
C                                       (averaging)
      IF (DOSUBS) THEN
         JNCIF = INCIF
         IF (JLOCF.LT.JLOCIF) JNCIF = INCIF / CHINC
         JNCS = INCS
         IF (JLOCF.LT.JLOCS) JNCS = INCS / CHINC
         JNCF = INCF
      ELSE
         IF (DOAVG .OR. DOIF) THEN
            JNCIF = INCIF
            IF (JLOCF.LT.JLOCIF) JNCIF = INCIF / NUMFRQ
            JNCS = INCS
            IF (JLOCF.LT.JLOCS) JNCS = INCS / NUMFRQ
            JNCF = INCF
         ELSE
            JNCIF = INCIF
            JNCS = INCS
            JNCF = INCF
            END IF
         END IF
C                                        If output file already open
C                                        close it.
      IF (FIND.GT.0) THEN
         IF ((FTAB(FIND).EQ.LUN) .AND. (VISOFF.LE.0))
     *      CALL ZCLOSE (LUN, FIND, IRET)
         END IF
C                                       Setup for new file
      IF (VISOFF.LE.0) THEN
C                                       Zero flag counts
         TOTREC(1,1) = 0
         TOTREC(2,1) = 0
         TOTREC(1,2) = 0
         TOTREC(2,2) = 0
         TOTREC(1,3) = 0
         TOTREC(2,3) = 0
C                                       Set output file name.
         CALL ZPHFIL ('UV', DISK, CNO, 1, NAME, IRET)
C                                       Open output file.
         CALL ZOPEN (LUN, FIND, DISK, NAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET
            GO TO 990
            END IF
C                                       Init vis file for write
         LENBU = 1
         NCORR = (LRECU - NCRPM) / CATBLK(KINAX)
         NCOPY = 3 * NCORR
         CALL UVINIT ('WRIT', LUN, FIND, CATBLK(KIGCN), VISOFF, LRECU,
     *      LENBU, JBUFSZ, BUFF2, BO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         END IF
C                                       initialize number of occurance
C                                       of the sources in the data
      CALL FILL (XSTBSZ, 0, SOCOUN)
      XCOUNT = 0
      DO 100 I = 1,NVIS+1
C                                       Average in freq in each IF
         CALL UVGET ('READ', RPARM, VIS, IRET)
         EOF = IRET.LT.0
         IF (EOF) THEN
            IF (SOLINT.GT.0.0) GO TO 80
            GO TO 110
            END IF
         IF (IRET.NE.0) GO TO 999
C                                       Average spectral chunks
         IF (DOSUBS) THEN
            CALL AVGSUB (VIS, NOPOL, NUMIF, NCHAVG, CHINC, NOUTCH, JNCS,
     *         JNCIF, JNCF, ISB, BUFF1)
C                                       Average in IF and freq.
         ELSE IF (DOIF) THEN
            CALL AVGCIF (VIS, NOPOL, BCHAN, ECHAN, TBIF, TEIF, CHNSEL,
     *         JNCS, BUFF1)
C                                       Average with channel selection
         ELSE IF (DOAVG) THEN
            CALL AVGCHN (VIS, NOPOL, BCHAN, ECHAN, TBIF, TEIF, CHNSEL,
     *         JNCS, JNCIF, BUFF1)
C                                       plain copy
         ELSE
            CALL RCOPY (NCOPY, VIS, BUFF1)
            END IF
         IF (ILOCSU.GT.-1) THEN
            CURSOU = RPARM(1+ILOCSU) + 0.1
C                                       Too many sources
            IF (CURSOU.GT.XSTBSZ) THEN
               IRET = 9
               WRITE (MSGTXT,1060)
               GO TO 990
            ELSE
               SOCOUN(CURSOU) = SOCOUN(CURSOU) + 1
               END IF
         ELSE IF (OUTSOU) THEN
            CURSOU = SOUWAN(1)
            SOCOUN(CURSOU) = SOCOUN(CURSOU) + 1
            END IF
C                                       Drop subarray if requested
         IF (NOSUB) THEN
            IF (ILOCB.GE.0) THEN
               BLCODE = RPARM(1+ILOCB)
               RPARM(1+ILOCB) = BLCODE
            ELSE
               RPARM(1+ILOCSA) = 1.0
               END IF
            END IF
C                                       Frequency scale uvw
         IF (DOFSCL) THEN
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * FRSCL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * FRSCL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * FRSCL
            END IF
C                                       restructure RPARM
         DO 20 J = 1,NPRM
            FRPARM(J) = RPARM(PRMTRN(J))
 20         CONTINUE
C                                       averaging
 80      IF (SOLINT.GT.0.0) THEN
            IF (I.EQ.1) THEN
               J = NRPARM
               NRPARM = NCRPM
               CALL LISRAN (OUTRAN, ANOTA)
               NRPARM = J
               NPD = 3 * NOUTCH * NUMIF * NOPOL
               NWORDS = (NPD * NBASL - 1) / 1024 + 1
               CALL ZMEMRY ('GET ', 'AVGTIM', NWORDS, DBUFF, IPD, IRET)
               IF (IRET.EQ.0) THEN
                  NPR = NCRPM + 2
                  NWORDS = (NPR * NBASL - 1) / 1024 + 1
                  CALL ZMEMRY ('GET ', 'AVGTIM', NWORDS, RBUFF, IPR,
     *               IRET)
                  END IF
               IF (IRET.NE.0) THEN
                  MSGTXT = 'FAILED TO GET DYNAMIC MEMORY FOR AVERAGING'
                  GO TO 990
                  END IF
               END IF
C                                       data time -UTC in days
            DTUTC = ANTIAT / 86400.0
C                                       time average
            CALL AVGTIM (SOLINT, NOUTCH, NUMIF, NOPOL, XCOUNT, FRPARM,
     *         BUFF1, BUFF2, BUFF3, NBASL, OUTCOM, LLOCWT, OUTSOU,
     *         LLOCSU, SOUWAN(1), NCRPM, JNCIF, JNCS, JNCF, NCORR, BIND,
     *         I, LUN, EOF, DTUTC, FIND, ANOTA, NPR, RBUFF(1+IPR), NPD,
     *         DBUFF(1+IPD), RNXRET, IRET)
            IF (EOF) GO TO 110
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1160) IRET
               GO TO 990
               END IF
C                                       Write new without time ave.
         ELSE
            NIO = 1
            XCOUNT = XCOUNT + 1
            CALL RCOPY (NCRPM, FRPARM, BUFF2(BIND))
            RNXSOR = 1
            IF (OUTSOU) THEN
               BUFF2(BIND+LLOCSU) = SOUWAN(1)
               RNXSOR = SOUWAN(1)
               END IF
C                                       update NX table
            CALL RNXUPD (BUFF2(BIND), RNXSOR, RNXRET)
            IF (OUTCOM) THEN
               CALL ZUVPAK (NCORR, BUFF1, BUFF2(BIND+LLOCWT),
     *            BUFF2(BIND+NCRPM))
            ELSE
               CALL RCOPY (NCOPY, BUFF1, BUFF2(BIND+NCRPM))
               END IF
            CALL UVDISK ('WRIT', LUN, FIND, BUFF2, NIO, BIND, IRET)
            IF (IRET.NE.0) THEN
              WRITE (MSGTXT,1160) IRET
               GO TO 990
               END IF
            END IF
 100        CONTINUE
C                                       If FINISH shut down output
 110  IF (XCOUNT.LE.0) THEN
         WRITE (MSGTXT,1110) NVIS
         CALL MSGWRT (6)
         J = JTRIM (SOURCS(1))
         WRITE (MSGTXT,1111) SOURCS(1)(:J), SELCOD, SELQUA
         CALL MSGWRT (6)
         WRITE (MSGTXT,1112) SUBARR
         CALL MSGWRT (6)
         END IF
      NVIS = XCOUNT
C                                       Sum flag counts
      TOTREC(1,1) = TOTREC(1,1) + CNTREC(1,1)
      TOTREC(2,1) = TOTREC(2,1) + CNTREC(2,1)
      TOTREC(1,2) = TOTREC(1,2) + CNTREC(1,2)
      TOTREC(2,2) = TOTREC(2,2) + CNTREC(2,2)
      TOTREC(1,3) = TOTREC(1,3) + CNTREC(1,3)
      TOTREC(2,3) = TOTREC(2,3) + CNTREC(2,3)
C                                       Flush output
      IF (.NOT.FINISH) GO TO 800
         NIO = 0
         CALL UVDISK ('FLSH', LUN, FIND, BUFF2, NIO, BIND, IRET)
         IF (IRET.EQ.0) GO TO 120
            WRITE (MSGTXT,1160) IRET
            GO TO 990
C                                       Compress output file.
 120     NVIS = XCOUNT + VISOFF
         CALL UCMPRS (NVIS, DISK, CNO, LUN, CATBLK, IRET)
C                                      Put vis. count in CATBLK
         CATBLK(KIGCN) = NVIS
C                                       Update CATBLK.
         CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFF1, IRET)
         IF (IRET.EQ.0) GO TO 140
            WRITE (MSGTXT,1180) IRET
C                                       Copy relevant portion of IF
C                                       table.
 140     IF (JLOCIF.GT.0) THEN
C                                       Read old
            IIVER = 1
            CALL CHNDAT ('READ', BUFF1, IUDISK, IUCNO, IIVER, CATUV,
     *         LUN, NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Fixup
            IF (DOIF) THEN
               NNIF = 1
               FOFF(1) = 0.0D0
               FINC(1) = CATR(KRCIC+JLOCF)
               TBIF = 1
            ELSE
               IF (DOAVG) THEN
                  DO 145 I = TBIF,TEIF
                     FOFF(I) = FOFF(I) + (AVGCH(I)-OLDRP)*FINC(I)
 145                 CONTINUE
                  END IF
               FDIFF = CATD(KDCRV+JLOCF) - OLDFRQ
               NNIF = TEIF - TBIF + 1
               FOFF1 = FOFF(TBIF)
               FREQO1 = FREQO(TBIF)
               FINCIN = FINC(TBIF)
               DO 150 I = TBIF,TEIF
                  IF (DOSUBS) THEN
                     FOFF(I) = FOFF(I) + (FINC(I) - FINCIN) *
     *                  (AVGCH(TBIF) - OLDRP)
                     FINC(I) = FINC(I) * CHINC
                  ELSE IF (DOAVG) THEN
                     FINC(I) = FINC(I) * CNTCHN(I)
                     END IF
C                                       force the first IF to zero
                  FOFF(I) = FOFF(I) - FOFF1 + FREQO(I) - FREQO1
 150              CONTINUE
               END IF
C                                       Output ref IF = 1
            CATD(KDCRV+JLOCIF) = 1.0D0
            CATR(KRCRP+JLOCIF) = 1.0
C                                       Rewrite new
            IIVER = 1
            FREQID = 1
            CALL CHNDAT ('WRIT', BUFF1, DISK, CNO, IIVER, CATBLK, LUN,
     *         NNIF, FOFF(TBIF), ISBAND(TBIF), FINC(TBIF), BNDCOD(TBIF),
     *         FREQID, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Give data summary and history
         IF (DOCAL) THEN
            WRITE (MSGTXT,1200)
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),1200)
            NUMHIS = NUMHIS + 1
            WRITE (MSGTXT,1220) TOTREC(1,1), TOTREC(1,2), TOTREC(1,3)
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),1220) TOTREC(1,1), TOTREC(1,2),
     *         TOTREC(1,3)
            NUMHIS = NUMHIS + 1
            WRITE (MSGTXT,1240) TOTREC(2,1), TOTREC(2,2), TOTREC(2,3)
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),1240) TOTREC(2,1), TOTREC(2,2),
     *         TOTREC(2,3)
            NUMHIS = NUMHIS + 1
         ELSE
            WRITE (MSGTXT,1260) NVIS
            IF (OUTCOM) WRITE (MSGTXT,1280) NVIS
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),1260) NVIS
            NUMHIS = NUMHIS + 1
            END IF
         IF (NVIS.GT.0) GO TO 700
C                                       No data found.
            IRET = 9
            MSGTXT = 'SPLCOP: ERROR - NO DATA WRITTEN'
            GO TO 990
C                                       Close files
 700     CALL ZCLOSE (LUN, FIND, IRET)
 800  CALL UVGET ('CLOS', BUFF1(BIND), BUFF1(BIND+NRPARM), IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPLCOP: ERROR',I5,' OPENING OUTPUT FILE')
 1020 FORMAT ('SPLCOP: ERROR',I5,' INIT. OUTPUT FILE')
 1060 FORMAT ('SPLCOP: TOO MANY SOURCES IN THE DATA')
 1110 FORMAT ('SPLCOP FOUND NO SAMPLES OUT OF',I8,' POSSIBLE')
 1111 FORMAT ('       SOURCE, CALCODE, QUAL= ''',A,'''  ''',A,'''',I5)
 1112 FORMAT ('       IS SUBARRAY',I3,' IN CL/SN TABLE?')
 1160 FORMAT ('SPLCOP: ERROR',I5,' WRITING OUTPUT FILE')
 1180 FORMAT ('SPLCOP: ERROR',I3,' UPDATING CATALOGUE HEADER')
 1200 FORMAT (10X,' Previously flagged ','  flagged by gain   ',
     *   '      kept')
 1220 FORMAT ('Partially ',2(I15,5X),I10)
 1240 FORMAT ('Fully     ',2(I15,5X),I10)
 1260 FORMAT (I9,' Visibilities written')
 1280 FORMAT (I9,' Compressed visibilities written')
      END
      SUBROUTINE UPDKEY (BUFFER, KEYWRD, KEYTYP, KEYVAL, IERR)
C-----------------------------------------------------------------------
C   Routine which updates a keyword-value pairs of an existing
C   table.
C   Inputs:
C      BUFFER   I(*)   Work buffer
C      KEYWRD   C*8    Keyword name
C      KEYTYP   I      Keyword type
C      KEYVAL   D      Keyword value
C   Outputs:
C      IERR     I      Error code, 0 => OK
C                         anything else => problem
C-----------------------------------------------------------------------
      CHARACTER KEYWRD*8
      INTEGER   LOCS, KEYTYP, KEYNUM
      INTEGER   BUFFER(*), IERR
      DOUBLE PRECISION KEYVAL
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
      LOCS = 1
      KEYNUM = 1
C
      CALL TABKEY ('WRIT', KEYWRD, KEYNUM, BUFFER, LOCS,
     *   KEYVAL, KEYTYP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UPDKEY: ERROR ',I3,' UPDATING CL KEYWORDS')
      END
      SUBROUTINE COPTAB (SOCOUN, IRET)
C-----------------------------------------------------------------------
C   Updates tables for selection by IF
C   Input:
C      SOCOUN(*)   I  Number of the given source occurances in the data
C   Inputs in common:
C      BIF         I  First IF
C      EIF         I  Highest IF selected
C      FQOFF       D  Frequency offset
C      SELIF       L  Select IFs or not
C      CNTCHN      I  Number of averaged frequency channels
C   Output:
C      IRET        I  Return code, 0=>OK
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      DOUBLE PRECISION TB, TE, CATD(128)
      INTEGER   IERR, LUN1, LUN2, VER, NVER, OFQID, ISUB, JSUB, NA,
     *   AN(50), I, SOCOUN(*), LEIF, BPOL, EPOL, IROUND, BVER
      LOGICAL   TABLE, EXIST, FITASC
      REAL      CATR(256), TEMP
      INCLUDE 'SPLAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATBLK, CATD, CATR)
      DATA LUN1, LUN2 /28,29/
C-----------------------------------------------------------------------
      IF (DOFLAG) THEN
         IFLUN = 30
         CALL FLGINI ('READ', FGBUFF, IUDISK, IUCNO, FGVER, CATUV,
     *      IFLUN, IFGRNO, FGKOLS, FGNUMV, IERR)
         IF (IERR.NE.0) DOFLAG = .FALSE.
         END IF
C                                       TSTART start of data in days
C                                       TEND end of data in days
C                                       TSTART, TEND is outout of
C                                       UVGET('INIT'..)
C                                       TB, TE should be DOUBLE PR.
C                                       for ...SEL routines
      TB = TSTART
      TE = TEND
      LEIF = EIF
      IF (DOIF) LEIF = BIF
      NA = 0
      DO 20 I = 1,50
         AN(I) = 0
 20      CONTINUE
      MSGTXT = 'Updating tables for IF/FREQID/channel selection'
      CALL MSGWRT (4)
C
      IF (SUBARR.GT.0) THEN
         ISUB = SUBARR
         JSUB = 1
      ELSE
         ISUB = 0
         JSUB = -1
         IF (APARM(3) .GT. 0.0001) JSUB = 1
         END IF
C                                       polarization
      IF (CATUV(KINAX+JLOCS).EQ.CATBLK(KINAX+JLOCS)) THEN
         BPOL = 1
         EPOL = MIN (2, CATBLK(KINAX+JLOCS))
      ELSE
         TEMP = CATD(KDCRV+JLOCS) + (1 - CATR(KRCRP+JLOCS)) *
     *      CATR(KRCIC+JLOCS)
         BPOL = ABS (IROUND (TEMP))
         EPOL = MIN (2, CATBLK(KINAX+JLOCS))
         EPOL = MAX (EPOL, BPOL)
         END IF
C                                       Revise tables: note depends on
C                                       doing loops 0 times if none
      OFQID = FRQSEL
C                                       copy BP tables only if
C                                       polarization callibration was
C                                       not applied
      CALL FNDEXT ('BP', CATUV, NVER)
      IF (DOBAND.GT.0) NVER = 0
      DO 120 VER = 1,NVER
         CALL ISTAB ('BP', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL BPSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, LEIF, BCHAN, ECHAN, TB, TE, OFQID, ISUB, JSUB, BUFF1,
     *      BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 120     CONTINUE
C                                       copy no CL table, even CL 1 has
C                                       calibration data supposing that
C                                       CL table cal was applied
C                                       NA number of selected antennas
C                                       AN(*) list of the sel. antennas
      CALL FNDEXT ('CL', CATUV, NVER)
      IF (ASSEMB) THEN
         IF ((DOCAL) .AND. (NVER.GT.0)) THEN
            VER = 1
            CALL ISTAB ('CL', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *         EXIST, FITASC, IERR)
C                                       do NOT select on sources
            IF (EXIST .AND. (IERR.EQ.0)) THEN
               CALL CLNULL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, LEIF, OFQID, TB,
     *            TE, 0, SOUWAN, AN, NA, ISUB, JSUB, IBUFF1, IBUFF2,
     *            IRET)
               IF (IRET.GT.0) GO TO 999
               IF (IRET.EQ.0) CALL CLTIDY (DISKO, FCNO(1), SOCOUN, VER,
     *            IRET)
               END IF
         ELSE
            DO 140 VER = 1,NVER
               CALL ISTAB ('CL', DISKIN, FCNO(2), VER, LUN1, BUFF1,
     *            TABLE, EXIST, FITASC, IERR)
C                                       do NOT select on sources
               IF (EXIST .AND. (IERR.EQ.0)) THEN
                  CALL CLSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER,
     *               CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, LEIF,
     *               OFQID, TB, TE, 0, SOUWAN, AN, NA, ISUB, JSUB,
     *               IBUFF1, IBUFF2, IRET)
                  IF (IRET.GT.0) GO TO 999
                  IF (IRET.EQ.0) CALL CLTIDY (DISKO, FCNO(1), SOCOUN,
     *               VER, IRET)
                  END IF
 140           CONTINUE
            END IF
         END IF
C                                       CD tables
      CALL FNDEXT ('CD', CATUV, NVER)
      DO 145 VER = 1,NVER
         CALL ISTAB ('CD', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do not select on antenna
         IF (EXIST.AND.(IERR.EQ.0)) CALL CDSEL (DISKIN, FCNO(2), DISKO,
     *      FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF,
     *      LEIF, OFQID, AN, 0, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 145     CONTINUE
C                                       CP tables
      CALL FNDEXT ('CP', CATUV, NVER)
      DO 150 VER = 1,NVER
         CALL ISTAB ('CP', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CPSEL (DISKIN, FCNO(2), DISKO,
     *      FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, BIF, LEIF, BCHAN,
     *      ECHAN, OFQID, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 150     CONTINUE
C                                       CQ tables
      CALL FNDEXT ('CQ', CATUV, NVER)
      DO 160 VER = 1,NVER
         CALL ISTAB ('CQ', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CQSEL (DISKIN, FCNO(2), DISKO,
     *      FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, BIF, LEIF, OFQID,
     *      ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 160     CONTINUE
C                                       copy FG tables if flaging was
C                                       not applied
      CALL FNDEXT ('FG', CATUV, NVER)
      IF ((FGVER.GT.0) .AND. (NVER.GT.0)) THEN
         BVER = FGVER + 1
         IF (NVER.LE.FGVER) THEN
            MSGTXT = 'WARNING: NO FG TABLES ARE COPIED SINCE HIGHEST' //
     *         ' WAS APPLIED'
            NVER = 0
         ELSE
            WRITE (MSGTXT,1160) BVER, NVER
            END IF
         CALL MSGWRT (6)
         END IF
      DO 180 VER = BVER,NVER
         CALL ISTAB ('FG', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0) .AND. (VER.GT.FGVER))
     *      CALL FGSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *      CATBLK, LUN1, LUN2, BIF, LEIF, BCHAN, ECHAN,  TB,
     *      TE, OFQID, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 180     CONTINUE
C                                       copy GC tables if calibration
C                                       was not applied
      CALL FNDEXT ('GC', CATUV, NVER)
      DO 200 VER = 1,NVER
         CALL ISTAB ('GC', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL GCSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BCHAN, ECHAN, BIF, LEIF, OFQID, AN, NA, ISUB, JSUB, IBUFF1,
     *      IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 200     CONTINUE
C                                       IM tables
      CALL FNDEXT ('IM', CATUV, NVER)
      DO 220 VER = 1,NVER
         CALL ISTAB ('IM', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL IMSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, LEIF, OFQID, TB, TE, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 220     CONTINUE
C                                       MC tables
      CALL FNDEXT ('MC', CATUV, NVER)
      DO 240 VER = 1,NVER
         CALL ISTAB ('MC', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL MCSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, LEIF, OFQID, TB, TE, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 240     CONTINUE

C                                       copy PC tables if calibration
C                                       was not applied
      CALL FNDEXT ('PC', CATUV, NVER)
      IF (DOCAL) NVER = 0
      DO 260 VER = 1,NVER
         CALL ISTAB ('PC', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL PCSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, LEIF, OFQID, TB, TE, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 260     CONTINUE
C                                       PD table if not applied
      CALL FNDEXT ('PD', CATUV, NVER)
      IF (DOPOL.LE.0) THEN
         DO 270 VER = 1,NVER
            CALL ISTAB ('PD', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *         EXIST, FITASC, IERR)
            IF (EXIST .AND. (IERR.EQ.0)) CALL PDSEL (DISKIN, FCNO(2),
     *         DISKO, FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, BPOL,
     *         EPOL, BIF, LEIF, BCHAN, ECHAN, OFQID, ISUB, JSUB, IBUFF1,
     *        IBUFF2, IRET)
            IF (IRET.NE.0) GO TO 999
 270        CONTINUE
         END IF
C                                       copy SN tables if calibration
C                                       was not applied
      CALL FNDEXT ('SN', CATUV, NVER)
      IF (DOCAL) NVER = 0
      DO 280 VER = 1,NVER
         CALL ISTAB ('SN', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            IF (DOFLAG) THEN
               CALL SNFSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, LEIF, OFQID, TB,
     *            TE, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
            ELSE
               CALL SNSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, LEIF, OFQID, TB,
     *            TE, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
            END IF
 280     CONTINUE
C                                       SY tables
      CALL FNDEXT ('SY', CATUV, NVER)
      DO 290 VER = 1,NVER
         CALL ISTAB ('SY', DISKIN, CNOIN, VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            IF (DOFLAG) THEN
               CALL SYFSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, LEIF, OFQID, TB,
     *            TE, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB, IBUFF1,
     *            IBUFF2, IRET)
            ELSE
               CALL SYSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, LEIF, OFQID, TB,
     *            TE, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB, IBUFF1,
     *            IBUFF2, IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
            END IF
 290     CONTINUE
C                                       SU tables
      CALL FNDEXT ('SU', CATUV, NVER)
C                                       Do not create SU table if only
C                                       one source is selected
      CALL AXEFND (8, 'SOURCE  ', CATBLK(KIPCN), CATBLK(KHPTP), VER,
     *   IERR)
      IF (IERR.NE.0) NVER = 0
      DO 300 VER = 1,NVER
         CALL ISTAB ('SU', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            CALL SUSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *         CATBLK, LUN1, LUN2, BIF, LEIF, OFQID, IBUFF1, IBUFF2,
     *         CNTCHN(1), REFCOR, IRET)
            IF (IRET.GT.0) GO TO 999
            IF (IRET.EQ.0) CALL SUTIDY (DISKO, FCNO(1), SOCOUN, VER,
     *         IRET)
            END IF
 300     CONTINUE
C                                       copy TY tables if calibration
C                                       was not applied
      CALL FNDEXT ('TY', CATUV, NVER)
      DO 320 VER = 1,NVER
         CALL ISTAB ('TY', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            IF (DOFLAG) THEN
               CALL TYFSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF, OFQID, TB,
     *            TE, AN, NA, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
            ELSE
               CALL TYSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, LEIF, OFQID, TB,
     *            TE, AN, NA, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
            END IF
 320     CONTINUE
C                                       WX tables
      CALL FNDEXT ('WX', CATUV, NVER)
      DO 340 VER = 1,NVER
         CALL ISTAB ('WX', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL WXSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, TB, TE, AN,
     *      NA, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 340     CONTINUE
C                                       BL tables
      CALL FNDEXT ('BL', CATUV, NVER)
      IF (DOBL) NVER = 0
      DO 350 VER = 1,NVER
         CALL ISTAB ('BL', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL BLSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      AN, NA, ISUB, JSUB, BIF, LEIF, OFQID, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 350     CONTINUE
C                                       close FG
      IF (DOFLAG) CALL TABIO ('CLOS', 0, VER, IBUFF1, FGBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1160 FORMAT ('WARNING: ONLY FG TABLE VERSIONS',I4,' TO',I4,' COPIED')
      END
      SUBROUTINE SUTIDY (DISK, ISLOT, SOCOUN, IN, IERR)
C-----------------------------------------------------------------------
C  Routine to tidy up the SU table after the loading has finished.
C  During loading the array SOCOUN(*) keeps track of how many entries
C  a particular source has, if any have zero then they are removed
C  from the SU table.
C  Inputs:
C    DISK       I       Volume on which data reside
C    ISLOT      I       Catalogue number of file
C    SOCOUN     I(*)    Array with # times source written
C    IN         I       input version number
C  Outputs:
C    IERR       I       Error code, 0 => OK.
C-----------------------------------------------------------------------
      INTEGER   DISK, ISLOT, SOCOUN(*), IN, IERR
C
      INTEGER   OUT, LUNIN, LUNOUT, BUFF1(512), BUFF2(512), I, NSOU
      INCLUDE 'INCS:PUVD.INC'
C                                       Declarations for SOUINI
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC), ISURNO, NOBAND,
     *   IFQ, SOKOLS(MAXSUC), SONUMV(MAXSUC), ISORNO
      CHARACTER VELTYP*8, VELDEF*8
C                                       Declarations for TABSOU
      CHARACTER SOUNAM*16, TCALC*4
      REAL      FLUX(4,MAXIF)
      INTEGER   IDSOU, TQUAL
      DOUBLE PRECISION  FREQO(MAXIF), LSRVEL(MAXIF), LRESTF(MAXIF),
     *   BANDW, TRAEPO, TDCEPO, TRAAPP, TDCAPP, TEQUIN, TPMRA, TPMDEC,
     *   TRAOBS, TDECOB
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Copy SU IN -> SU MAX + 1
      OUT = 0
      LUNIN = 45
      LUNOUT = 46
      I = MSGSUP
      MSGSUP = 32005
      CALL TABCOP ('SU', IN, OUT, LUNIN, LUNOUT, DISK, DISK, ISLOT,
     *   ISLOT, CATBLK, BUFF1, BUFF2, IERR)
      MSGSUP = I
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Delete SU 1
      CALL RMEXT (DISK, ISLOT, 'SU', IN, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Open SU table #2
      CALL SOUINI ('READ', BUFF2, DISK, ISLOT, OUT, CATBLK, LUNOUT,
     *   NOBAND, VELTYP, VELDEF, IFQ, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('READ', 'SOUINI', 'SUTIDY', IERR)
         GO TO 990
         ENDIF
      NSOU = BUFF2(5)
C                                       Open SU table #1
      CALL SOUINI ('WRIT', BUFF1, DISK, ISLOT, IN, CATBLK, LUNIN,
     *   NOBAND, VELTYP, VELDEF, IFQ, ISORNO, SOKOLS, SONUMV, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('WRIT', 'SOUINI', 'SUTIDY', IERR)
         GO TO 990
         ENDIF
C                                       Loop & copy
      DO 100 I = 1, NSOU
         ISURNO = I
         CALL TABSOU ('READ', BUFF2, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW, TRAEPO, TDCEPO,
     *      TEQUIN, TRAAPP, TDCAPP, TRAOBS, TDECOB, LSRVEL, LRESTF,
     *      TPMRA, TPMDEC, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABSOU', 'SUTIDY', IERR)
            GO TO 999
            END IF
C                                       Check have valid data
         IF (SOCOUN(IDSOU).EQ.0) GO TO 100
C                                       If so, write
         CALL TABSOU ('WRIT', BUFF1, ISORNO, SOKOLS, SONUMV, IDSOU,
     *      SOUNAM, TQUAL, TCALC, FLUX, FREQO, BANDW, TRAEPO, TDCEPO,
     *      TEQUIN, TRAAPP, TDCAPP, TRAOBS, TDECOB, LSRVEL, LRESTF,
     *      TPMRA, TPMDEC, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABSOU', 'SUTIDY', IERR)
            GO TO 999
            END IF
 100     CONTINUE
C                                       Close tables
      CALL TABIO ('CLOS', 1, ISURNO, BUFF2, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SUTIDY', IERR)
         GO TO 999
         END IF
      CALL TABIO ('CLOS', 1, ISORNO, BUFF1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SUTIDY', IERR)
         GO TO 999
         END IF
C                                       Delete SU 2
      CALL RMEXT (DISK, ISLOT, 'SU', OUT, CATBLK, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUTIDY: ERROR',I3,' COPYING SU #1 -> SU #2')
 1010 FORMAT ('SUTIDY: ERROR',I3,' DELETING SU #1')
      END
      SUBROUTINE CLTIDY (DISK, ISLOT, SOCOUN, IN, IERR)
C-----------------------------------------------------------------------
C  Routine to tidy up the CL table after the loading has finished.
C  During loading the array SOCOUN(*) keeps track of how many entries
C  a particular source has, if any have zero then they are removed
C  from the SU table.
C  Inputs:
C    DISK       I       Volume on which data reside
C    ISLOT      I       Catalogue number of file
C    SOCOUN     I(*)    Array with # times source written
C    IN         I       input version number
C  Outputs:
C    IERR       I       Error code, 0 => OK.
C-----------------------------------------------------------------------
      INTEGER   DISK, ISLOT, SOCOUN(*), IN, IERR
C
      INTEGER   OUT, LUNIN, LUNOUT, BUFF1(512), BUFF2(512), I
      INCLUDE 'INCS:PUVD.INC'
C                                       Declarations for CALINI
      INTEGER   CLKOLS(MAXCLC), CLNUMV(MAXCLC), ICLRNO, NUMANT, NUMPOL,
     *   NUMIF, NTERM, SOKOLS(MAXCLC), SONUMV(MAXCLC), OCLRNO, NREC
      REAL      GMMOD
C                                       Declarations for TABCAL
      DOUBLE PRECISION TIME, GEODLY(10)
      REAL      TIMEI, IFR, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF)
      INTEGER   SOURID, ANTNO, SUBA, FREQID, REFA(2,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Copy CL IN -> CL MAX + 1
      OUT = 0
      LUNIN = 45
      LUNOUT = 46
      I = MSGSUP
      MSGSUP = 32005
      CALL TABCOP ('CL', IN, OUT, LUNIN, LUNOUT, DISK, DISK, ISLOT,
     *   ISLOT, CATBLK, BUFF1, BUFF2, IERR)
      MSGSUP = I
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Delete CL 1
      CALL RMEXT (DISK, ISLOT, 'CL', IN, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Open CL table #2
      CALL CALINI ('READ', BUFF2, DISK, ISLOT, OUT, CATBLK, LUNOUT,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('READ', 'CALINI', 'CLTIDY', IERR)
         GO TO 990
         ENDIF
      NREC = BUFF2(5)
C                                       Open CL table #1
      CALL CALINI ('WRIT', BUFF1, DISK, ISLOT, IN, CATBLK, LUNIN,
     *   OCLRNO, SOKOLS, SONUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('WRIT', 'CALINI', 'CLTIDY', IERR)
         GO TO 990
         ENDIF
C                                       Loop & copy
      DO 100 I = 1,NREC
         ICLRNO = I
         CALL TABCAL ('READ', BUFF2, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABCAL', 'CLTIDY', IERR)
            GO TO 999
            END IF
C                                       Check have valid data
         IF (SOCOUN(SOURID).GT.0) THEN
C                                       If so, write
            CALL TABCAL ('WRIT', BUFF1, OCLRNO, CLKOLS, CLNUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *         GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *         DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *         IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('READ', 'TABSOU', 'CLTIDY', IERR)
               GO TO 999
               END IF
            END IF
 100     CONTINUE
C                                       Close tables
      CALL TABIO ('CLOS', 1, ICLRNO, BUFF2, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CLTIDY', IERR)
         GO TO 999
         END IF
      CALL TABIO ('CLOS', 1, OCLRNO, BUFF1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CLTIDY', IERR)
         GO TO 999
         END IF
C                                       Delete CL 2
      CALL RMEXT (DISK, ISLOT, 'CL', OUT, CATBLK, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLTIDY: ERROR',I3,' COPYING CL #1 -> CL #2')
 1010 FORMAT ('CLTIDY: ERROR',I3,' DELETING CL #1')
      END
      SUBROUTINE AVGSUB (VIS, NUMPOL, NUMIF, NCHAVG, CHINC, NOUTCH,
     *   JNCS, JNCIF, JNCF, ISB, VISOUT)
C-----------------------------------------------------------------------
C   Routine to diminish number of frequency chanels averaging a
C   group of them.
C   Inputs:
C      VIS      R(*)      Array containing the input visibility data
C                         (Re, Im, Wt)
C      NUMPOL   I         Number of polarizations
C      NUMIF    I         Number of IF's
C      NCHAVG   I         # channels to average to form 1 o/p channel
C      CHINC    I         increment in input channels to output
C      NOUTCH   I         # output channels
C      JNCS     I         Index for polzn in output array
C      JNCIF    I         Index for IF in output array
C      JNCF     I         Index for FREQ in output array
C      ISB      I(*)      Sidband of each IF
C   Output:
C      VISOUT   R(*)      Output visibility
C-----------------------------------------------------------------------
      INTEGER   NUMPOL, NUMIF, NCHAVG, CHINC, NOUTCH, JNCS,
     *   JNCIF, JNCF, ISB(*)
      REAL      VIS(*), VISOUT(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LOOPS, LOOPIF, LOOPF, INDEX, INP, OUTDEX, I
      REAL      SUMWT, SUMRE, SUMIM, WT, XNORM
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Check increment in channel
C                                       selector array and default
C                                       values of start and stop
C                                       Select/average
      DO 70 LOOPS = 1,NUMPOL
         DO 60 LOOPIF = 1,NUMIF
            INDEX = 1 + (LOOPS-1)*INCS + (LOOPIF-1)*INCIF
C                                       Upper sideband
            IF (ISB(LOOPIF).GE.0) THEN
               DO 30 LOOPF = 1, NOUTCH
                  OUTDEX = 1 + (LOOPS-1)*JNCS + (LOOPIF-1)*JNCIF +
     *               (LOOPF-1)*JNCF
                  SUMWT = 0.0
                  SUMRE = 0.0
                  SUMIM = 0.0
                  DO 20 I = 1,NCHAVG
                     INP = INDEX + (((LOOPF-1)*CHINC) + I-1)*INCF
                     WT = VIS(INP+2)
                     IF (WT.GT.0.0) THEN
                        SUMRE = SUMRE + VIS(INP)*WT
                        SUMIM = SUMIM + VIS(INP+1)*WT
                        SUMWT = SUMWT + WT
                        END IF
 20                  CONTINUE
                  XNORM = 1.0
                  IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
                  VISOUT(OUTDEX) = SUMRE * XNORM
                  VISOUT(OUTDEX+1) = SUMIM * XNORM
                  VISOUT(OUTDEX+2) = SUMWT
 30               CONTINUE
                  END IF
C                                       Lower sideband
            IF (ISB(LOOPIF).LT.0) THEN
               DO 50 LOOPF = NOUTCH, 1, -1
                  OUTDEX = 1 + (LOOPS-1)*JNCS + (LOOPIF-1)*JNCIF +
     *               (LOOPF-1)*JNCF
                  SUMWT = 0.0
                  SUMRE = 0.0
                  SUMIM = 0.0
                  DO 40 I = NCHAVG,1,-1
                     INP = INDEX + (((LOOPF-1)*CHINC) + I-1)*INCF
                     WT = VIS(INP+2)
                     IF (WT.GT.0.0) THEN
                        SUMRE = SUMRE + VIS(INP)*WT
                        SUMIM = SUMIM + VIS(INP+1)*WT
                        SUMWT = SUMWT + WT
                        END IF
 40                  CONTINUE
                  XNORM = 1.0
                  IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
                  VISOUT(OUTDEX) = SUMRE * XNORM
                  VISOUT(OUTDEX+1) = SUMIM * XNORM
                  VISOUT(OUTDEX+2) = SUMWT
 50               CONTINUE
                  END IF
 60         CONTINUE
 70      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE AVGTIM (SOLINT, NOUTCH, NUMIF, NOPOL, XCOUNT, RPARM,
     *   BUFF1, BUFF2, BUFF3, NBASL, OUTCOM, LLOCWT, OUTSOU, LLOCSU,
     *   NUMSOU, NRPRIN,JNCIF, JNCS, JNCF, NCORR, BIND, I, LUN, EOF,
     *   DTUTC, FIND, ANOTA, NPR, RBUFF, NPD, DBUFF, RNXRET, IRET)
C-----------------------------------------------------------------------
C   AVGTIM averages the visibilities in time axis
C   Input:
C      SOLINT   R      Time of averaging in days
C      NOUTCH   I      Number of frequencies at the output file
C      NUMIF    I      Number of IFs at the output file
C      NOPOL    I      Number of polarization at the output file
C      RPARM    R(20)  Output random parameters
C      BUFF1    R(*)   Array of read visibilities
C      BUFF2    R(*)   Array used for writing data
C      BUFF3    R(*)   Intermediate array
C      NBASL    I      Number of baseline
C      OUTCOM   L      If TRUE write compressed data
C      LLOCWT   I      Offset of compressed weight r.p in output data
C      OUTSOU   L      Force source number to random parameters
C      LLOCSU   I      Where to force it
C      NRPRIN   I      # random parms in uncompressed vis record
C      JNCIF    I      Increment in IF of data
C      JNCS     I      Increment in Stokes' of data
C      JNCF     I      Increment in frequencies of data
C      NCORR    I      Length of the vis part at BUFF1
C      I        I      The read visibility number
C      LUN      I      Number of device
C      EOF      L      End of file
C      DTUTC    R      Time difference of data and UTC, days
C      NPR      I      Number parameters per baseline (incl overhead)
C      NPD      I      Number data words / baseline
C   Input/Output:
C      XCOUNT   I      Number of visibilities
C      BIND     I      Pointer at the output data
C      RBUFF    R(*)   Random parameter summing area
C      DBUFF    R(*)   Data summing area
C   Output:
C      BUFF1    R(*)   Output I/O buffer.
C      BUFF2    R(*)   Buffer for compressed data
C      BUFF3    R(*)   Buffer to store the read data
C      IRET     I      Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   NOUTCH, NUMIF, NOPOL, XCOUNT, NBASL, LLOCWT, LLOCSU,
     *   NUMSOU, NRPRIN, JNCIF, JNCS, JNCF, NCORR, BIND, I, LUN, FIND,
     *   ANOTA(*), NPR, NPD, RNXRET, IRET
      LOGICAL   OUTCOM, OUTSOU, EOF
      REAL      SOLINT, RPARM(*), BUFF1(*), BUFF2(*), BUFF3(*),
     *   DBUFF(NPD,*), RBUFF(NPR,*)
C
      INTEGER   NIO, K, IBAS, KBAS, CURSOU, LOOPS, LOOPIF, LOOPF, INDEX,
     *   INP, LBIND, IDAY, IROUND, PRESOU, L, J, ICUR, IP, RNXSOR,
     *   CURA1, CURA2, CURSA
      REAL      CURTMI, XNORM, WT, SUMWT, DTUTC, WTOUT, CURBAS
      DOUBLE PRECISION X8, CURTIM, ENDTIM, INTTIM
      LOGICAL   NEXT
      SAVE KBAS, PRESOU, ENDTIM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'NEWHD.INC'
C-----------------------------------------------------------------------
      J = NRPRIN + NPD
      CALL RCOPY (NRPRIN, RPARM, BUFF3(1))
      CALL RCOPY (NPD, BUFF1, BUFF3(NRPRIN+1))
C                                       First call
      IF (I.EQ.1) THEN
C                                       initialize the buffer of
C                                       accumulated visibilities
         J = NPD * NBASL
         CALL RFILL (J, 0.0, DBUFF)
         J = NPR * NBASL
         CALL RFILL (J, 0.0, RBUFF)
         KBAS = 0
         PRESOU = 0
         IF (MLOCSU.GT.-1) PRESOU = IROUND(BUFF3(1+MLOCSU))
         CURTIM = BUFF3(1+MLOCT) - DTUTC
         IDAY = CURTIM
         X8 = (CURTIM-IDAY) / SOLINT
C                                       current time in integers of
C                                       SOLINT
         INTTIM = IDAY +  DINT (X8) * SOLINT
         ENDTIM = INTTIM + SOLINT
         END IF
C                                       store BIND before it's changed
C                                       after UVDISK('WRIT'..)
      LBIND = 1
      CURTIM = BUFF3(LBIND+MLOCT) - DTUTC
      IF (MLOCB.GE.0) THEN
         CURBAS = BUFF3(LBIND+MLOCB)
      ELSE
         CURA1 = BUFF3(LBIND+MLOCA1) + 0.1
         CURA2 = BUFF3(LBIND+MLOCA2) + 0.1
         CURSA = BUFF3(LBIND+MLOCSA) + 0.1
         END IF
      CURSOU = 0
      IF (MLOCSU.GT.-1) CURSOU = IROUND(BUFF3(LBIND+MLOCSU))
C
      NEXT = (CURTIM.GT.ENDTIM) .OR. (CURSOU.NE.PRESOU)
      NEXT = NEXT .OR. EOF
      IF (NEXT) THEN
C                                       evaluate averaged times
         DO 10 L = 1,KBAS
            WT = RBUFF(NRPRIN+1,L)
            IF (WT.GT.0.0) THEN
               DO 5 IP = 1,NRPRIN
                  IF (ANOTA(IP).EQ.2) RBUFF(IP,L) = RBUFF(IP,L) / WT
 5                CONTINUE
               RBUFF(NRPRIN+2,L) = RBUFF(1+MLOCT,L)
            ELSE
               RBUFF(NRPRIN+2,L) = FBLANK
               END IF
 10         CONTINUE
C                                       sort the average data by time
         DO 80 L = 1,KBAS
            CURTMI = 1.0E10
            DO 15 K = 1,KBAS
               IF (RBUFF(NRPRIN+2,K).NE.FBLANK) THEN
                  IF (RBUFF(1+MLOCT,K).LT.CURTMI) THEN
                     CURTMI = RBUFF(1+MLOCT,K)
                     ICUR = K
                     END IF
                  END IF
 15            CONTINUE
            RBUFF(NRPRIN+2,ICUR) = FBLANK
            K = ICUR
C                                       Random parameters
            CALL RCOPY (NRPRIN, RBUFF(1,K), BUFF2(BIND))
            IF (OUTSOU) THEN
               BUFF2(BIND+LLOCSU) = NUMSOU
               RNXSOR = NUMSOU
            ELSE
               RNXSOR = 1
               END IF
C                                       update NX table
            CALL RNXUPD (BUFF2(BIND), RNXSOR, RNXRET)
C                                       data
            DO 60 LOOPS = 1,NOPOL
               DO 40 LOOPIF = 1,NUMIF
                  DO 20 LOOPF = 1, NOUTCH
                     INP = 1 + (LOOPS-1)*JNCS + (LOOPIF-1)*JNCIF +
     *                  (LOOPF-1)*JNCF
                     XNORM = 1.0
                     SUMWT = DBUFF(INP+2,K)
                     IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
                     BUFF1(INP)   = DBUFF(INP,K) * XNORM
                     BUFF1(INP+1) = DBUFF(INP+1,K) * XNORM
                     BUFF1(INP+2) = SUMWT
 20                  CONTINUE
 40               CONTINUE
 60            CONTINUE

C                                       Write new
            NIO = 1
            XCOUNT = XCOUNT + 1
            IF (OUTCOM) THEN
               CALL ZUVPAK (NCORR, BUFF1, BUFF2(BIND+LLOCWT),
     *            BUFF2(BIND+NRPRIN))
            ELSE
               CALL RCOPY (3*NCORR, BUFF1, BUFF2(BIND+NRPRIN))
               END IF
            CALL UVDISK ('WRIT', LUN, FIND, BUFF2, NIO, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1060) IRET
               GO TO 990
               END IF
 80         CONTINUE
C
C                                       initialize the buffer of
C                                       accumulated visibilities
         J = NPD * NBASL
         CALL RFILL (J, 0.0, DBUFF)
         J = NPR * NBASL
         CALL RFILL (J, 0.0, RBUFF)
         KBAS = 0
C                                       new interval at the same source
         IF (CURSOU.EQ.PRESOU) THEN
 90         IF (CURTIM.GT.ENDTIM) THEN
               ENDTIM = ENDTIM + SOLINT
               GO TO 90
               END IF
         ELSE
            IDAY = CURTIM
            X8 = (CURTIM-IDAY) / SOLINT
C                                       current time in integers of
C                                       SOLINT
            INTTIM = IDAY + DINT (X8) * SOLINT
            ENDTIM = INTTIM + SOLINT
            END IF
         PRESOU = CURSOU
         END IF
C                                       find a baseline number
      IBAS = 0
      IF (MLOCB.GE.0) THEN
         DO 100 K = 1,KBAS
            IF (ABS(CURBAS-RBUFF(1+MLOCB,K)).LT.0.001) IBAS = K
 100        CONTINUE
      ELSE
         DO 105 K = 1,KBAS
            IF ((ABS(CURA1-RBUFF(1+MLOCA1,K)).LT.0.001) .AND.
     *         (ABS(CURA2-RBUFF(1+MLOCA2,K)).LT.0.001) .AND.
     *         (ABS(CURSA-RBUFF(1+MLOCSA,K)).LT.0.001)) IBAS = K
 105        CONTINUE
         END IF
      IF (IBAS.LE.0) THEN
         IF (KBAS.GE.NBASL) THEN
            IF (MLOCB.GE.0) THEN
               IP = CURBAS / 256.0
               ICUR = CURBAS - 256 * IP
               WRITE (MSGTXT,1100) IP, ICUR
            ELSE
               WRITE (MSGTXT,1100) CURA1, CURA2
               END IF
            IRET = 1
            GO TO 990
            END IF
         KBAS = KBAS + 1
         IBAS = KBAS
         END IF
      DO 130 LOOPS = 1,NOPOL
         DO 120 LOOPIF = 1,NUMIF
            DO 110 LOOPF = 1,NOUTCH
               INDEX = 1 + (LOOPS-1)*JNCS + (LOOPIF-1)*JNCIF +
     *            (LOOPF-1)*JNCF
               INP = INDEX + NRPRIN
               WT = BUFF3(INP+2)
               IF (WT.GT.0.0) THEN
                  WTOUT = MAX (WT, WTOUT)
                  DBUFF(INDEX,IBAS) = DBUFF(INDEX,IBAS) + BUFF3(INP)*WT
                  DBUFF(INDEX+1,IBAS) = DBUFF(INDEX+1,IBAS)
     *               + BUFF3(INP+1)*WT
                  DBUFF(INDEX+2,IBAS) = DBUFF(INDEX+2,IBAS) + WT
                  END IF
 110           CONTINUE
 120        CONTINUE
 130     CONTINUE
      DO 200 IP = 1,NRPRIN
C                                       weighted average variables
         IF (ANOTA(IP).EQ.2) THEN
            RBUFF(IP,IBAS) = RBUFF(IP,IBAS) + WTOUT*BUFF3(IP)
C                                       no weight for summed variables
         ELSE IF (ANOTA(IP).EQ.1) THEN
            RBUFF(IP,IBAS) = RBUFF(IP,IBAS) + BUFF3(IP)
         ELSE
            IF (RBUFF(IP,IBAS).EQ.0.0) RBUFF(IP,IBAS) = BUFF3(IP)
            END IF
 200     CONTINUE
      RBUFF(NRPRIN+1,IBAS) = RBUFF(NRPRIN+1,IBAS) + WTOUT
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('AVGTIM: ERROR',I5,' WRITING OUTPUT FILE')
 1100 FORMAT ('AVGTIM: TOO MANY BASELINES FOR BUFFER AT',I2,' - ',I2)
      END
      SUBROUTINE SUSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, IFQID, BUFFER, OBUFF, CNTCHN, REFCOR,
     *   IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in an SU table.
C   Inputs:
C      DISKI           I       Input volume number
C      CNOI            I       Input catalog number
C      DISKO           I       Output volume number
C      CNOO            I       Output catalog number
C      VER             I       Version to check/modify
C      CATIN(256)      I       Input catalog header
C      CATOUT(256)     I       Output catalog header
C      LUNI            I       LUN to use
C      LUNO            I       LUN to use
C      BIF             I       Start IF number
C      EIF             I       End IF number
C      IFQID           I       FQ ID to select
C      CNTCHN          I       Number of averaged frequency channels
C      REFCOR          R       Correction of the reference channel
C   Input/Output:
C      BUFFER          I(*)    Work buffer
C      OBUFF           I(*)    Work buffer
C   Output:
C      IRET            I       Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256),
     *   CATOUT(256), LUNI, LUNO, BIF, EIF, IFQID, BUFFER(*),
     *   OBUFF(*), CNTCHN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   ISURNO, SUKOLS(MAXSUC), SUNUMV(MAXSUC), NUMIF,
     *   OKOLS(MAXSUC), ONUMV(MAXSUC), NSUROW, I, IIF, JIF, OSURNO,
     *   NEWNIF, IDSOU, QUAL, SUFQID, OVER
      LOGICAL   REFMT
      REAL      FLUX(4,MAXIF), REFCOR
      DOUBLE PRECISION    FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH,
     *   RAAPP, DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   RAOBS, DECOBS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open SU file
      CALL SOUINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *  NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IFs
      NEWNIF = MAX (MIN (NUMIF, EIF) - BIF + 1, 0)
      REFMT = NEWNIF.NE.NUMIF
C                                       # rows in old table
      NSUROW = BUFFER(5)
C                                       Open up new SU table
      OVER = VER
      IF (SUFQID.EQ.-999) SUFQID = -1
C                                       Check selected FQ ID
      IF ((SUFQID.EQ.IFQID).AND.(IFQID.GT.0)) SUFQID = 1
      IF ((SUFQID.NE.IFQID).AND.(IFQID.GT.0)) SUFQID = -1
C
      CALL SOUINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *  NEWNIF, VELTYP, VELDEF, SUFQID, OSURNO, OKOLS, ONUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1, NSUROW
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *      PMRA, PMDEC, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
C                                       De-selected record
         IF (NEWNIF.EQ.0) GO TO 100
C                                       Select IFs
         DO 90 JIF = 1, NEWNIF
            IIF = JIF + BIF - 1
            FREQO(JIF) = FREQO(IIF)
            LSRVEL(JIF) = LSRVEL(IIF)
C                                       correct the reference frequency
            LRESTF(JIF) = LRESTF(IIF) + REFCOR * BANDW
            FLUX(1,JIF) = FLUX(1,IIF)
            FLUX(2,JIF) = FLUX(2,IIF)
            FLUX(3,JIF) = FLUX(3,IIF)
            FLUX(4,JIF) = FLUX(4,IIF)
 90         CONTINUE

C                                       change bandwidth in accordance
C                                       of number of averaged frequency
C                                       channels
         IF (CNTCHN.GT.0) BANDW = BANDW * CNTCHN
C                                       Write new one
         CALL TABSOU ('WRIT', OBUFF, OSURNO, OKOLS, ONUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISURNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSURNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted SU', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied SU', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SUSEL: ERROR ',I3,' RETURNED FROM SOUINI')
 1020 FORMAT ('SUSEL: ERROR ',I3,' RETURNED FROM TABSOU')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
      SUBROUTINE PRMSET (CATBLK, TRN)
C-----------------------------------------------------------------------
C   Drops REMOVED from random parameter list and makes list of output
C   indices
C   In/Out:
C      CATBLK   I(256)   Catalog header - input random parms can have
C                        REMOVED  - output does not (and # changed)
C   Output:
C      TRN      I(14)    output parm(j) = input parm(trn(j))
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256), TRN(*)
C
      INTEGER   NPI, NPO, I
      CHARACTER TYPE*8
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      NPO = 0
      NPI = CATBLK(KIPCN)
      DO 20 I = 1,NPI
         CALL H2CHR (8, 1, CATBLK(KHPTP+2*I-2), TYPE)
         IF (TYPE.NE.'REMOVED') THEN
            NPO = NPO + 1
            TRN(NPO) = I
            CALL CHR2H (8, TYPE, 1, CATBLK(KHPTP+2*NPO-2))
            END IF
 20      CONTINUE
      CATBLK(KIPCN) = NPO
C
 999  RETURN
      END
      SUBROUTINE RNXUPD (RPARM, DEFSRC, IRET)
C-----------------------------------------------------------------------
C   Updates tables in core if needed, and if necessary, write a new
C   NX table record.  SPLAT version with added arg DEFSRC
C   Inputs:
C      RPARM    R(*)   Random parameters
C      DEFSRC   I      Source number to use if no source parameter in
C                      data
C   In/output:
C     IRET      I      Error code: input IRET unchanged if failure flag
C                      is set  (RNXRNO < 0)
C-----------------------------------------------------------------------
      INTEGER   DEFSRC, IRET
      REAL      RPARM(*)
C
      INTEGER   IA1, IA2, CURSUB, CURSOU, CURFQI, ISUB
      REAL      CURTIM, TEMP, GAP, LENGTH, CTIME, DTIME, T
      INCLUDE 'INCS:DRNX.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'NEWHD.INC'
C-----------------------------------------------------------------------
C                                       leave if not doing
      IF (RNXRNO.LE.0) GO TO 999
      IRET = 0
C                                       parameters of this record
      IF (MLOCB.GE.0) THEN
         TEMP = RPARM(1+MLOCB)
         IA1 = TEMP / 256.0 + 0.001
         IA2 = TEMP - 256 * IA1 + 0.01
         CURSUB = 100.0 * (TEMP - IA1*256 -IA2) + 1.5
      ELSE
         IA1 = RPARM(1+MLOCA1) + 0.1
         IA2 = RPARM(1+MLOCA2) + 0.1
         CURSUB = RPARM(1+MLOCSA) + 0.1
         END IF
      IF (MLOCSU.GE.0) THEN
         CURSOU = RPARM(1+MLOCSU)
      ELSE
         CURSOU = DEFSRC
         END IF
      IF (MLOCFQ.GE.0) THEN
         CURFQI = RPARM(1+MLOCFQ)
      ELSE
         CURFQI = 1
         END IF
      CURTIM = RPARM(1+MLOCT)
C                                       new vis
      RNXVIS = RNXVIS + 1
C                                       start of scan
      IF (RNXANT(CURSUB).EQ.0) THEN
         VSTART(CURSUB) = RNXVIS
         STIME(CURSUB)  = CURTIM
         SRC(CURSUB)    = CURSOU
         FQID(CURSUB)   = CURFQI
 10      IF (RNXNOS(CURSUB).GT.0) THEN
            IF (CURTIM.GT.RNXTSC(RNXCUR(CURSUB)+1)) THEN
               RNXCUR(CURSUB) = RNXCUR(CURSUB) + 1
               IF (RNXCUR(CURSUB)-RNXFIR(CURSUB)+1.GE.RNXNOS(CURSUB))
     *            RNXNOS(CURSUB) = 0
               GO TO 10
               END IF
            END IF
C                                       Time has gone down!
      ELSE IF (CURTIM.LT.ETIME(CURSUB)) THEN
         GAP    = CURTIM - ETIME(CURSUB)
         STIME(CURSUB) = MIN (STIME(CURSUB), CURTIM)
         IF (GAP.LT.-MAXGAP) THEN
            IRET = 1
            MSGTXT = 'RNXUPD: DATA SERIOUSLY OUT OF TIME ORDER'
            GO TO 990
            END IF
C                                       Time has increased
      ELSE IF (CURTIM.GT.ETIME(CURSUB)) THEN
C                                       use previous info
         IF (RNXNOS(CURSUB).GT.0) THEN
            GAP = 0.0
            LENGTH = 0.0
 15         IF (CURTIM.GT.RNXTSC(RNXCUR(CURSUB)+1)) THEN
               RNXCUR(CURSUB) = RNXCUR(CURSUB) + 1
               LENGTH = 2. * MAXLEN
               IF (RNXCUR(CURSUB)-RNXFIR(CURSUB)+1.GE.RNXNOS(CURSUB))
     *            THEN
                  RNXNOS(CURSUB) = 0
               ELSE
                  GO TO 15
                  END IF
               END IF
C                                       figure it out ourselves
         ELSE
            GAP    = CURTIM - ETIME(CURSUB)
            LENGTH = CURTIM - STIME(CURSUB)
C                                       try to find proper maxgap
            IF ((NGAPS.GE.0) .AND. (GAP.GT.RNXEPS) .AND. (GAP.LE.MAXGAP)
     *         .AND. (CURSOU.EQ.SRC(CURSUB)) .AND.
     *         (CURFQI.EQ.FQID(CURSUB))) THEN
               T = MAX (10.0*SGAPS/NGAPS, 0.0007)
               IF (NGAPS.EQ.0) THEN
                  NGAPS = 1
                  SGAPS = GAP
               ELSE IF ((NGAPS.GT.1) .AND. (GAP.GT.T)) THEN
                  MAXGAP = (GAP + T)/2
                  NGAPS = -1
               ELSE
                  NGAPS = NGAPS + 1
                  SGAPS = SGAPS + GAP
                  END IF
               END IF
            END IF
C                                       close this scan
         IF ((LENGTH.GT.MAXLEN) .OR. (GAP.GT.MAXGAP) .OR.
     *      (CURSOU.NE.SRC(CURSUB)) .OR. (CURFQI.NE.FQID(CURSUB))) THEN
C                                       dump others??
            DO 20 ISUB = 1,MAXSUB
               IF ((RNXANT(ISUB).GT.0) .AND. (ISUB.NE.CURSUB) .AND.
     *            (ETIME(ISUB).LT.STIME(CURSUB))) THEN
                  CTIME = (ETIME(ISUB) + STIME(ISUB)) / 2.0
                  DTIME = ETIME(ISUB) - STIME(ISUB)
                  CALL TABNDX ('WRIT', RNXBUF, RNXRNO, RNXKOL, RNXNUM,
     *               CTIME, DTIME, SRC(ISUB), ISUB, VSTART(ISUB),
     *               VEND(ISUB), FQID(ISUB), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITE NX TABLE'
                     GO TO 990
                     END IF
                  RNXANT(ISUB) = 0
                  END IF
 20            CONTINUE
C                                       now write current
            CTIME = (ETIME(CURSUB) + STIME(CURSUB)) / 2.0
            DTIME = ETIME(CURSUB) - STIME(CURSUB)
            CALL TABNDX ('WRIT', RNXBUF, RNXRNO, RNXKOL, RNXNUM,
     *         CTIME, DTIME, SRC(CURSUB), CURSUB, VSTART(CURSUB),
     *         VEND(CURSUB), FQID(CURSUB), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NX TABLE'
               GO TO 990
               END IF
C                                       Set up new scan:
            RNXANT(CURSUB) = 0
            VSTART(CURSUB) = RNXVIS
            STIME(CURSUB)  = CURTIM
            SRC(CURSUB)    = CURSOU
            FQID(CURSUB)   = CURFQI
            END IF
         END IF
C                                       Update scan information:
      VEND(CURSUB) = RNXVIS
      ETIME(CURSUB) = CURTIM
      RNXANT(CURSUB) = 1
      GO TO 999
C
 990  CALL MSGWRT (8)
      RNXRNO = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RNXUPD: ERROR',I3,' DOING ',A)
      END
      SUBROUTINE TYFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, TB, TE, AN, NA, ISUB,
     *   JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in a TY table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      LUNI     I    p   LUN to use
C      LUNO     I        LUN to use
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      IFQID    I        FQ ID to select (output value is 1)
C                        if <= 0 then output value unchanged.
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      CATOUT   I(256)   Output catalog header
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, AN(8), NA, ISUB, JSUB,
     *   BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ITYRNO, TYKOLS(MAXTYC), TYNUMV(MAXTYC), NUMPOL, NUMIF,
     *   OKOLS(MAXTYC), ONUMV(MAXTYC), NTYROW, I, OVER, SOURID, ANTNO,
     *   SUBA, FREQID, OTYRNO, NEWNIF, IIF, JIF, IPOL, K, NDEL, NTOT,
     *   JRET, LBPOL, NEWPOL
      LOGICAL   REFMT, GOTIT, GOTONE
      REAL      TIME, TIMEI, TSYS(2,MAXIF), TANT(2,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
C                                       Open TY file
      CALL TYINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, ITYRNO,
     *   TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IFs
      NEWNIF = MAX (MIN (NUMIF, EIF) - BIF + 1, 0)
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NTYROW = BUFFER(5)
C                                       Open up new TY table
      OVER = VER
      CALL TYINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OTYRNO, OKOLS, ONUMV, NEWPOL, NEWNIF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NTYROW
         CALL TABTY ('READ', BUFFER, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT,
     *      IRET)
C                                       Error reading table
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       check subarray, time
         ELSE IF (IRET.EQ.0) THEN
            CALL TYFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA,
     *         FREQID, TSYS, TANT, JRET)
            IF (JRET.GT.0) THEN
               IRET = JRET
               GO TO 999
               END IF
            IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *         IRET = -1
            IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
            IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (IFQID.NE.FREQID)) IRET = -1
C                                       antenna
            IF ((NA.GT.0) .AND. (ANTNO.GT.0)) THEN
               GOTIT = .FALSE.
               DO 30 K = 1,NA
                  GOTIT = GOTIT .OR. (ANTNO.EQ.AN(K))
 30               CONTINUE
               IF (.NOT.GOTIT) IRET = -1
               END IF
            IF ((JRET.EQ.-1) .AND. (IRET.EQ.0)) THEN
               IRET = -1
               NDEL = NDEL + 1
               END IF
            END IF
C                                       Is this record selected ?
         IF ((IRET.LT.0) .OR. (NEWNIF.EQ.0)) THEN
            REFMT = .TRUE.
C                                       Select IFs
         ELSE
            GOTONE = .FALSE.
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + BIF - 1
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  TSYS(IPOL,JIF) = TSYS(K,IIF)
                  TANT(IPOL,JIF) = TANT(K,IIF)
                  IF (TSYS(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
                  IF (TANT(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80               CONTINUE
 90            CONTINUE
C                                       Write new one
            IF (GOTONE) THEN
               IF (IFQID.GT.0) FREQID = 1
               IF (JSUB.GE.0) SUBA = JSUB
               NTOT = NTOT + 1
               CALL TABTY ('WRIT', OBUFF, OTYRNO, OKOLS, ONUMV, NEWPOL,
     *            NEWNIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            TSYS, TANT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET
                  GO TO 990
                  END IF
            ELSE
               REFMT = .TRUE.
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OTYRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted TY', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied TY', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         NTOT = NTOT + NDEL
         WRITE (MSGTXT,1101) NDEL, NTOT
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TYFSEL: ERROR ',I3,' RETURNED FROM TYINI')
 1020 FORMAT ('TYFSEL: ERROR ',I3,' RETURNED FROM TABTY')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' TY records applying',
     *   ' flag table')
      END
      SUBROUTINE TYFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, TSYS, TANT, IRET)
C-----------------------------------------------------------------------
C   Flags a TY table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I      Number polarizations in TY data
C      NIF      I      Number of IFs in those data
C      TIME     R      Time of table row
C      SOURID   I      Source number of row
C      ANTNO    I      Antenna number of row
C      SUBA     I      Subarray of row
C      FREQID   I      Frequency ID if row
C   In/Out:
C      TSYS     R(*)   System temperature array - flagged -> FBLANK
C      TANT     R(*)   Antenna temperature array
C   Inputs from include DSEL.INC:
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
C      IRET     I      0 -> okay, -1 -> all flagged
C-----------------------------------------------------------------------
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, IRET
      REAL      TIME, TSYS(2,*), TANT(2,*)
C
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (TMFLST.LT.TIME) CALL NXTFLG (TIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
C                                       loop over current flags
      DO 50 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *      GO TO 50
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.SOURID) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (SOURID.NE.0)) GO TO 50
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.ANTNO)) GO TO 50
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 50
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.FREQID) .AND.
     *      (FREQID.GT.0)) GO TO 50
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NPOL
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  TSYS(JPOLN,JIF) = FBLANK
                  TANT(JPOLN,JIF) = FBLANK
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((TSYS(JPOLN,JIF).NE.FBLANK) .OR.
     *         (TANT(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IRET = -1
C
 999  RETURN
      END
      SUBROUTINE SNFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, TB, TE, ISUB, JSUB,
     *   BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in a SN table, can also modify the FQ ID
C   Applies flagging as well
C   Inputs:
C      DISKI           I       Input volume number
C      CNOI            I       Input catalog number
C      DISKO           I       Output volume number
C      CNOO            I       Output catalog number
C      VER             I       Version to check/modify
C      CATIN(256)      I       Input catalog header
C      CATOUT(256)     I       Output catalog header
C      LUNI            I       LUN to use
C      LUNO            I       LUN to use
C      BIF             I       Start IF number
C      EIF             I       End IF number
C      IFQID           I       FQ ID to select (output value is 1)
C                              if <= 0 then output value unchanged.
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER          I(*)    Work buffer
C      OBUFF           I(*)    Work buffer
C   Output:
C      IRET            I       Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, ISUB, JSUB, BUFFER(*),
     *   OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT, NUMPOL,
     *   NUMIF, NUMNOD, OKOLS(MAXSNC), ONUMV(MAXSNC), NSNROW, I, OVER,
     *   SOURID, ANTNO, SUBA, FREQID, NODENO, REFA(2,MAXIF), OSNRNO,
     *   NEWNIF, IIF, JIF, IPOL, NDEL, NTOT, JRET, LBPOL, NEWPOL, K,
     *   LBIF
      LOGICAL   ISAPPL, REFMT, GOTONE
      REAL      GMMOD, RANOD(25), DECNOD(25), TIMEI, IFR, MBDELY(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), DISP(2), DDISP(2)
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
C                                       Open SN file
      CALL SNINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IFs
      LBIF = MAX (1, BIF)
      NEWNIF = MIN (NUMIF, EIF) - BIF + 1
      IF (NEWNIF.LE.0) THEN
         NEWNIF = NUMIF
         LBIF = 1
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NSNROW = BUFFER(5)
C                                       Open up new SN table
      OVER = VER
      CALL SNINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OSNRNO, OKOLS, ONUMV, NUMANT, NEWPOL, NEWNIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NSNROW
         CALL TABSN ('READ', BUFFER, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
C                                       check subarray and FQ
         IF (IRET.EQ.0) THEN
            CALL SNFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA,
     *         FREQID, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, JRET)
            IF (JRET.GT.0) THEN
               IRET = JRET
               GO TO 999
               END IF
            IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *         IRET = -1
            IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (IFQID.NE.FREQID)) IRET = -1
            IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
            IF ((JRET.EQ.-1) .AND. (IRET.EQ.0)) THEN
               IRET = -1
               NDEL = NDEL + 1
               END IF
            END IF
C                                       Is this record selected ?
         IF ((IRET.LT.0) .OR. (NEWNIF.EQ.0)) THEN
            REFMT = .TRUE.
C                                       Error reading table
         ELSE IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       Select IFs
         ELSE
            GOTONE = .FALSE.
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + LBIF - 1
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  CREAL(IPOL,JIF) = CREAL(K,IIF)
                  CIMAG(IPOL,JIF) = CIMAG(K,IIF)
                  DELAY(IPOL,JIF) = DELAY(K,IIF)
                  RATE(IPOL,JIF) = RATE(K,IIF)
                  WEIGHT(IPOL,JIF) = WEIGHT(K,IIF)
                  REFA(IPOL,JIF) = REFA(K,IIF)
                  IF (CREAL(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80               CONTINUE
 90            CONTINUE
C                                       Write new one
            IF (GOTONE) THEN
               IF (IFQID.GT.0) FREQID = 1
               IF (JSUB.GE.0) SUBA = JSUB
               NTOT = NTOT + 1
               CALL TABSN ('WRIT', OBUFF, OSNRNO, OKOLS, ONUMV, NEWPOL,
     *            TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *            MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, REFA, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET
                  GO TO 990
                  END IF
            ELSE
               REFMT = .TRUE.
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSNRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted SN', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied SN', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         NTOT = NTOT + NDEL
         WRITE (MSGTXT,1101) NDEL, NTOT
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SNSEL: ERROR ',I3,' RETURNED FROM SNINI')
 1020 FORMAT ('SNSEL: ERROR ',I3,' RETURNED FROM TABSN')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' SN records applying',
     *   ' flag table')
      END
      SUBROUTINE SNFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
C-----------------------------------------------------------------------
C   Flags a SN table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I        Number polarizations in TY data
C      NIF      I        Number of IFs in those data
C      TIME     D        Time of table row
C      SOURID   I        Source number of row
C      ANTNO    I        Antenna number of row
C      SUBA     I        Subarray of row
C      FREQID   I        Frequency ID if row
C   In/Out:
C      CREAL    R(2,*)   Real part of solution
C      CIMAG    R(2,*)   Imaginary part of solution
C      DELAY    R(2,*)   Delay
C      RATE     R(2,*)   Rate
C      WEIGHT   R(2,*)   Solution weight
C      REFA     I(2,*)   Reference antenna
C   Inputs from include DSEL.INC:
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
C      IRET     I      0 -> okay, -1 -> all flagged
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, REFA(2,*), IRET
      REAL      CREAL(2,*), CIMAG(2,*), DELAY(2,*), RATE(2,*),
     *   WEIGHT(2,*)
C
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      REAL      RTIME
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      RTIME = TIME
      IF (TMFLST.LT.TIME) CALL NXTFLG (RTIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
C                                       loop over current flags
      DO 50 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *      GO TO 50
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.SOURID) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (SOURID.NE.0)) GO TO 50
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.ANTNO)) GO TO 50
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 50
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.FREQID) .AND.
     *      (FREQID.GT.0)) GO TO 50
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NPOL
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  CREAL(JPOLN,JIF) = FBLANK
                  CIMAG(JPOLN,JIF) = FBLANK
                  DELAY(JPOLN,JIF) = FBLANK
                  RATE(JPOLN,JIF) = FBLANK
                  WEIGHT(JPOLN,JIF) = 0.0
                  REFA(JPOLN,JIF) = 0
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all gone
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((CREAL(JPOLN,JIF).NE.FBLANK) .AND.
     *         (CIMAG(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IRET = -1
C
 999  RETURN
      END
      SUBROUTINE SYFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, TB, TE, NSOU, SOUIND,
     *   AN, NA, ISUB, JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in a SY table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      CATOUT   I(256)   Output catalog header
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C      BPOL     I        First polarization to copy
C      EPOL     I        Last polarization to copy
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      IFQID    I        FQ ID to select (set to 1 on output)
C                           if <= 0 then output value unchanged.
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      NSOU     I        Number of selected sources
C      SOUIND   I(*)     Array of sources indexes selected
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, AN(*), NA, SOUIND(*),
     *   NSOU, ISUB, JSUB, BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSYC
      PARAMETER (MAXSYC=12)
C
      INTEGER   ISYRNO, SYKOLS(MAXSYC), SYNUMV(MAXSYC), NUMANT, NUMPOL,
     *   NUMIF, OKOLS(MAXSYC), ONUMV(MAXSYC), NSYROW, I, SOURID, ANTNO,
     *   SUBA, FREQID, OSYRNO, NEWNIF, IIF, JIF, IPOL, K, OVER, LBIF,
     *   NEWPOL, LBPOL, NDEL, NTOT, JRET, NPART
      REAL      PDIFF(2,MAXIF), PSUM(2,MAXIF), PGAIN(2,MAXIF), TIMEI
      DOUBLE PRECISION TIME
      LOGICAL   GOTIT, REFMT, GOTONE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
      NPART = 0
C                                       Open SY file
      CALL SYINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, ISYRNO,
     *   SYKOLS, SYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IF's
      LBIF = MAX (1, BIF)
      NEWNIF = MIN (NUMIF, EIF) - LBIF + 1
      IF (NEWNIF.LE.0) THEN
         LBIF = 1
         NEWNIF = NUMIF
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NSYROW = BUFFER(5)
C                                       Open up new SY table
      OVER = VER
      CALL SYINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OSYRNO, OKOLS, ONUMV, NUMANT, NEWPOL, NEWNIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NSYROW
         CALL TABSY ('READ', BUFFER, ISYRNO, SYKOLS, SYNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, PDIFF,
     *      PSUM, PGAIN, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       flag info
         CALL SYFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA, FREQID,
     *      PDIFF, PSUM, PGAIN, NPART, JRET)
         IF (JRET.GT.0) THEN
            IRET = JRET
            GO TO 999
            END IF
C                                       Time selection
         IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
C                                       Sources selection
         IF ((NSOU.GT.0) .AND. (SOURID.GT.0)) THEN
            GOTIT = .FALSE.
            DO 20 K = 1,NSOU
               GOTIT = GOTIT .OR. (SOURID.EQ.SOUIND(K))
 20            CONTINUE
            IF (.NOT.GOTIT) IRET = -1
            END IF
C                                       Antennas selection
         IF ((NA.GT.0) .AND. (ANTNO.GT.0)) THEN
            GOTIT = .FALSE.
            DO 30 K = 1,NA
               GOTIT = GOTIT .OR. (ANTNO.EQ.AN(K))
 30            CONTINUE
            IF (.NOT.GOTIT) IRET = -1
            END IF
C                                       FQ selection
         IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND. (IFQID.NE.FREQID))
     *      IRET = -1
C                                       Suba selection
         IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *      IRET = -1
         IF ((JRET.EQ.-1) .AND. (IRET.EQ.0)) THEN
            IRET = -1
            NDEL = NDEL + 1
            END IF
C                                       Is this record selected ?
         IF (IRET.LT.0) THEN
            REFMT = .TRUE.
C                                       Re-number IF's
         ELSE
            GOTONE = .FALSE.
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + BIF - 1
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  PDIFF(IPOL,JIF) = PDIFF(K,IIF)
                  PSUM(IPOL,JIF) = PSUM(K,IIF)
                  PGAIN(IPOL,JIF) = PGAIN(K,IIF)
                  IF (PDIFF(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
                  IF (PSUM(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80               CONTINUE
 90            CONTINUE
            IF (GOTONE) THEN
               IF (IFQID.GT.0) FREQID = 1
               IF (JSUB.GE.0) SUBA = JSUB
               NTOT = NTOT + 1
               CALL TABSY ('WRIT', OBUFF, OSYRNO, OKOLS, ONUMV, NEWPOL,
     *            NEWNIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            PDIFF, PSUM, PGAIN, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1040) IRET
                  GO TO 990
                  END IF
            ELSE
               REFMT = .TRUE.
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISYRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSYRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted SY', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied SY', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         NTOT = NTOT + NDEL
         WRITE (MSGTXT,1101) NDEL, NTOT
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         WRITE (MSGTXT,1102) NPART
         CALL REFRMT (MSGTXT, '_', I)
         IF (NPART.GT.0) CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SYFSEL: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('SYFSEL: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('SYFSEL: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('SYFSEL: ERROR ',I3,' WRITING NEW TABLE')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' SY records applying',
     *   ' flag table')
 1102 FORMAT ('__Partly flagged',I10,' SY records applying flag table')
      END
      SUBROUTINE SYFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, PDIFF, PSUM, PGAIN, NPART, IRET)
C-----------------------------------------------------------------------
C   Flags a TY table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I      Number polarizations in TY data
C      NIF      I      Number of IFs in those data
C      TIME     R      Time of table row
C      SOURID   I      Source number of row
C      ANTNO    I      Antenna number of row
C      SUBA     I      Subarray of row
C      FREQID   I      Frequency ID if row
C   In/Out:
C      PDIFF    R(*)   Pon-Poff
C      PSUM     R(*)   Pon+Poff
C      PGAIN    R(*)   Post detection gains
C      NPART    I      count of partly flagged records
C   Inputs from include DSEL.INC:
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
C      IRET     I      0 -> okay, -1 -> all flagged
C-----------------------------------------------------------------------
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, NPART, IRET
      REAL      PDIFF(2,*), PSUM(2,*), PGAIN(2,*)
      DOUBLE PRECISION TIME
C
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      REAL      RTIME
      LOGICAL   PART
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      RTIME = TIME
      IF (TMFLST.LT.TIME) CALL NXTFLG (RTIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
      PART = .FALSE.
C                                       loop over current flags
      DO 50 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *      GO TO 50
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.SOURID) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (SOURID.NE.0)) GO TO 50
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.ANTNO)) GO TO 50
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 50
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.FREQID) .AND.
     *      (FREQID.GT.0)) GO TO 50
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NPOL
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
               PART = .TRUE.
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  PDIFF(JPOLN,JIF) = FBLANK
                  PSUM(JPOLN,JIF) = FBLANK
                  PGAIN(JPOLN,JIF) = FBLANK
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all flagged
      IF (PART) NPART = NPART + 1
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((PDIFF(JPOLN,JIF).NE.FBLANK) .OR.
     *         (PSUM(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IF (PART) NPART = NPART - 1
      IRET = -1
C
 999  RETURN
      END
