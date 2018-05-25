LOCAL INCLUDE 'SPLIT.INC'
C                                       Local include for SPLIT
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SEQIN, CNOIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ,
     *   LRECC, LLOCWT, UQUAL, PRMTRN(14), NPRM, CHNSEL(3,20,MAXIF),
     *   CNTCHN(MAXIF), ORIGCH
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XXSTOK,
     *   XCLAOU(2)
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XCALCO*4, XSTOK*4,
     *   CLAOUT*6, HISCRD(10)*64, UCALC*4
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XANTS(50), XBAND, XFREQ,
     *   XFQID, XBIF, XEIF, XBCHAN, XECHAN, XSUBA, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3),
     *   XSOUT, XDISO, XDOUV, APARM(10), XNPTS, XCHINC, XCHNS(4,20),
     *   XBADD(10), BUFF1(UVBFSS), BUFF2(UVBFSS), AVGCH(MAXIF)
      LOGICAL   DOUVCM
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XXCALC, XTIME, XXSTOK, XANTS, XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XBCHAN, XECHAN, XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH, XCLAOU, XSOUT, XDISO, XDOUV,
     *   APARM, XNPTS, XCHINC, XCHNS, XBADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XCALCO, XSTOK, CLAOUT,
     *   HISCRD, UCALC
      COMMON /MISC/ ORIGCH, SEQIN, SEQOUT, DISKIN, DISKO, CNOIN, NUMHIS,
     *   LRECC, LLOCWT, DOUVCM, UQUAL, PRMTRN, NPRM, CHNSEL, AVGCH,
     *   CNTCHN
LOCAL END
      PROGRAM SPLIT
C-----------------------------------------------------------------------
C! Applies calibration and/or editing and/or splits multisource uv data.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2003-2016
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
C   Splits a multisource uv data set into single source data
C   files.  Multiple sources may be processed in a single run.
C   Calibration and editing may be optionally applied.  Fully
C   flagged data will not be copied.
C      Should also handle applying an SN table to a single source file.
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
C      DOUVCMP....If > 1, compress output data.
C      APARM......Control information:
C                    1 = 1 => avg. freq. in IF
C                      = 2 => avg IFs also
C                    2 = Integration time (sec)
C                    3 > 0 => drop subarray info
C                    4 > 0 => calibrate weights
C                    5 = 0 => pass only xc data
C                      = 1 => pass xc/ac
C                      = 2 => pass only ac
C                    6 > 0 => add full sourcename to header
C      CHNSEL.....Channel selection
C      BADDISK....Disks to avoid for scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  NUMSOU, IRET
      LOGICAL   DOWANT
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'SPLIT.INC'
      INTEGER   SULIST(XSTBSZ)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'SPLIT '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL SPLTIN (PRGM, DOWANT, NUMSOU, SULIST, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop over sources.
      CALL SPLTUV (DOWANT, NUMSOU, SULIST, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SPLTIN (PRGN, DOWANT, NUMSOU, SULIST, IRET)
C-----------------------------------------------------------------------
C   SPLTIN gets input parameters for SPLIT, finds input file and
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
C   See prologue comments in SPLIT for more details.
C-----------------------------------------------------------------------
      HOLLERITH CATH(256)
      CHARACTER STAT*4, PRGN*6, UTYPE*2, TELTYP*8
      INTEGER   NUMSOU, SULIST(*), IRET
      LOGICAL   DOWANT, HADSRC
      INTEGER   NPARM, IROUND, IERR, I, LUN
      REAL      CATR(256)
      LOGICAL   T, MATCH
      INCLUDE 'SPLIT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATBLK, CATH)
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 314
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
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
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
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
 40   CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IERR
         GO TO 990
 50   NCFILE = NCFILE + 1
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
C                                       Save the original number of
C                                       channels for later use.
      ORIGCH = CATBLK(KINAX+JLOCF)
C                                       BADDISK
      DO 60 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 60      CONTINUE
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      CALL CCOPY (30, XSOUR, SOURCS)
      CALL RCOPY (8, XTIME, TIMRNG)
      STOKES = XSTOK
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
      DOCAL = XDOCAL.GT.0.0
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      DOUVCM = XDOUV.GT.0.0
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
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

C                                       Kludge for SPLIT logic error
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
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 4000 FORMAT ('You are using a non-standard program')
      END
      SUBROUTINE SPLTUV (DOWANT, NUMSOU, SULIST, IRET)
C-----------------------------------------------------------------------
C   SPLTUV uses UVGET and SPLCOP to copy data into single source files.
C   The history and relevant tables are also copied.
C   Input:
C     DOWANT       L    If true sources listed are selected
C     NUMSOU       I    Number of sources to process, 0=>all
C     SULIST(*)    I    Source number list.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER NAME*12, VELTYP*8, VELDEF*8, CHVEL(4)*4
      CHARACTER TEST*16, CHTM2*2
      HOLLERITH CATH(256), CATUH(256), SNAME1(2),SNAME2(2)
      LOGICAL   DOWANT, T, F, DOAVG, DOAPPT, NOSUB, SMILE, SINGLE,
     *   TABLE, EXIST, FITASC, ONZE, DOIF, DOFSM, LOCAL, LOBL, NOTSRT,
     *   UVOPEN
      INTEGER   NUMVIS, SOUCUR, MAXSOU, SLOOP, TOTREC(2,3), NUMSOU,
     *   SULIST(*), IRET, INDEX, LRECU, NUMFRQ, IERR, NUMCH, FINISH,
     *   SUKOLS(MAXSUC), SUNUMV(MAXSUC), SBUFF(768), I, SLUN, IDSOU,
     *   INOGRP, TMPVER, SUB, LIMS1, LIMS2, SUBTMP, CATSAV(256), OUTDSK,
     *   OUTCNO, ITEMP, SAVBND, IROUND, NCRPM, J, SUMCHN, SUFQID, JTRIM,
     *   DPOSAV, NCHAVG, CHINC, K, K1, K2, NC, LUN, TBIF, TEIF, DUM,
     *   ERRCNT, INHIS
      REAL      RPARM, VIS, CATR(256), OLDRP, TMAXX, CHWT
      DOUBLE PRECISION  BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, CATD(128), OLDFRQ, RAOBS, DECOBS
      INCLUDE 'SPLIT.INC'
      INCLUDE 'INCS:DSEL.INC'
      DOUBLE PRECISION   LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF),
     *   FOFF(MAXIF), FSUM, BWSUM
      INTEGER   NW(MAXIF), ISBAND(MAXIF), IIVER, CSUM, NNIF, RNXRET,
     *   NSLIST, NSNAME(XSTBSZ), ONLIST
      CHARACTER SNMS(XSTBSZ)*12, TMPNAM*12, BNDCOD(MAXIF)*8
      REAL      FLUX(4,MAXIF), FINC(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD),   (CATUV, CATUH)
      SAVE ONZE
      DATA CHVEL /'LSR ','BARY','HELI','RADI'/
      DATA T, F /.TRUE.,.FALSE./, ONZE/.FALSE./
      DATA LUN /18/
C-----------------------------------------------------------------------
      ERRCNT = 0
      SMILE = F
      UVOPEN = F
      TMPVER = CLVER
      DOAPPT = DOAPPL
      TBIF = BIF
      TEIF = EIF
      DOFSM = (APARM(1).GT.0.0) .AND. (APARM(1).LE.1.5)
      I = ECHAN - BCHAN + 1
      IF (DOFSM) THEN
         NCHAVG = IROUND (XNPTS)
         NCHAVG = MIN (NCHAVG, I)
         CHINC = IROUND (XCHINC)
         CHINC = MIN (I, CHINC)
         IF (CHINC.LE.0) CHINC = NCHAVG
         IF (NCHAVG.LE.0) THEN
            NCHAVG = ECHAN - BCHAN + 1
            CHINC = NCHAVG
            END IF
         CALL FILL (MAXIF, NCHAVG, CNTCHN)
         CHWT = BCHAN + (NCHAVG-1.0) / 2.0
         CALL RFILL (MAXIF, CHWT, AVGCH)
      ELSE
         NCHAVG = 1
         CHINC = 1
         END IF
      NUMCH = ((ECHAN - BCHAN) / CHINC) + 1
      IF (NUMCH.EQ.1) CHINC = NCHAVG
      XNPTS = NCHAVG
      XCHINC = CHINC
      DOAVG = APARM(1).GT.1.5
      DOIF  = (APARM(1).GT.2.5) .AND. (JLOCIF.GE.0)
      IF (DOFSM) THEN
         MSGTXT = 'Averaging adjacent channels in frequency'
         CALL MSGWRT (3)
         END IF
      IF (DOAVG) THEN
         MSGTXT = 'Averaging all selected channels in frequency'
         CALL MSGWRT (3)
         END IF
      IF (DOIF) THEN
         MSGTXT = 'Also averaging all selected IFs'
         CALL MSGWRT (3)
         END IF
      NOSUB = APARM(3).GT.0.0
      OLDRP = CATR(KRCRP+JLOCF)
C                                       For all channels selections
      IF (DOAVG) THEN
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
      IF (LIMS2.GT.LIMS1) THEN
         WRITE (MSGTXT,1030) LIMS1, LIMS2
      ELSE
         WRITE (MSGTXT,1031) LIMS1
         END IF
      CALL MSGWRT (2)
      SUBTMP = SUBARR
      DPOSAV = DOPOL
      SAVBND = DOBAND
      LOCAL = DOCAL
      LOBL = DOBL
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
C                                       Open source table
      IF (.NOT.SINGLE) THEN
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
                  IF ((IDSOU.EQ.SULIST(I)) .AND. DOWANT) GO TO 60
                  IF ((IDSOU.EQ.SULIST(I)) .AND. (.NOT.DOWANT))
     *               GO TO 90
                  IF (IDSOU.EQ.SULIST(I)) GO TO 90
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
               IF (SOURCS(1)(:12).EQ.SNMS(I)) THEN
                  NSNAME(I) = NSNAME(I)+1
                  GO TO 90
                  END IF
 70            CONTINUE
            NSLIST = NSLIST + 1
            NSNAME(NSLIST) = 1
            SNMS(NSLIST) = SOURCS(1)(:12)
 90         CONTINUE
C                                       check for naming conflicts
C                                       Setup for single source case:
      ELSE
         NSLIST = 0
         IRET = 0
         MAXSOU = 1
         NUMSOU = 0
         CALL CFILL (30, ' ', SOURCS)
         END IF
C                                       Loop here over sources
      INHIS = NUMHIS
      DO 500 SLOOP = 1,MAXSOU
         SOUCUR = SLOOP
         NUMHIS = INHIS
C                                       Read source table
         IF (.NOT.SINGLE) THEN
            CALL TABSOU ('READ', SBUFF, SOUCUR, SUKOLS, SUNUMV, IDSOU,
     *         SOURCS, SELQUA, SELCOD, FLUX, FREQO, BANDW, RAEPO,
     *         DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL,
     *         RESTFQ, PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       See if this source wanted.
            IF (NUMSOU.GT.0) THEN
               DO 120 I = 1,NUMSOU
                  IF ((IDSOU.EQ.SULIST(I)) .AND. DOWANT) GO TO 130
                  IF ((IDSOU.EQ.SULIST(I)) .AND. (.NOT.DOWANT))
     *               GO TO 500
                  IF (IDSOU.EQ.SULIST(I)) GO TO 500
 120             CONTINUE
               IF (DOWANT) GO TO 500
               END IF
 130        IF ((UQUAL.GE.0) .AND. (UQUAL.NE.SELQUA)) GO TO 500
            IF (UCALC.NE.' ') THEN
               IF (UCALC.EQ.'*') THEN
                  IF (SELCOD.EQ.' ') GO TO 500
               ELSE IF (UCALC.EQ.'-CAL') THEN
                  IF (SELCOD.NE.' ') GO TO 500
               ELSE
                  IF (SELCOD.NE.UCALC) GO TO 500
                  END IF
               END IF
            IF (SELCOD.EQ.' ') SELCOD = '-CAL'
            END IF
         ONLIST = 1
         DO 135 I = 1,NSLIST
            IF (SOURCS(1)(:12).EQ.SNMS(I)) ONLIST = NSNAME(I)
 135        CONTINUE
C                                       Create header, fiddle tables etc
         SUBARR = SUBTMP
         DOPOL = -1
         DOBAND = 0
         DOCAL = .FALSE.
         DOBL = .FALSE.
         CALL UVGET ('INIT', RPARM, VIS, IERR)
         UVOPEN = T
C                                       If a UV GET ERROR
         IF (IERR.NE.0) THEN
            IF (IERR.EQ.2) THEN
               MSGTXT = 'UVGET ERROR, CHECK THAT REQUESTED GAIN '//
     *            'TABLE EXISTS'
               CALL MSGWRT (7)
               END IF
C                                       Jump to try next source
            GO TO 450
            END IF
C                                       Close again - only need header
C                                       for sum of subarrays.
         CALL UVGET ('CLOS', RPARM, VIS, IERR)
         IF (IERR.NE.0) GO TO 450
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
C                                       Name after source.
         IF (.NOT.SINGLE) THEN
            TMPNAM = SOURCS(1)(:12)
            IF (ONLIST.GT.1) THEN
               I = JTRIM (SELCOD)
               WRITE (TMPNAM(9:),1130) SELQUA
               IF (I.GT.0) TMPNAM(9:8+I) = SELCOD(:I)
               END IF
            CALL CHR2H (12, TMPNAM, KHIMNO, CATH(KHIMN))
         ELSE
            CALL CHR2H (12, NAME(1:12), KHIMNO, CATH(KHIMN))
            END IF
         CATBLK(KIIMS) = SEQOUT
C                                       Velocity etc. info IF=BIF
C                                       Do it only for multiple source
C                                       file; otherwise keep the
C                                       original values in header
         IF (.NOT.SINGLE) THEN
            CATD(KDRST) = RESTFQ(BIF)
            CATD(KDARV) = LSRVEL(BIF)
C                                       Alt frequency axis ref pixel.
C                                       CRP already corr for bchan
            CATR(KRARP) = (CATR(KRCRP+JLOCF) - 1.0 - (NCHAVG-1.0)/2.0)
     *         / CHINC + 1.0
C                                       Velocity reference frame
            ITEMP = 3
            IF (VELTYP(1:4).EQ.CHVEL(1)) ITEMP = 1
            IF (VELTYP(1:4).EQ.CHVEL(2)) ITEMP = 2
            IF (VELTYP(1:4).EQ.CHVEL(3)) ITEMP = 2
            IF (VELDEF(1:4).EQ.CHVEL(4)) ITEMP = ITEMP + 256
            CATBLK(KIALT) = ITEMP
C                                       Alt frequency axis ref pixel.
C                                       UVGET corrects for BCHAN
         ELSE
            CATR(KRARP) = (CATR(KRARP) - 1.0 - (NCHAVG-1.0)/2.0) /
     *         CHINC + 1.0
            END IF
C                                       Freq smoothing
C                                       CRP already corr for bchan
         IF (DOFSM) THEN
            CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF) * CHINC
            CATR(KRCRP+JLOCF) = (CATR(KRCRP+JLOCF) - 1.0 -
     *         ((NCHAVG-1.0)/2.0)) / CHINC + 1.0
            CATBLK(KINAX+JLOCF) = NUMCH
            END IF
C                                       Update CATBLK if Averaging with
C                                       use output freq pixel as ref.
C                                       Averaging everything
         IF (DOIF) THEN
            IIVER = 1
            CALL CHNDAT ('READ', BUFF1, IUDISK, IUCNO, IIVER, CATUV,
     *         LUN, NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
            IF (IRET.NE.0) GO TO 999
            FSUM = 0.0D0
            BWSUM = 0.0D0
            CSUM = 0
            DO 160 I = TBIF,TEIF
               FSUM = FSUM + (FOFF(I) + (AVGCH(I) - OLDRP) * FINC(I)) *
     *            CNTCHN(I)
               BWSUM = BWSUM + CNTCHN(I) * FINC(I)
               CSUM = CSUM + CNTCHN(I)
 160           CONTINUE
            FSUM = FSUM / CSUM
            CATD(KDCRV+JLOCF) = OLDFRQ + FSUM
            CATR(KRCRP+JLOCF) = 1.0
            CATR(KRCIC+JLOCF) = BWSUM
            CATBLK(KINAX+JLOCF) = 1
            CATBLK(KINAX+JLOCIF) = 1
C                                       No velocity info if averaging.
            CATD(KDRST) = 0.0
            CATD(KDARV) = 0.0
            CATR(KRARP) = 0.0
C                                       averaging spectral channels
         ELSE IF (DOAVG) THEN
            CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + (AVGCH(1) - OLDRP)
     *         * CATR(KRCIC+JLOCF)
            CATR(KRCRP+JLOCF) = 1.0
C                                       No velocity info if averaging.
            CATD(KDRST) = 0.0
            CATD(KDARV) = 0.0
            CATR(KRARP) = 0.0
C                                       If averaging set CATBLK
            CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF) * CNTCHN(1)
            CATBLK(KINAX+JLOCF) = 1
            END IF
C                                       Make sure there is data
         IF (CATBLK(KIGCN).LE.0) GO TO 450
C                                       remove REMOVEDs
         CALL PRMSET (CATBLK, PRMTRN)
         NPRM = CATBLK(KIPCN)
C                                       Create output file.
         IF (DOUVCM) THEN
            LLOCWT = NPRM
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
                  WRITE (MSGTXT,1100) IERR
                  CALL MSGWRT (6)
                  END IF
               GO TO 460
            ELSE
               IF (IERR.EQ.2) THEN
                  MSGTXT = 'CANNOT OVERWRITE OLD FILE'
                  CALL MSGWRT (6)
                  END IF
               GO TO 450
               END IF
            END IF
C                                       actual output file
         CALL UVPGET (IERR)
         LRECU = LREC
         NCRPM = NRPARM
C                                       Report gain table version, once
         IF ((DOCAL) .AND. (.NOT.ONZE)) THEN
            IF (SINGLE) THEN
               IF (CLVER.LE.0) CALL FNDEXT ('SN', CATUV, CLVER)
               WRITE (MSGTXT,1000) 'SN', CLVER
            ELSE
               IF (CLVER.LE.0) CALL FNDEXT ('CL', CATUV, CLVER)
               WRITE (MSGTXT,1000) 'CL', CLVER
               END IF
            CALL MSGWRT (3)
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
C                                       init NX table on output
         CALL RNXGET (DISKIN, CNOIN, CATUV)
         CALL RNXINI (DISKO, CCNO, CATBLK, RNXRET)
C                                       Loop over subarrays.
         NUMVIS = 0
         NUMFRQ = ECHAN - BCHAN + 1
C                                       Save CATBLK - UVGET will modify
         TMAXX = -1.E10
         NOTSRT = .FALSE.
         DO 300 SUB = LIMS1,LIMS2
            SUBARR = SUB
            CALL COPY (256, CATBLK, CATSAV)
C                                       Initialize reading data
            CALL UVGET ('INIT', RPARM, VIS, IERR)
            IF (IERR.GT.0) GO TO 440
C                                       handle missing subarray
            IF ((IERR.LT.0) .AND. (SUB.LT.LIMS2)) THEN
               MSGSUP = 32000
               CALL UVGET ('CLOS', RPARM, VIS, IERR)
               MSGSUP = 0
               CALL COPY (256, CATSAV, CATBLK)
               GO TO 300
               END IF
            UVOPEN = T
C                                       Restore CATBLK
            CALL COPY (256, CATSAV, CATBLK)
C                                       Copy data
            FINISH = 0
            IF (SUB.EQ.LIMS2) FINISH = 1
            IF (IERR.LT.0) FINISH = 2
            CALL SPLCOP (NUMVIS, TOTREC, FINISH, DOAVG, DOIF, DISKO,
     *         CCNO, NOSUB, BUFF1, JBUFSZ, FREQO, OLDFRQ, NUMHIS,
     *         HISCRD, CHNSEL, DOUVCM, LLOCWT, BUFF2, LRECU, NCRPM,
     *         AVGCH, CNTCHN, OLDRP, CHINC, NCHAVG, NPRM, PRMTRN, TMAXX,
     *         NOTSRT, RNXRET, IERR)
            IF (IERR.NE.0) GO TO 440
            SMILE = .TRUE.
            UVOPEN = F
C                                       Update sort order if necessary
            IF (NOTSRT) CALL CHR2H (2, '**', 1, CATBLK(KITYP))
            NUMVIS = NUMVIS + NVIS
 300        CONTINUE
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
C                                       close NX table
         CALL RNXCLS (RNXRET)
         IF (RNXRET.NE.0) THEN
            MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
            CALL MSGWRT (6)
         ELSE
            MSGTXT = 'New NX table written'
            CALL MSGWRT (2)
            END IF
C                                       History
         CLVER = TMPVER
         IF (.NOT.DOCAL) CLVER = 0
         DOAPPL = DOAPPT
         SUBARR = SUBTMP
         CALL SPLTHI (OUTDSK, OUTCNO, SINGLE, IDSOU)
         CLVER = CLUSE
         DOAPPL = F
         NCFILE = NCFILE - 2
         GO TO 500
C                                       error after UVCREA
 440     DUM = 0
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
 450     IF ((CATBLK(KIGCN).LE.0) .OR. (IERR.EQ.-1)) THEN
            MSGTXT = '** NO VISIBILITIES FOUND FOR ''' // SOURCS(1) //
     *         ''''
         ELSE
            MSGTXT = '** PROBLEM WITH SOURCE ''' // SOURCS(1) //
     *         ''''
            END IF
         CALL MSGWRT (6)
         WRITE (MSGTXT,1450) SELCOD, SELQUA
         CALL MSGWRT (6)
         ERRCNT = ERRCNT + 1
 460     IF (UVOPEN) THEN
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
 500     CONTINUE
C                                       Make sure bad files
C                                       destroyed
      IRET = 1
      IF (SMILE) IRET = 0
      IF (IRET.NE.0) THEN
         MSGTXT = 'FAILED TO SELECT VALID DATA'
         CALL MSGWRT (9)
         END IF
C                                       Close source table
      IF (.NOT.SINGLE)
     *   CALL TABIO ('CLOS', 1, SOUCUR, SBUFF, SBUFF, IERR)
      NCFILE = NCFILE - 1
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Applying ',A,' Table version ',I5)
 1030 FORMAT ('Looping over subarrays',I2,' through',I3)
 1031 FORMAT ('Doing subarray',I3)
 1100 FORMAT ('ERROR',I5,' CREATING OUTPUT UV FILE')
 1130 FORMAT (I4.4)
 1450 FORMAT ('**     CALCODE ''',A,'''  QUALIFIER',I5)
      END
      SUBROUTINE SPLTHI (DISK, CNO, SINGLE, IDSOU)
C-----------------------------------------------------------------------
C   SPLTHI copies and updates history file.  It also copies any tables
C   extension files.
C   Input:
C      DISK     I   Output file disk number
C      CNO      I   Output file catalog slot number.
C      SINGLE   L   F => make an FO table from CL table
C      IDSOU    I   Source number being done at present
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, IDSOU
      LOGICAL   SINGLE
C
      INTEGER   NONOT
      PARAMETER (NONOT = 24)
      CHARACTER NOTTYP(NONOT)*2, NAMOUT*12, HILINE*72, LABEL*8, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   IERR, I, LUN1, LUN2, J, SUBA, K, NSUB, NCHAVG, CHINC,
     *   IROUND, I1, I2
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128), FQOFF
      LOGICAL   T, DOFSM, DOAVG, DOIF, TABLE, EXIST, FITASC
      INCLUDE 'SPLIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA LUN1, LUN2 /28,29/
      DATA T /.TRUE./
      DATA NOTTYP /'CL', 'FG', 'NX', 'SU', 'SN', 'CC', 'CH', 'BP', 'FQ',
     *   'TY', 'IM', 'AT', 'CT', 'MC', 'VT', 'PC', 'HF', 'GC', 'CQ',
     *   'BL', 'AN', 'PD', 'CP', 'SY'/
C-----------------------------------------------------------------------
      DOFSM = (APARM(1).GT.0.0) .AND. (APARM(1).LE.1.5)
      DOAVG = APARM(1).GT.1.5
      DOIF  = APARM(1).GT.2.5
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
      WRITE (HILINE,2010) TSKNAM, XSTOK
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IF range
      WRITE (HILINE,2001) TSKNAM, BIF, EIF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Chan range
      WRITE (HILINE,2002) TSKNAM, BCHAN, ECHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Subarray
      WRITE (HILINE,2003) TSKNAM, SUBARR
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Flagging
      IF (FGVER.GT.0) THEN
         WRITE (HILINE,2004) TSKNAM, FGVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                        Spectral smoothing
      IF (SMOOTH(1).GT.0.5) THEN
         I1 = SMOOTH(1) + 0.5
         I2 = SMOOTH(3) + 0.5
         WRITE (HILINE,2013) TSKNAM, I1, SMOOTH(2), I2
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Channel selection
      IF (DOAVG) THEN
         IF (DOIF) THEN
            HILINE = TSKNAM // '/ average IFs and spectral channels'
         ELSE
            HILINE = TSKNAM // '/ average following spectral channels'
            END IF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         DO 65 K = BIF,EIF
            DO 60 I = 1,20
               IF ((CHNSEL(1,I,K).GT.0) .AND.
     *            (CHNSEL(2,I,K).GE.CHNSEL(1,I,K))) THEN
                  WRITE (HILINE,2014) TSKNAM, (CHNSEL(J,I,K), J = 1,3),
     *               K
                  CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                  IF (IERR.NE.0) GO TO 200
                  END IF
 60            CONTINUE
 65         CONTINUE
         END IF
      IF (DOFSM) THEN
         NCHAVG = MAX (1, IROUND (XNPTS))
         CHINC = MAX (1, IROUND (XCHINC))
         WRITE (HILINE,2011) TSKNAM, NCHAVG, CHINC
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Compressed data
      IF (DOUVCM) THEN
         WRITE (HILINE,2015) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Calibration
C                                       Table
      IF (DOCAL) THEN
         WRITE (HILINE,2005) TSKNAM, CLUSE
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Integration time
      IF (APARM(2).GT.0.0) THEN
         WRITE (HILINE,2012) TSKNAM, APARM(2)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Polzn correction
      IF (DOPOL.GT.0) THEN
         WRITE (HILINE,2016) TSKNAM, DOPOL
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       BL table
      IF (XBLVER.GE.0.0) THEN
         WRITE (HILINE,2017) TSKNAM, BLVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       BP table
      IF (XDOBND.GT.0.0) THEN
         WRITE (HILINE,2018) TSKNAM, DOBAND
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         WRITE (HILINE,2019) TSKNAM, BPVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Calibrate weights?
      IF (DOWTCL) THEN
         HILINE = TSKNAM // '/ Weights calibrated'
      ELSE
         HILINE = TSKNAM // '/ Weights not calibrated'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
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
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
C
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       copy antenna files
      CALL FNDEXT ('AN', CATUV, NSUB)
C                                       Get freq from AN table
      SUBA = 1
      CALL GETANT (DISKIN, CNOIN, SUBA, CATUV, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
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
      DO 100 SUBA = 1,NSUB
         CALL ISTAB ('AN', DISKIN, CNOIN, SUBA, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF ((EXIST) .AND. (IERR.EQ.0)) THEN
            CALL ANSEL (DISKIN, CNOIN, DISK, CNO, SUBA, SUBA, CATUV,
     *         CATBLK, LUN1, LUN2, BIF, EIF, FQOFF, DOPOL, BUFF1, BUFF2,
     *         IERR)
            IF (IERR.GT.0) GO TO 999
            END IF
 100     CONTINUE
C                                       Clear status
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISK, CNO, NAMOUT, CLAOUT, CATBLK(KIIMS),
     *   UTYPE, NLUSER, 'CLWR', BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPLTHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ ')
 1015 FORMAT ('SPLTHI: ERROR',I3,' READING INPUT AN TABLE')
 2001 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 2002 FORMAT (A6,'BCHAN =',I4,', ECHAN =',I4,'/ Chan range')
 2003 FORMAT (A6,'SUBARRAY =',I4)
 2004 FORMAT (A6,'FLAGVER=',I4,'  / Edited using FG table version #')
 2005 FORMAT (A6,'GAINUSE =',I3,' / CL table')
 2010 FORMAT (A6,'STOKES = ''',A4,''' / Stokes type')
 2011 FORMAT (A6,'NCHAVG=',I4,'  CHINC=',I4,'  / average n channels',
     *   ' every ch channels')
 2012 FORMAT (A6,'APARM(2) =',F5.2,' / Integration time (sec)')
 2013 FORMAT (A6,'SMOOTH = ',I1,',',F6.1,',',I4,
     *   ' / Spectral smoothing parms')
 2014 FORMAT (A6,'/ Avgd: Start, Stop, Inc ',2I5,I4,'  IF=',I3)
 2015 FORMAT (A6,'/ Output data in compressed format')
 2016 FORMAT (A6,'DOPOL = ',I2,', polarization correction made')
 2017 FORMAT (A6,'BLVER=',I3,'  / BL table # applied to data')
 2018 FORMAT (A6,'DOBAND = ',I2,'  / TYpe of BP correction done')
 2019 FORMAT (A6,'BPVER=',I3,'  / BP correction used BP table #')
      END
      SUBROUTINE SPLCOP (VISOFF, TOTREC, FINISH, DOAVG, DOIF, DISK, CNO,
     *   NOSUB, BUFF1, JBUFSZ, FREQO, OLDFRQ, NUMHIS, HISCRD, CHNSEL,
     *   DOUVCM, LLOCWT, BUFF2, LRECU, NCRPM, AVGCH, CNTCHN, OLDRP,
     *   CHINC, NCHAVG, NPRM, PRMTRN, TMAXX, NOTSRT, RNXRET, IRET)
C-----------------------------------------------------------------------
C   SPLCOP reads and corrects data files with optional averaging
C   over the frequency axis.
C   Input:
C      VISOFF   I        Offset in output file
C      TOTREC   I(2,3)   Total counts of record flagging
C      FINISH   I        > 0 this finishes the source, compress, etc.
C                        > 1 no vis this time
C      DOAVG    L        If true, average frequencies in IF
C      DOIF     L        If true, average across IF's also
C      DISK     I        Output disk number.
C      CNO      I        Output catalog slot number.
C      NOSUB    L        IF True drop subarray code.
C      JBUFSZ   I        Size of BUFF1
C      FREQO    D(*)     IF Frequency offsets for source (Hz)
C      OLDFRQ   D        Reference freq. of input data.
C      CHNSEL   I(*)     Start, stop and incr channel # to average
C      DOUVCM   L        If TRUE write compressed data
C      LLOCWT   I        Offset of compressed weight r.p in output data
C      LRECU    I        Length of output vis record
C      NCRPM    I        # random parms in output vis record
C      CHINC    I        Channel increment between oputput channels
C      NCHAVG   I        # of channels to average each output channel
C      NPRM     I        Number of UVGET rprams used
C      PRMTRN   I(14)    Translation indices of old RPARM to new
C   In/out
C      TMAXX    R        MAX time in data set
C      NOTSRT   L        Not in time order
C      RNXRET   I        NX table error code
C   Input from common:
C    INCF   I     Increment in freq. of data from UVGET
C    INCIF  I     Increment in IF of data from UVGET
C    INCS   I     Increment in Stokes' of data from UVGET
C    JLOCF  I     Offset of freq. of data from UVGET
C    JLOCIF I     Offset of IF of data from UVGET
C    JLOCS  I     Offset of Stokes' of data from UVGET
C   Output:
C    NUMHIS I     Number of history entries
C    HISCRD C*64  History entries
C    BUFF1  R     Output I/O buffer.
C    BUFF2(*)    R    Buffer for compressed data
C    IRET   I     Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      CHARACTER HISCRD(*)*64, NAME*48
      INTEGER   VISOFF, DISK, CNO, JBUFSZ, NUMHIS, LLOCWT, CNTCHN(*),
     *   CHINC, NCHAVG, NPRM, PRMTRN(*), IRET, FINISH, CHNSEL(3,20,*),
     *   RNXRET
      LOGICAL   DOAVG, NOSUB, DOUVCM, DOIF, NOTSRT
      REAL      BUFF1(*), BUFF2(*), TMAXX, AVGCH(*), OLDRP
      DOUBLE PRECISION    OLDFRQ, FDIFF
C
      INTEGER   LUN, FIND, LENBU, NIO, JNCIF, JNCS, NUMFRQ, NNIF, NOPOL,
     *   NCORR, BO, I, XCOUNT, NCOPY, TOTREC(2,3), BLCODE, IIVER, NCRPM,
     *   LRECU, BIND, FREQID, JNCF, TBIF, TEIF, NUMCH, J, JTRIM
      LOGICAL   T, F, DOFSCL, DOFSM
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   ISBAND(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      REAL      VIS(MAXCIF*3), FRSCL, CATR(256), FINC(MAXIF), RPARM(20),
     *   TIME, FINCIN
      DOUBLE PRECISION CATD(128), FOFF(MAXIF), FREQO(MAXIF), FOFF1,
     *   FREQO1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, BO /18, 1/
C-----------------------------------------------------------------------
C                                       Set lengths of input axes.
      DOFSM = (CHINC.GT.1) .OR. (NCHAVG.GT.1)
      NUMFRQ = ECHAN - BCHAN + 1
      NUMCH = ((NUMFRQ - 1) / CHINC) + 1
      NUMCH = MAX (NUMCH, 1)
      IF (DOAVG) NUMCH = 1
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
      DOFSCL = ABS (FRSCL-1.0) .GT. 1.0E-6
C                                       Set output increments
C                                       (averaging)
      JNCIF = INCIF
      IF (JLOCF.LT.JLOCIF) JNCIF = (INCIF / NUMFRQ) * NUMCH
      JNCS = INCS
      IF (JLOCF.LT.JLOCS) JNCS = (INCS / NUMFRQ) * NUMCH
      JNCF = INCF
C                                        If output file already open
      IF (FIND.GT.0) THEN
C                                        close it.
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
            GO TO 980
            END IF
         END IF
C                                       Copy file
      XCOUNT = 0
      IF (FINISH.GT.1) GO TO 115
      DO 100 I = 1,NVIS
C                                       Read data
         CALL UVGET ('READ', RPARM, VIS, IRET)
         IF (IRET.LT.0) GO TO 110
         IF (IRET.NE.0) GO TO 999
         TIME = RPARM(1+ILOCT)
         IF (TIME.LT.TMAXX) NOTSRT = .TRUE.
         TMAXX = MAX (TMAXX, TIME)
C                                       Average in IF and freq.
         IF (DOIF) THEN
            CALL AVGCIF (VIS, NOPOL, BCHAN, ECHAN, TBIF, TEIF, CHNSEL,
     *         JNCS, BUFF1)
C                                       Average with channel selection
         ELSE IF (DOAVG) THEN
            CALL AVGCHN (VIS, NOPOL, BCHAN, ECHAN, TBIF, TEIF, CHNSEL,
     *         JNCS, JNCIF, BUFF1)
C                                       Smooth in freq
         ELSE IF (DOFSM) THEN
            CALL AVGSMO (VIS, NOPOL, NUMIF, NUMFRQ, NCHAVG, CHINC, JNCS,
     *         JNCIF, BUFF1)
         ELSE
            CALL RCOPY (NCOPY, VIS, BUFF1)
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
            BUFF2(BIND+J-1) = RPARM(PRMTRN(J))
 20         CONTINUE
C                                       update NX table
         CALL RNXUPD (BUFF2(BIND), RNXRET)
C                                       Write new
         NIO = 1
         XCOUNT = XCOUNT + 1
         IF (DOUVCM) THEN
            CALL ZUVPAK (NCORR, BUFF1, BUFF2(BIND+LLOCWT),
     *         BUFF2(BIND+NCRPM))
         ELSE
            CALL RCOPY (NCOPY, BUFF1, BUFF2(BIND+NCRPM))
            END IF
         CALL UVDISK ('WRIT', LUN, FIND, BUFF2, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1090) IRET
            GO TO 980
            END IF
 100     CONTINUE
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
 115  IF (FINISH.LE.0) GO TO 800
C                                       Flush output
         NIO = 0
         CALL UVDISK ('FLSH', LUN, FIND, BUFF2, NIO, BIND, IRET)
         IF (IRET.EQ.0) GO TO 120
            WRITE (MSGTXT,1090) IRET
            GO TO 980
C                                       Compress output file.
 120     NVIS = XCOUNT + VISOFF
         CALL UCMPRS (NVIS, DISK, CNO, LUN, CATBLK, IRET)
C                                      Put vis. count in CATBLK
         CATBLK(KIGCN) = NVIS
C                                       Update CATBLK.
         CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFF1, IRET)
         IF (IRET.EQ.0) GO TO 140
            WRITE (MSGTXT,1130) IRET
C                                       Copy relevant portion of IF
C                                       table.
 140     IF (JLOCIF.GE.0) THEN
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
                  IF (DOAVG) THEN
                     FINC(I) = FINC(I) * CNTCHN(I)
                  ELSE IF (DOFSM) THEN
C                                       SPLIT does not change freq
C                                       it moves ref pixel instead
C                     FOFF(I) = FOFF(I) + (FINC(I) - FINCIN) *
C     *                  (AVGCH(TBIF) - OLDRP)
                     FINC(I) = FINC(I) * CHINC
                     END IF
C                                       force the first IF to zero
                  FOFF(I) = FOFF(I) - FOFF1 + (FREQO(I) - FREQO1)
                  IF (ABS(FOFF(I)).LT.1.0E-3) FOFF(I) = 0.0D0
  150             CONTINUE
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
            WRITE (MSGTXT,2800)
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),2800)
            NUMHIS = NUMHIS + 1
            WRITE (MSGTXT,2801) TOTREC(1,1), TOTREC(1,2), TOTREC(1,3)
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),2801) TOTREC(1,1), TOTREC(1,2),
     *         TOTREC(1,3)
            NUMHIS = NUMHIS + 1
            WRITE (MSGTXT,2802) TOTREC(2,1), TOTREC(2,2), TOTREC(2,3)
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),2802) TOTREC(2,1), TOTREC(2,2),
     *         TOTREC(2,3)
            NUMHIS = NUMHIS + 1
         ELSE
            WRITE (MSGTXT,2803) NVIS
            IF (DOUVCM) WRITE (MSGTXT,2804) NVIS
            CALL MSGWRT (6)
            WRITE (HISCRD(NUMHIS+1),2803) NVIS
            NUMHIS = NUMHIS + 1
            END IF
         IF (NVIS.GT.0) GO TO 700
C                                       No data found.
            IRET = 9
            MSGTXT = 'SPLCOP: ERROR - NO DATA WRITTEN'
            GO TO 980
C                                       Close files
 700     CALL ZCLOSE (LUN, FIND, IRET)
 800  CALL UVGET ('CLOS', BUFF1(BIND), BUFF1(BIND+NRPARM), IRET)
      GO TO 999
C                                       Error
 980  CALL ZCLOSE (LUN, FIND, I)
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPLCOP: ERROR',I5,' OPENING OUTPUT FILE')
 1020 FORMAT ('SPLCOP: ERROR',I5,' INIT. OUTPUT FILE')
 1090 FORMAT ('SPLCOP: ERROR',I5,' WRITING OUTPUT FILE')
 1110 FORMAT ('SPLCOP FOUND NO SAMPLES OUT OF',I8,' POSSIBLE')
 1111 FORMAT ('       SOURCE, CALCODE, QUAL= ''',A,'''  ''',A,'''',I5)
 1112 FORMAT ('       IS SUBARRAY',I3,' IN CL/SN TABLE?')
 1130 FORMAT ('SPLCOP: ERROR',I3,' UPDATING CATALOGUE HEADER')
 2800 FORMAT (10X,' Previously flagged ','  flagged by gain   ',
     *   '      kept')
 2801 FORMAT ('Partially ',2(I15,5X),I10)
 2802 FORMAT ('Fully     ',2(I15,5X),I10)
 2803 FORMAT (I9,' Visibilities written')
 2804 FORMAT (I9,' Compressed visibilities written')
      END
      SUBROUTINE UPDKEY (BUFFER, KEYWRD, KEYTYP, KEYVAL, IERR)
C-----------------------------------------------------------------------
C  Routine which updates a keyword-value pairs of an existing
C  table.
C   Inputs:
C     BUFFER      I(*)       Work buffer
C     KEYWRD      C*8        Keyword name
C     KEYTYP      I          Keyword type
C     KEYVAL      D          Keyword value
C   Outputs:
C     IERR        I          Error code, 0 => OK
C                            anything else => problem
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
