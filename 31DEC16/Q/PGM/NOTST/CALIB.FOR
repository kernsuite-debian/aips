LOCAL INCLUDE 'CALIB.INC'
C                                                          Include CALIB
C                                       Local include for CALIB
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXPRM, XBFSZ, MPOLIF
C                                       MAXPRM = maximum no. parms in
C                                       Least squares solutions
      PARAMETER (MAXPRM = MAXANT * 2)
C                                       XBFSZ = buffer size
      PARAMETER (XBFSZ = UVBFSL)
C                                       MPOLIF = Max NPOL*MAXIF
      PARAMETER (MPOLIF = MAXIF*2)
      INTEGER   CATIN(256), SEQIN, SEQ2, SEQOUT, DISKIN, DISK2, DISOUT,
     *   CNOIN, CNOOUT, JBUFSZ, BUFFS(XBFSZ), NANT, CNOIN2, CCTVER,
     *   NFREQ, REFANT, NPOL, NVAL, SNVER, VISDSK, VISCNO, VER, NUMNOD,
     *   NUMBL, NUMTIM, PRTLV, BLDO, MODE, MINNO, LOCIF, LOCF, CNTMGM,
     *   CNTMG(MPOLIF), IPOLIF, NFLAGD, DOMGM, CHNSEL(3,20,MAXIF),
     *   LECHAN, LBCHAN, DOSAVG, CNTMMM
      LOGICAL   SINGLE, DOMODL, TSMOTH, AVGIF, AVGPOL, DOL1, DOGCON,
     *   DOPASS, GSOLV(MAXANT+1), SLMEAN(MAXANT), ALZERO, ONENEG, FLAGIT
      INTEGER   NCOMP(MAXFLD), REFUSE(MAXANT), REFUSS(MAXANT,100)
      INTEGER   LSTUSE(MAXANT), NUSE
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XNAME2(3),
     *   XCLAS2(2), XNAMOU(3), XCLAOU(2), XXSOLT, XXSOLM, XCMETH, XCMOD
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XCALCO*4, NAME2*12,
     *   CLAS2*6, NAMOUT*12, CLAOUT*6, XSOLTY*4, XSOLMO*4, CMETH*4,
     *   CMOD*4
      REAL      XSI, XDI, XQUAL, XBAND, XFREQ, XFQID, XTIME(8),
     *   XCHNS(4,20),XANTS(50), XANUSE(50), XSUBA, XUVRA(2), XWTUV,
     *   XWTIT, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH(3), XS2, XD2, XVER, XSO, XDO, APPLY,
     *   XNCOMP(MAXAFL), XFLUX, XNMAP, SMODEL(7), XREFA, XSOLIN, XSOLS,
     *   XSOLM, APARM(10), XDOFL, XSOLCO, MNAMPE, MNPHSE, XNORM,
     *   CPARM(10), XSNVER, XANT(30), XGERR(30), XBADD(10),
     *   DELTIM, BUFF1(XBFSZ), BUFF2(XBFSZ), ANTWT(MAXANT), IATOFF,
     *   SOLINT, DELWIN, RATWIN, GAERR(MAXANT), CONFAC, SNRMIN, MXPABL,
     *   MNPABL, WTPABL, FINC(MAXIF), SUMMGM, SUMMG(MPOLIF),
     *   CLOSER(2,2), XDOFIT(30), GMINEL, SUMMMM, AGMMO(MAXIF,2,100)
      INTEGER   ANTUSE(50), CNTOK, CNTBAD, CNTFEW, CNTSAD, TOTREC(2,3),
     *   ISBAND(MAXIF)
      DOUBLE PRECISION  RANOD, DECNOD
      CHARACTER BNDCOD(MAXIF)*8
      COMMON /CINFO/ RANOD, DECNOD, CATIN, DELTIM, IATOFF, ANTWT,
     *   SOLINT, DELWIN, RATWIN, GAERR, CONFAC, SNRMIN, MXPABL, MNPABL,
     *   WTPABL, NCOMP, REFUSE, SINGLE, DOMODL, TSMOTH, AVGIF, GSOLV,
     *   AVGPOL, DOMGM, DOL1, DOGCON, DOSAVG, DOPASS, CNOIN, CNOOUT,
     *   NANT, NFREQ, NPOL, NVAL, REFANT, CNTMGM, SUMMGM, CNOIN2,
     *   CCTVER, SNVER, VISDSK, VISCNO, VER, NUMNOD, NUMBL, NUMTIM,
     *   PRTLV, BLDO, MODE, MINNO, LOCIF, LOCF, FINC, ISBAND, ANTUSE,
     *   LSTUSE, NUSE, CLOSER, CNTMG, SUMMG, IPOLIF, SLMEAN, ALZERO,
     *   ONENEG, DISKIN, DISK2, DISOUT, SEQIN, SEQ2, SEQOUT, GMINEL,
     *   FLAGIT, NFLAGD, CHNSEL, LECHAN, LBCHAN, SUMMMM, CNTMMM, REFUSS,
     *   AGMMO
      COMMON /RESLTS/ CNTOK, CNTBAD, CNTFEW, CNTSAD, TOTREC
      COMMON /BUFRS/ BUFF1, BUFF2, BUFFS, JBUFSZ
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XCALCO, NAME2, CLAS2,
     *   NAMOUT, CLAOUT, XSOLTY, XSOLMO, CMETH, CMOD, BNDCOD
      COMMON /XINPUT/ XNAMEI, XCLAIN, XSI, XDI, XXSOUR, XQUAL, XXCALC,
     *   XBAND, XFREQ, XFQID, XTIME, XCHNS, XANTS, XDOFIT, XANUSE,
     *   XSUBA, XUVRA, XWTUV, XWTIT, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XNAME2, XCLAS2, XS2,
     *   XD2, XVER, XNCOMP, XFLUX, XNMAP, XCMETH, XCMOD, SMODEL, XNAMOU,
     *   XCLAOU, XSO, XDO, APPLY, XREFA, XSOLIN, XSOLS, XSOLM, APARM,
     *   XDOFL, XXSOLT, XXSOLM, XSOLCO, MNAMPE, MNPHSE, XNORM, CPARM,
     *   XSNVER, XANT, XGERR, XBADD
LOCAL END
LOCAL INCLUDE 'CALFLAG'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   FGKOLS(MAXFGC), FGNUMV(MAXFGC), FGVERO, LFGRNO,
     *   FGBUFL(512), FLGLUN
      CHARACTER REASON*24
      COMMON /CALFP/ FGBUFL, FGKOLS, FGNUMV, LFGRNO, FGVERO, FLGLUN
      COMMON /CALFC/ REASON
LOCAL END
      PROGRAM CALIB
C-----------------------------------------------------------------------
C! Basic continuum calibration routine using source models
C# UV Calibration EXT-appl AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-2016
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
C   This task determines the calibration to be applied to a uv data set
C   given a model of the source(s).  The output data will have the
C   corrections applied for a single source input file; and the
C   Cal (CL) table will be updated for a multi source data set.
C       For multisource (raw) data sets the current contents of a
C   specified Cal (CL) table can be applied to the data before
C   determining the new calibration constants.
C   Adverbs:
C                                      Input uv data.
C   INNAME                                UV file name (name)
C   INCLASS                               UV file name (class)
C   INSEQ              0.0      9999.0    UV file name (seq. #)
C   INDISK             0.0         9.0    UV file disk drive #
C                                      Data selection (multisource):
C   CALSOUR                            Calibrator sources
C   QUAL            -10.0              Calibrator qualifier -1=>all
C   CALCODE                            Calibrator code '    '=>all
C   SELBAND                            Bandwidth to select (kHz)
C   SELFREQ                            Frequency to select (MHz)
C   FREQID                             Freq. ID to select.
C   TIMERANG                           Time range to use.
C   ICHANSEL          0.0     2048.0   begin/end chan, incr, if
C                                      for averaging data
C   ANTENNAS                           Antennas to select. 0=all
C   DOFIT                              Subset of ANTENNAS list for
C                                      which solns are desired.  0
C                                      => all in ANTENNAS, < 0 all
C                                      but those in DOFIT
C   ANTUSE                             Mean gain is calculated
C                                      (CPARM(2)>0) using only the
C                                      listed antennas. See explain.
C   SUBARRAY          0.0     1000.0   Subarray, 0=>all
C   UVRANGE                            Range of uv distance for full
C                                      weight
C   WTUV                               Weight outside UVRANGE 0=0.
C
C                                      Cal. info for input:
C   DOCALIB          -1.0        2.0   If >0 calibrate data
C                                      = 2 calibrate weights
C   GAINUSE                            CL table to apply.
C   DOPOL            -1.0       10.0   If >0 correct polarization.
C   BLVER                              BL table to apply.
C   FLAGVER                            Flag table version
C   DOBAND           -1.0       10.0   If >0 apply bandpass cal.
C                                      Method used depends on value
C                                      of DOBAND (see HELP file).
C   BPVER                              Bandpass table version
C   SMOOTH                             Smoothing function. See
C                                      HELP SMOOTH for details.
C
C                                      CLEAN map. See HELP.
C   IN2NAME                               Cleaned map name (name)
C   IN2CLASS                              Cleaned map name (class)
C   IN2SEQ            0.0     9999.0      Cleaned map name (seq. #)
C   IN2DISK           0.0        9.0      Cleaned map disk unit #
C   INVERS           -1.0    46655.0   CC file version #.
C   NCOMP                              # comps to use for model.
C                                      1 value per field
C   FLUX                               Lowest CC component used.
C   NMAPS             0.0     4096.0   No. Clean map files
C   CMETHOD                            Modeling method:
C                                      'DFT','GRID','    '
C   CMODEL                             Model type: 'COMP','IMAG'
C   SMODEL                             Source model, 1=flux,2=x,3=y
C                                      See HELP SMODEL for models.
C
C                                      Output uv data file.
C   OUTNAME                               UV file name (name)
C   OUTCLASS                              UV file name (class)
C   OUTSEQ            -1.0      9999.0    UV file name (seq. #)
C   OUTDISK            0.0         9.0    UV file disk drive #
C
C                                      Solution control adverbs:
C   REFANT                             Reference antenna
C   SOLINT                             Solution interval (min)
C   APARM                              General parameters
C                                         1=min. no. antennas
C                                         2 > 0 => data divided
C                                         3 > 0 => avg. RR,LL
C                                         5 > 0 => avg. IFs.
C                                         6=print level, 1=good,
C                                           2 closure, 3 SNR
C                                         7=SNR cutoff (0=>5)
C                                         8=max. ant. # (no AN)
C                                         9 > 0 => pass failed soln
C                                      Phase-amplitude Parameters:
C   SOLTYPE                            Soln type,'  ','L1','GCON'
C   SOLMODE                            Soln. mode: 'A&P','P','P!A',
C                                      'GCON', 'A&PR', 'PR', 'P!AR'
C   SOLCON                             Gain constraint factor.
C   MINAMPER          0.0              Amplitude closure error
C                                      regarded as excessive in %
C   MINPHSER          0.0              Phase closure error regarded
C                                      as excessive in degrees
C   CPARM                              Phase-amp. parameters
C                                         1 = Min el for gain
C                                              normalization (deg)
C                                         2 >0 => normalize gain
C                                         3 avg. amp. closure err
C                                         4 avg. ph. closure err
C                                         5 >0 => scalar average
C                                         6 limit robust discard levels
C   SNVER             -1.0     46655.0 Output SN table, 0=>new table
C   ANTWT                              Ant. weights (0=>1.0)
C   GAINERR                            Std. Dev. of antenna gains.
C   BADDISK                            Disk no. not to use for
C                                         scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   MAXBL, MAXFRQ
      INTEGER   NUMSUB, ISUB, IS1, IS2, IST, IRET, IPOL, J, KK2, IERR
      LOGICAL   GOTD
      INCLUDE 'CALIB.INC'
      INCLUDE 'CALFLAG'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      REAL      GMMO(MAXIF,2)
      DATA PRGM /'CALIB '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL CLBIN (PRGM, MAXBL, MAXFRQ, NUMSUB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop over subarrays
      GOTD = .FALSE.
      IST = SUBARR
      IF (SUBARR.EQ.0) THEN
         IS1 = 1
         IS2 = NUMSUB
      ELSE
         IS1 = SUBARR
         IS2 = SUBARR
         IF (DOMGM.GE.6) DOMGM = DOMGM - 2
         END IF
      DO 100 ISUB = IS1,IS2
         WRITE (MSGTXT,1000) ISUB, IS2
         IF (IS1.NE.IS2) CALL MSGWRT (3)
         SUBARR = ISUB
         IF (DOMGM.GE.3) THEN
            CNTMGM = 0
            SUMMGM = 0.0
            END IF
C                                       Select data.
         CALL CLBSEL (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Check if data found
         IF (NVIS.LE.0) GO TO 100
         GOTD = .TRUE.
C                                       Divide data by model if nec.
         IF (DOMODL) CALL CLBMOD (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Do solutions.
         CALL CLBSOL (MAXBL, MAXFRQ, IRET)
         IF (IRET.NE.0) GO TO 990
 100     CONTINUE
C                                       No data selected
      IF (.NOT.GOTD) THEN
         IRET = 1
         WRITE (MSGTXT,1100)
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       normalize over subarray
      IF (DOMGM.GE.6) THEN
         SUBARR = -1
         IF (DOMGM.EQ.6) THEN
            MSGTXT = 'Apply mean gain modulus to SN table by IF'
            CALL MSGWRT (3)
            DO 120 KK2 = BIF,EIF
               J = 0
               GMMO(KK2,1) = 0.0
               DO 110 ISUB = 1,NUMSUB
                  DO 105 IPOL = 1,NUMPOL
                     IF (AGMMO(KK2,IPOL,ISUB).NE.FBLANK) THEN
                        GMMO(KK2,1) = GMMO(KK2,1) + AGMMO(KK2,IPOL,ISUB)
                        J = J + 1
                        END IF
 105                 CONTINUE
 110              CONTINUE
               IF (J.GT.0) THEN
                  GMMO(KK2,1) = GMMO(KK2,1) / J
               ELSE
                  GMMO(KK2,1) = 1.0
                  END IF
               WRITE (MSGTXT,1110) KK2, GMMO(KK2,1)
               CALL MSGWRT (5)
               GMMO(KK2,2) = GMMO(KK2,1)
 120           CONTINUE
         ELSE IF (DOMGM.EQ.7) THEN
            MSGTXT = 'Apply mean gain modulus to SN table by IF/pol'
            CALL MSGWRT (3)
            DO 140 KK2 = BIF,EIF
               DO 130 IPOL = 1,NUMPOL
                  J = 0
                  GMMO(KK2,IPOL) = 0.0
                  DO 125 ISUB = 1,NUMSUB
                     IF (AGMMO(KK2,IPOL,ISUB).NE.FBLANK) THEN
                        GMMO(KK2,IPOL) = GMMO(KK2,IPOL) +
     *                     AGMMO(KK2,IPOL,ISUB)
                        J = J + 1
                        END IF
 125                 CONTINUE
                  IF (J.GT.0) THEN
                     GMMO(KK2,IPOL) = GMMO(KK2,IPOL) / J
                  ELSE
                     GMMO(KK2,IPOL) = 1.0
                     END IF
 130              CONTINUE
               WRITE (MSGTXT,1130) KK2, IPOL, GMMO(KK2,IPOL)
               CALL MSGWRT (5)
 140           CONTINUE
            END IF
         CALL GMODIT (DISKIN, CNOIN, SNVER, SUBARR, BIF, EIF, CATIN,
     *      GMMO, IERR)
         END IF
C                                       Restore subarray
      SUBARR = IST
C                                       Smooth solutions
      CALL CLBADJ (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Apply solns. and average data.
C                                       Single source files only.
      IF ((SINGLE) .AND. (DOMODL) .AND. (APPLY.GE.0.0)) THEN
         IF (NFLAGD.GT.0) FGVER = FGVERO
         CALL CLBAPL (IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       Write history.
      CALL CLBHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT (' START SUBARRAY ',I4,' OF ',I4)
 1100 FORMAT (' WARNING: NO DATA SELECTED')
 1110 FORMAT ('IF =',I3,' apply average gain over subarray/pol',
     *   1PE10.3)
 1130 FORMAT ('IF =',I3,' pol',I2,' apply average gain over subarray',
     *   1PE10.3)
      END
      SUBROUTINE CLBIN (PRGN, MAXBL, MAXFRQ, NUMSUB, IRET)
C-----------------------------------------------------------------------
C   CLBIN gets input parameters for CALIB and creates an output file
C   if necessary.
C   Inputs:  PRGN    C*6       Program name
C   Output:  MAXBL   I         Maximum number of baselines in data.
C            MAXFRQ  I         Maximum number of frequency channels.
C            NUMSUB  I         Number of subarrays (AN tables)
C            IRET    I         Error code: 0 => ok
C                                1 => too few frequency channels.
C                                5 => catalog troubles
C                                7 => Too many ant. for ls.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in CALIB for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6, STAT*4, MODCOD(4)*4, TYPCOD(6)*4, UTYPE*2,
     *   ATIME*8, ADATE*12, LSTOK*4
      HOLLERITH CATH(256)
      INTEGER   MAXBL, MAXFRQ, NUMSUB, IRET
      INTEGER   IERR, NPARM, I, J, MXFLD, SOLTYP, IROUND, MXANT, NUMFRQ,
     *   LUN1, ANVER, LIM1, LIM2, KK1, IKK1, DATE(3), TIME(3), K, K1,
     *   K2, INDEX, LLOCWT
      LOGICAL   T, F, TABLE, EXIST, FITASC, MATCH, DOCMP
      REAL      CATR(256), DUM
      DOUBLE PRECISION CATD(128)
      INCLUDE 'CALIB.INC'
      INCLUDE 'CALFLAG'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      INTEGER   NW(MAXIF), NCHAN
      EQUIVALENCE (CATR, CATBLK, CATH, CATD)
      DATA MODCOD /'A&P','P','P!A','GCON'/
      DATA TYPCOD /' ','L1  ','GCON', 'R', 'L1R', 'GCOR'/
      DATA LUN1 /28/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = 2 * XBFSZ
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
      NSOUWD = 1
      CNTMGM = 0
      SUMMGM = 0.0
      CNTMMM = 0
      SUMMMM = 0.0
      NFLAGD = 0
C                                       Get input parameters.
      MXFLD = MAXAFL
      NPARM = 363 + MXFLD + 50 + 80
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSI)
      SEQ2 = IROUND (XS2)
      SEQOUT = IROUND (XSO)
      DISKIN = IROUND (XDI)
      DISK2 = IROUND (XD2)
      DISOUT = IROUND (XDO)
      CCTVER = IROUND (XVER)
      CCTVER = MAX (0, CCTVER)
      FLAGIT = XDOFL.GT.0.0
      IF (XDOFL.GT.0.0) THEN
         IF (XDOFL.LT.2.0) XDOFL = 2.5
      ELSE IF (XDOFL.GT.-1.0) THEN
         XDOFL = -2.5
         IF (APARM(6).GT.0.5) XDOFL = -99.0
         END IF
      XDOFL = ABS (XDOFL)
C                                       Set flagging reason
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      REASON = TSKNAM // ADATE // ' ' // ATIME(:5)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XXSOLT, XSOLTY)
      CALL H2CHR (4, 1, XXSOLM, XSOLMO)
      CALL H2CHR (4, 1, XCMETH, CMETH)
      CALL H2CHR (4, 1, XCMOD, CMOD)
      GSOLV(1) = .FALSE.
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         J = IROUND (XDOFIT(I))
         IF (J.NE.0) GSOLV(1) = .TRUE.
 10      CONTINUE
C                                      Default ant. wt = 1.0
      MXANT = MAXANT
      CALL RFILL (MXANT, 1.0, ANTWT)
      DO 15 I = 1, 30
         ANTWT(I) = XANT(I)
         IF (XANT(I).LE.0.0) ANTWT(I) = 1.0
 15      CONTINUE
C                                       Zero ref. ant. count.
      CALL FILL (MAXANT, 0, REFUSE)
      I = 100 * MAXANT
      CALL FILL (I, 0, REFUSS)
C                                       Get CATBLK from old file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
         END IF
C                                       Save Input file info
      VISDSK = DISKIN
      VISCNO = CNOIN
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       set flag versions
      CALL FNDEXT ('FG', CATBLK, I)
      FGVERO = I + 1
      FGVER = IROUND (XFLAG)
      IF ((FGVER.EQ.0) .OR. (FGVER.GT.I)) FGVER = I
      LFGRNO = -1
      FLGLUN = 48
      CALL FILL (512, 0, FGBUFL)
C                                       See if a multiple source file
      LUNS(1) = 29
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNS, BUFF1, TABLE, EXIST,
     *   FITASC, IERR)
      SINGLE = (.NOT.EXIST) .OR. (IERR.NE.0) .OR. (ILOCSU.LT.0)
C
      WRITE (MSGTXT,3010) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
      CALL REFRMT (MSGTXT, '_', I)
      CALL MSGWRT (3)
C                                       Save IF and freq pointers
      LOCIF = JLOCIF
      LOCF = JLOCF
C                                       Freq id
      IF (SINGLE) THEN
         FRQSEL = 1
      ELSE IF (.NOT.SINGLE) THEN
         IF (XBAND.GT.0.0) SELBAN = XBAND
         IF (XFREQ.GT.0.0) SELFRQ = XFREQ
         FRQSEL = IROUND (XFQID)
         IF (FRQSEL.EQ.0) FRQSEL = -1
         CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN1, SELBAN, SELFRQ,
     *      MATCH, FRQSEL, IRET)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1040)
            IRET = 1
            GO TO 990
            END IF
         IF (IRET.GT.0) GO TO 999
         END IF
C                                       General parms
      SOLINT = XSOLIN / (24.0 * 60.0)
      MXPABL = XUVRA(2)
      IF (MXPABL.LE.1.0E-20) MXPABL = 1.0E15
      MNPABL = XUVRA(1)
      WTPABL = XWTUV
      REFANT = IROUND (XREFA)
      DOMODL = APARM(2).LE.1.0E-20
      PRTLV = IROUND (APARM(6))
      SNRMIN = APARM(7)
      AVGPOL = APARM(3).GT.0.0
      IF ((AVGPOL) .AND. ((ICOR0.NE.-1) .OR.
     *   (CATBLK(KINAX+JLOCS).LT.2))) THEN
         MSGTXT = 'POLARIZATION AVERAGING INAPPROPRIATE - TURNED OFF'
         CALL MSGWRT (6)
         AVGPOL = .FALSE.
         END IF
      AVGIF = APARM(5).GT.0.0
      IF ((AVGIF) .AND. ((JLOCIF.LT.0) .OR.
     *   (CATBLK(KINAX+JLOCIF).LT.2))) THEN
         MSGTXT = 'AVERAGING OF IFs INAPPROPRIATE - TURNED OFF'
         CALL MSGWRT (6)
         AVGIF = .FALSE.
         END IF
C                                       Pass data with failed soln?
      DOPASS = APARM(9).GT.0.01
      IF (DOPASS) THEN
         MSGTXT = 'Warning: failed solutions replaced by (1,0)'
         CALL MSGWRT (6)
         END IF
C                                       Def. SNR min = 5.0
      IF (SNRMIN.LE.1.0E-30) SNRMIN = 5.0
C                                       Default s.i. = 10 sec.
      IF (SINGLE .AND. (SOLINT.LE.1.0E-10))
     *   SOLINT = 10.0 / (24.0 * 60.0 * 60.0)
C                                       Default multisource = scan
      IF ((.NOT.SINGLE) .AND. (SOLINT.LE.1.0E-10)) SOLINT = 0.1
C                                       Solution type.
      SOLTYP = -1
      IF (XSOLTY.EQ.TYPCOD(1)) SOLTYP = 1
      IF (XSOLTY.EQ.TYPCOD(2)) SOLTYP = 2
      IF (XSOLTY.EQ.TYPCOD(3)) SOLTYP = 3
      IF (XSOLTY.EQ.TYPCOD(4)) SOLTYP = 4
      IF (XSOLTY.EQ.TYPCOD(5)) SOLTYP = 5
      IF (XSOLTY.EQ.TYPCOD(6)) SOLTYP = 6
      IF ((GSOLV(1)) .AND. (SOLTYP.NE.1) .AND. (SOLTYP.NE.4)) THEN
         MSGTXT = 'Sorry. DOFIT invalidates SOLTYPE for now...'
         CALL MSGWRT (6)
         SOLTYP = SOLTYP/4 + 1
         SOLTYP = MAX (1, SOLTYP)
         END IF
      DOL1 = SOLTYP.EQ.2 .OR. SOLTYP.EQ.5
      DOGCON = SOLTYP.EQ.3 .OR. SOLTYP.EQ.6
C                                       Tell user which type
      IF (DOL1) THEN
         MSGTXT = 'L1 Solution type'
         CALL MSGWRT (3)
         END IF
      IF (DOGCON) THEN
         MSGTXT = 'Gain constraint solution type'
         CALL MSGWRT (3)
         END IF
C                                       Invalid TYPE
      IF (SOLTYP.LT.0) THEN
         WRITE (MSGTXT,1060) XSOLTY
         IRET = 6
         GO TO 990
         END IF
      IF ((.NOT.DOGCON) .AND. ((MODE.EQ.3) .OR. (MODE.EQ.7))) THEN
         MSGTXT = 'SOLMODE IS ''GCON'' BUT SOLTYPE IS NOT'
         IRET = 6
         GO TO 990
         END IF
C                                       Solution mode
      MODE = -1
      IF (XSOLMO.EQ.MODCOD(1)) MODE = 0
      IF (XSOLMO.EQ.MODCOD(2)) MODE = 1
      IF (XSOLMO.EQ.MODCOD(3)) MODE = 2
      IF (XSOLMO.EQ.MODCOD(4)) MODE = 3
      IF ((MODE.EQ.-1) .AND. SINGLE) MODE = 1
      IF (MODE.EQ.-1) MODE = 0
      IF ((MODE.EQ.0) .OR. (MODE.EQ.3)) THEN
         DOMGM = 1
         IF (XNORM.GT.0.0) DOMGM = 2
         IF (XNORM.GT.1.5) DOMGM = 3
         IF (XNORM.GT.2.5) DOMGM = 4
         IF (XNORM.GT.3.5) DOMGM = 5
         IF (XNORM.GT.4.5) DOMGM = 6
         IF (XNORM.GT.5.5) DOMGM = 7
      ELSE
         DOMGM = 0
         END IF
      IF (SOLTYP.GT.3) THEN
         MODE = MODE + 4
         SOLTYP = SOLTYP - 3
         END IF
      CONFAC = XSOLCO
      IF (CONFAC.LE.1.0E-20)  CONFAC = 1.0
      IF (CPARM(5).LE.0.0) THEN
         DOSAVG = 0
      ELSE
         DOSAVG = MAX (1., MIN (2., CPARM(5)+0.5)) + 0.01
         END IF
C                                       Check closure error print limits
      IF (CPARM(3).LE.1.0E-10) CPARM(3) = 1.E12
      IF (CPARM(4).LE.1.0E-10) CPARM(4) = 1.E12
      IF (MNAMPE.LE.1.0E-10) MNAMPE = 1.E12
      IF (MNPHSE.LE.1.0E-10) MNPHSE = 1.E12
C                                       Solving for gain?
      IF ((MODE.NE.0) .AND. (MODE.NE.3)) CPARM(3) = 1.0E12
      IF ((MODE.NE.0) .AND. (MODE.NE.3)) MNAMPE = 1.0E12
      I = IROUND (XWTIT)
      IF ((I.GT.0) .AND. (I.LE.3)) MODE = 10 * I + MODE
C                                       Convert to fraction, rad.
      CLOSER(1,1) = CPARM(3) * 0.01
      CLOSER(1,2) = CPARM(4) / 57.296
      CLOSER(2,1) = MNAMPE * 0.01
      CLOSER(2,2) = MNPHSE / 57.296
      CALL RFILL (MXANT, 0.0, GAERR)
      CALL RCOPY (30, XGERR, GAERR)
C                                       BADDISK
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 20      CONTINUE
C                                       Check sort order, must be T*
      IF (ISORT(1:1) .NE.'T') THEN
         IRET = 4
         WRITE (MSGTXT,1070) ISORT, 'T*'
         GO TO 990
         END IF
C                                       Save input header.
      CALL COPY (256, CATBLK, CATIN)
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      IF (.NOT.SINGLE) THEN
         DO 25 I = 1,30
            SOURCS(I) = XSOUR(I)
            CALSOU(I) = XSOUR(I)
 25         CONTINUE
         SELQUA = IROUND (XQUAL)
         SELCOD = XCALCO
         END IF
      CALL RCOPY (8, XTIME, TIMRNG)
      NCHAN = CATBLK(KINAX+JLOCF)
      DXTIME = 0.0
      BIF = 1
      EIF = 1
      IF (JLOCIF.GE.0) EIF = CATBLK(KINAX+JLOCIF)
C                                       Channel selection
      I = 60 * MAXIF
      CALL FILL (I, 0, CHNSEL)
      CALL FILL (MAXIF, 0, NW)
      DO 40 J = 1,20
         K = IROUND (XCHNS(2,J))
         IF (K.GT.0) THEN
            K = IROUND (XCHNS(4,J))
            IF ((K.LE.0) .OR. (K.GT.MAXIF)) THEN
               K1 = 1
               K2 = MAXIF
            ELSE
               K1 = K
               K2 = K
               END IF
            DO 35 K = K1,K2
               NW(K) = NW(K) + 1
               DO 30 I = 1,3
                  CHNSEL(I,NW(K),K) = IROUND (XCHNS(I,J))
                  IF (CHNSEL(I,NW(K),K).LT.0) CHNSEL(I,NW(K),K) = 0
 30               CONTINUE
               IF (CHNSEL(3,NW(K),K).EQ.0) CHNSEL(3,NW(K),K) = 1
 35            CONTINUE
            END IF
 40      CONTINUE
C                                       If no channel selection
C                                       use VLA definition of
C                                       channel 0
      BCHAN = NCHAN
      ECHAN = 1
      DO 50 K = 1,MAXIF
         IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = (NCHAN+1)/8 + 1
            CHNSEL(2,1,K) = NCHAN - ((NCHAN+1)/8)
            CHNSEL(3,1,K) = 1
            END IF
         DO 45 I = 1,NW(K)
            CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), NCHAN))
            IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K))
     *         CHNSEL(2,I,K) = NCHAN
            CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), NCHAN))
            BCHAN = MIN (BCHAN, CHNSEL(1,I,K))
            ECHAN = MAX (ECHAN, CHNSEL(2,I,K))
 45         CONTINUE
 50      CONTINUE
C                                       squirrel away for HIstory
      LBCHAN = BCHAN
      LECHAN = ECHAN
C                                       Warn user about what will happen
C                                       if not all channels are selected
C                                       from a single source file
      IF (SINGLE
     *   .AND.((CHNSEL(1,1,1).GT.1) .OR. (CHNSEL(2,1,1).LT.NCHAN))) THEN
         MSGTXT = 'WARNING: Calibration will be determined from ' //
     *      'selected'
         CALL MSGWRT (6)
         MSGTXT = '         frequency channels but applied to all ' //
     *      'channels'
         CALL MSGWRT (6)
         END IF
C                                       offset to sub-set
      DO 55 K = 1,MAXIF
         DO 54 I = 1,NW(K)
            CHNSEL(1,I,K) = CHNSEL(1,I,K) - BCHAN + 1
            CHNSEL(2,I,K) = CHNSEL(2,I,K) - BCHAN + 1
 54         CONTINUE
 55      CONTINUE
C                                       initialize arrays needed for
C                                       normalization of gains
      DO 65 I = 1,2
         DO 60 J = BIF,EIF
            IPOLIF = I + (J - 1) * NUMPOL
            CNTMG(IPOLIF) = 0
            SUMMG(IPOLIF) = 0.0
 60         CONTINUE
 65      CONTINUE
C                                       Antennas
      DO 70 I = 1,50
         ANTENS(I) = IROUND (XANTS(I))
         ANTUSE(I) = IROUND (XANUSE(I))
 70      CONTINUE
C                                       If any GSOLV entries are non-blank
C                                       solve only for those antennas.
C                                       excepting the reference antenna
      GSOLV(1) = .FALSE.
      DO 75 I = 1,30
         J = IROUND (XDOFIT(I))
         IF (J.LT.0) GSOLV(1) = .TRUE.
 75      CONTINUE
      CALL LFILL (MAXANT, GSOLV(1), GSOLV(2))
      IF (GSOLV(1)) THEN
         DO 80 I = 1,30
            J = IROUND (XDOFIT(I))
            J = ABS (J)
            IF ((J.GT.0) .AND. (J.LE.MAXANT)) GSOLV(J+1) = .FALSE.
 80         CONTINUE
      ELSE
         DO 85 I = 1,30
            J = IROUND (XDOFIT(I))
            IF ((J.GT.0) .AND. (J.LE.MAXANT)) THEN
               GSOLV(J+1) = .TRUE.
               GSOLV(1) = .TRUE.
               END IF
 85         CONTINUE
         END IF
      IF ((XDOCAL.GT.0.0) .AND. (SINGLE)) THEN
         MSGTXT = 'DOCALIB FORCED TO -1 FOR SINGLE-SOURCE FILE'
         CALL MSGWRT (7)
         XDOCAL = -1.0
         END IF
      IF (GSOLV(1)) THEN
         IF (REFANT.LE.0) THEN
            IRET = 8
            MSGTXT = 'DOFIT OPTION REQUIRES A REFANT > 0'
            GO TO 990
         ELSE IF (GSOLV(REFANT+1)) THEN
            IRET = 8
            MSGTXT = 'REFANT MUST NOT BE IN DOFIT LIST'
            GO TO 990
            END IF
         END IF
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      SNVER = IROUND (XSNVER)
      BLVER = IROUND (XBLVER)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
C                                       Stokes' - ask for what's needed
      STOKES = '    '
      IF (CATD(KDCRV+JLOCS).GT.0.0) STOKES = 'I'
      IF ((CATD(KDCRV+JLOCS).LT.0.0) .AND. (CATBLK(KINAX+JLOCS).GE.2))
     *   STOKES = 'HALF'
C                                       Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Put new values in CATBLK.
      IF (CLAOUT.EQ.' ') CLAOUT = 'CALIB '
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       set no. frequencies
      NFREQ = CATBLK(KINAX+JLOCF)
      IRET = 0
      NANT = IROUND (APARM(8))
      IATOFF = 0.0
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
C                                       Max. ant. no.
      IF (NANT.LE.0) THEN
C                                       Check selected subarrays
         IF (SUBARR.GT.0) THEN
            LIM1 = SUBARR
            LIM2 = SUBARR
         ELSE
            LIM1 = 1
            LIM2 = NUMSUB
            END IF
         DO 165 I = LIM1,LIM2
            ANVER = I
            CALL GETANT (DISKIN, CNOIN, ANVER, CATIN, BUFFS, IRET)
            DO 162 J = 1, NSTNS
               NANT = MAX (NANT, TELNO(J))
 162           CONTINUE
C                                       Most likely screwup is for the
C                                       VLA.
            IF (IRET.NE.0) THEN
               NANT = 28
               MSGTXT = 'There was a problem with the AN table so I' //
     *            ' assume that this is VLA data'
               CALL MSGWRT (6)
               MSGTXT = 'If there are fewer than 28 ant. set APARM(8)'
     *            // ' if necessary'
               CALL MSGWRT (6)
               IRET = 0
               GO TO 170
               END IF
 165        CONTINUE
         END IF
C                                       Determine max. bl., time.
 170  NUMANT = NANT
C                                       Def. min. no. of antennas = 6
      MINNO = IROUND (APARM(1))
      IF (MINNO.LE.1) THEN
         MINNO = MIN (6, NUMANT/2)
         MINNO = MAX (3, MINNO)
      ELSE IF (MINNO.EQ.2) THEN
         MSGTXT = 'WARNING: PURE BASELINE RATHER THAN '
     *      // 'ONLY ANTENNA SOLUTIONS ALLOWED'
         CALL MSGWRT (6)
         END IF
C                                       Get the minimum elevation
C                                       for gain normalization.
      IF (DOMGM.GT.0) THEN
         GMINEL = CPARM(1)
      ELSE
         GMINEL = -90.0
         END IF
      IF ((GMINEL.GT.80.0) .OR. (GMINEL.LE.0.0)) GMINEL = -100.0
C                                       select the list of antennas
C                                       used in the averaging
      IF (DOMGM.GT.0) THEN
         ALZERO = T
         ONENEG = F
         DO 175 KK1 = 1,NUMANT
            SLMEAN(KK1) = T
            IF (KK1.LE.50) THEN
               ALZERO = ALZERO .AND. (ANTUSE(KK1).EQ.0)
               ONENEG = ONENEG .OR. (ANTUSE(KK1).LT.0)
               END IF
 175        CONTINUE
C                                       Deselect antennas if any of
C                                       ANTUSE is negative
         IF (ONENEG) THEN
            DO 180 KK1 = 1,50
               IKK1 = ABS (ANTUSE(KK1))
               IF ((IKK1.GT.0) .AND. (IKK1.LE.MAXANT)) SLMEAN(IKK1) = F
 180           CONTINUE
C                                       Use all antennas selected by
C                                       ANTUSE
         ELSE IF (.NOT.ALZERO) THEN
            DO 185 KK1 = 1,NUMANT
               SLMEAN(KK1) = F
 185           CONTINUE
            DO 190 KK1 = 1,50
               IKK1 = ANTUSE(KK1)
               IF ((IKK1.GT.0) .AND. (IKK1.LE.MAXANT)) SLMEAN(IKK1) = T
 190           CONTINUE
            END IF
         NUSE = 0
         DO 200 KK1 = 1,NUMANT
            IF (SLMEAN(KK1)) THEN
               NUSE = NUSE + 1
               LSTUSE(NUSE) = KK1
               END IF
 200        CONTINUE
         END IF
C
      NUMIF = 1
      IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
      MAXBL = (NANT * (NANT - 1)) / 2
      MAXFRQ = NUMIF * 2
      IRET = 0
C                                       Create output file for SINGLE
C                                       and not already divided by
C                                       model.
      IF ((SINGLE) .AND. (DOMODL) .AND. (APPLY.GE.0.0)) THEN
         BCHAN = 1
         ECHAN = NCHAN
         LSTOK = STOKES
         STOKES = ' '
         DOCMP = CATBLK(KINAX).EQ.1
C                                       get a proper UVGET header
         CALL UVGET ('INIT', DUM, DUM, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1140) IRET
            GO TO 990
            END IF
C                                       add compressed back
         IF (DOCMP) THEN
            LLOCWT = CATBLK(KIPCN)
            INDEX = KHPTP + 2 * LLOCWT
            CALL CHR2H (8, 'WEIGHT  ', 1, CATH(INDEX))
            CALL CHR2H (8, 'SCALE   ', 1, CATH(INDEX+2))
            CATBLK(KINAX) = 1
            CATBLK(KIPCN) = CATBLK(KIPCN) + 2
            END IF
         CALL UVGET ('CLOS', DUM, DUM, IRET)
         CNOOUT = 1
         FRW(NCFILE+1) = 3
         IRET = 4
         IF (.NOT.DOMODL) CATBLK(KIGCN) = 1
         NUMFRQ = CATBLK(KINAX+JLOCF)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQOUT
         CALL UVCREA (DISOUT, CNOOUT, BUFF1, IRET)
         CATBLK(KINAX+JLOCF) = NUMFRQ
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1150) IRET
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISOUT
         FCNO(NCFILE) = CNOOUT
         FRW(NCFILE) = FRW(NCFILE) - 1
C                                       Save CATBLK for output.
         CALL CATIO ('UPDT', DISOUT, CNOOUT, CATBLK, 'REST', BUFF1,
     *      IRET)
         IF ((IRET.NE.0) .AND. (IRET.LT.5)) THEN
            WRITE (MSGTXT,1170) IRET
            GO TO 990
            END IF
C                                       Other "single" parameters
C                                       allow flagging and BP cal now
C         DOBAND = 0
C         FGVER = -1
C                                       Make SN be cumulative not
C                                       incremental for single source
         SNVER = 0
C                                       copy keywords
         CALL KEYCOP (DISKIN, CNOIN, DISOUT, CNOOUT, IRET)
         STOKES = LSTOK
         BCHAN = LBCHAN
         ECHAN = LECHAN
         END IF
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL REFRMT (MSGTXT, '_', I)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLBIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' .',
     *   I5,' _DISK=',I3,' USID=',I4)
 1040 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1050 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('UNKNOWN SOLUTION TYPE = ',A4)
 1070 FORMAT ('SORT ORDER IS ',A2,' NOT ',A2,' AS REQUIRED')
 1140 FORMAT ('ERROR',I3,' OPENING INPUT FILE')
 1150 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1170 FORMAT ('ERROR',I3,' UPDATING OUTPUT FILE CATBLK')
 3010 FORMAT ('Using ',A12,' . ',A6,' .',I5,' _Disk=',I3,' Usid=',I6)
      END
      SUBROUTINE CLBSEL (IRET)
C-----------------------------------------------------------------------
C   CLBSEL will read a multi source data set into a temporary scratch
C   file.  Editing and calibration may be applied.
C   Inputs via common /SELCAL/  (Include DSEL.INC)
C      UNAME        C    AIPS name of input file.
C      UCLAS        C    AIPS class of input file.
C      UDISK        R    AIPS disk of input file.
C      USEQ         R    AIPS sequence of input file.
C      SOURCS(30)   C    Names (16 char) of up to 30 sources, *=>all
C                        First character of name '-' => all except those
C                        specified.
C      TIMRNG(8)    R    Start day, hour, min, sec, end day, hour,
C                        min,sec. 0's => all
C      UVRNG(2)     R    Minimum and maximum baseline lengths in
C                        1000's wavelengths. 0's => all
C      STOKES       C    Stokes types wanted.
C                        'I','Q','U','V','R','L','IQU','IQUV'
C      BCHAN        I    First channel number selected, 1 rel. to first
C                        channel in data base. 0 => all
C      ECHAN        I    Last channel selected. 0=>all
C      BIF          I    First IF number selected, 1 rel. to first
C                        IF in data base. 0 => all
C      EIF          I    Last IF selected. 0=>all
C      DOCAL        L    If true apply calibration, else not.
C      ANTENS(50)   I    List of antennas selected, 0=>all,
C                        any negative => all except those specified
C      FGVER        I    FLAG file version number, if .le. 0 then
C                        NO flagging is applied.
C      CLUSE        I    Cal file version number to apply.
C   Output:
C      IRET         I    Error code: 0 => OK,
C                        -1 => end of data
C                        >0 => failed, abort process.
C-----------------------------------------------------------------------
      INTEGER   IRET, LUN1, LUN2, IIVER, OOVER
      REAL     DUM
      INCLUDE 'CALIB.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION FOFF(MAXIF)
      DATA LUN1, LUN2 /28, 29/
C-----------------------------------------------------------------------
C                                       Setup
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.GT.0) GO TO 999
      IF ((NVIS.LE.0) .OR. (IRET.LT.0)) THEN
          MSGTXT = 'CLBSEL: NO DATA FOUND; CHECK ADVERBS'
          CALL MSGWRT (8)
          NVIS = 0
          GO TO 100
          END IF
C                                       Message
      IF (DOCAL) THEN
         IF (DOFLAG) THEN
            MSGTXT = 'Selecting, editing and calibrating the data'
         ELSE
            MSGTXT = 'Selecting and calibrating the data'
            END IF
      ELSE
         IF (DOFLAG) THEN
            MSGTXT = 'Selecting and editing the data'
         ELSE
            MSGTXT = 'Selecting the data'
            END IF
         END IF
      CALL MSGWRT (2)
C                                       Copy
      VISDSK = 0
      VISCNO = 0
      CALL CALCOP (VISDSK, VISCNO, BUFF1, JBUFSZ, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy relevant portion of IF
C                                       table.
      IIVER = 1
      OOVER = 1
      CALL CHNCOP (IIVER, OOVER, LUN1, LUN2, DISKIN, SCRVOL(VISCNO),
     *   CNOIN, SCRCNO(VISCNO), CATUV, CATBLK, BIF, EIF, FRQSEL,
     *   SFREQS, BUFF1, FOFF, ISBAND, FINC, IRET)
      GO TO 999
C                                       No data
 100  CALL UVGET ('CLOS', DUM, DUM, IRET)
C
 999  RETURN
      END
      SUBROUTINE CLBMOD (IRET)
C-----------------------------------------------------------------------
C   CLBMOD divides the CLEAN model visibilities into the data.
C   If no model is found or a point model is specified then the data
C   is divided by the flux density found in the Source (SU) table.
C   Inputs: from commons
C     XNCOMP    R    Number of components to be divided.
C     DISKIN    R    Input file disk number.
C     CNOIN     I    Input file catalog number.
C     DISK2     R    CLEAN file disk number.
C     XNMAP     R    Number of model files.
C     CCTVER    I    CC table version number.
C     NAME2     C    Cleaned map name (name)
C     CLAS2     C    Cleaned map name (class)
C     SMODEL(1) R    Flux of a point model
C   Output:
C     CNOIN2    I    CLEAN file catalog number.
C     IRET      I    Return code, 0 => ok, otherwise not.
C-----------------------------------------------------------------------
      INTEGER   IRET
      INTEGER   MODEL, METHOD, ISTOKE, DISKO, ISCR, CHAN, NCHAN, I, IIF,
     *   IROUND
      LOGICAL   DOMSG, F, NONAM, WASOME
      INCLUDE 'CALIB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INTEGER   BITER(MAXFLD)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSCD.INC'
      REAL RBUF(MAXIF)
      DATA DOMSG, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check for correct request
      IF (SINGLE) NSOUWD = 1
      NONAM = (NAME2.EQ.' ') .OR. (CLAS2.EQ.' ')
C                                       multi-source request
      IF (NSOUWD.NE.1) THEN
         IF (.NOT.NONAM) THEN
            MSGTXT = 'A CLEAN MODEL WAS REQESTED FOR MULTIPLE SOURCES'
            CALL MSGWRT (8)
            IRET = 5
            GO TO 999
         ELSE
            GO TO 300
            END IF
C                                       single source request
      ELSE IF (SINGLE) THEN
         IF ((NONAM) .AND. (SMODEL(1).LE.1.0E-20)) THEN
            MSGTXT = 'SINGLE-SOURCE REQUIRES CLEAN OR SMODEL MODEL'
            CALL MSGWRT (8)
            IRET = 5
            GO TO 999
         ELSE IF ((.NOT.NONAM) .AND. (SMODEL(1).GT.1.E-20)) THEN
            MSGTXT = 'BOTH CLEAN AND SMODEL SPECIFIED FOR SINGLE-SOURCE'
     *         // ' FILE'
            CALL MSGWRT (8)
            IRET = 5
            GO TO 999
            END IF
C                                       multi-source file, no model
      ELSE IF (NONAM) THEN
         GO TO 300
      ELSE
         SMODEL(1) = 0.0
         END IF
C                                       Get  modeling method
      METHOD = 0
      IF (CMETH.EQ.'DFT ') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
C                                       Get  model type
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
      IF (CMOD.EQ.'SUBI') MODEL = 3
C                                       If Model required
C                                       then model must be specified
      IF (((CMETH.EQ.'GRID') .OR. (CMOD.EQ.'IMAG')) .AND. (NONAM)) THEN
         IRET = 5
         MSGTXT = 'YOU MUST SPECIFY A MODEL IMAGE IF CMETH=''GRID'''
     *      // 'OR CMOD=''IMAG'''
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Point source parameters
      DOPTMD = NONAM .AND. (ABS (SMODEL(1)).GT.1.0E-20)
      PTFLX = SMODEL(1)
      PTRAOF = SMODEL(2)
      PTDCOF = SMODEL(3)
      PARMOD(1) = SMODEL(4)
      PARMOD(2) = SMODEL(5)
      PARMOD(3) = SMODEL(6)
      PARMOD(4) = SMODEL(7)
C                                       Get info on model file(s)
      LIMFLX = XFLUX
      MFIELD = MAX (IROUND (XNMAP), 1)
      IF (DOPTMD) MFIELD = 0
      NONEG = F
      WASOME = F
      DO 10 I = 1,MFIELD
         BITER(I) = 1
         IF (I.LE.MAXAFL) THEN
            NCOMP(I) = ABS (XNCOMP(I)) + 0.1
            IF (XNCOMP(I).LE.-0.5) NONEG = .TRUE.
            IF (NCOMP(I).GT.0) WASOME = .TRUE.
         ELSE
            NCOMP(I) = 0
            IF (WASOME) NCOMP(I) = 1000000000
            END IF
 10      CONTINUE
C                                       If using CCs, check comps.
      FACGRD(1) = 1.0
      IF (DOPTMD) THEN
         DO3DIM = .FALSE.
         MSGTXT = 'Doing self-cal mode with point model'
         CALL MSGWRT (3)
         MODEL = 0
      ELSE
         IF (MODEL.EQ.3) THEN
            MSGTXT = 'Using sub-images for the source model'
         ELSE IF (MODEL.EQ.2) THEN
            MSGTXT = 'Using images for the self-cal source model'
         ELSE
            MSGTXT = 'Using Clean Component self-cal source model'
            END IF
         CALL MSGWRT (3)
         CALL SETGDS (DISKIN, CNOIN, NAME2, CLAS2, SEQ2, DISK2, MFIELD,
     *      CCTVER, NCOMP, BITER, MODEL, METHOD, BUFF1, BUFF2, ISTOKE,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IF (MODEL.GT.0) THEN
            CALL FACSET (DISKIN, CNOIN, 1, SOUWAN(1), MODEL, 1.0, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
      XVER = CCTVER
      CNOIN2 = CCCNO(1)
C                                       Divide data by model
      DISKO = VISDSK
      ISCR = VISCNO
C                                       Set division parameters
      FACGRD(2) = 1.0
      COMPDT = .FALSE.
      DATDIV = .TRUE.
C                                       Consider whether to process
C                                       1 IF at a time
      IF ((NUMIF.GT.1) .AND. (MODEL.EQ.1) .AND.
     *   (ABS(FACGRD(1)-1.0).GT.1.E-10)) THEN
C                                       number of channels
         NCHAN = CATBLK(KINAX+JLOCF)
C                                       For each IF
         DO 15 IIF = 1,NUMIF
C                                       Already know IF 1 scale
            IF (IIF.GT.1) THEN
C                                       Reset Components for div
               IF (MFIELD.GT.0) THEN
                  DO 12 I = 1,MFIELD
                     BITER(I) = 1
                     IF (I.LE.MAXAFL) THEN
                        NCOMP(I) = ABS (XNCOMP(I)) + 0.1
                     ELSE
                        NCOMP(I) = 0
                        IF (WASOME) NCOMP(I) = 1000000000
                        END IF
 12                  CONTINUE
                  CALL SETGDS (DISKIN, CNOIN, NAME2, CLAS2, SEQ2, DISK2,
     *               MFIELD, CCTVER, NCOMP, BITER, MODEL, METHOD, BUFF1,
     *               BUFF2, ISTOKE, IRET)
                  IF (IRET.NE.0) GO TO 999
                  XVER = VER
                  CNOIN2 = CCCNO(1)
                  END IF
C                                       Divide data by model
               DISKO = VISDSK
               ISCR = VISCNO
C                                       Set division parameters
               COMPDT = .FALSE.
               DATDIV = .TRUE.
               FACGRD(1) = 1.0
               IF (MODEL.GT.0) THEN
                  CALL FACSET (DISKIN, CNOIN, IIF, SOUWAN(1), MODEL,
     *               1.0, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
               END IF
C                                       start channel
            CHAN = 1 + (NCHAN * (IIF-1))
C                                       Divide 1 IF by model
            CALL UVMDIV (VISDSK, VISCNO, DISKO, ISCR, MODEL, METHOD,
     *         DOMSG, CHAN, NCHAN, CATBLK, JBUFSZ, FRQSEL, BUFF1,
     *         BUFF2, UBUFF, RBUF, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL UNSETG (BUFF2)
            DOMSG  = .FALSE.
 15         CONTINUE
C                                       else processing all IFs
      ELSE
         CHAN = 1
         NCHAN = CATBLK(KINAX+JLOCF) * NUMIF
C                                       Div all vis by model
         CALL UVMDIV (VISDSK, VISCNO, DISKO, ISCR, MODEL, METHOD, DOMSG,
     *      CHAN, NCHAN, CATBLK, JBUFSZ, FRQSEL, BUFF1, BUFF2, UBUFF,
     *      RBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (.NOT.DOPTMD) CALL UNSETG (BUFF2)
         END IF
C                                       Get true values of NCOMP
      DO 20 I = 1,MFIELD
         NCOMP(I) = MAX (NSUBG(I), 1) - 1
C                                       Tell user about CCs
         IF (LIMFLX.GT.0.0) THEN
            IF (NONEG) THEN
               WRITE (MSGTXT,1000) I, NCOMP(I), LIMFLX
            ELSE
               WRITE (MSGTXT,1010) I, NCOMP(I), LIMFLX
               END IF
         ELSE
            IF (NONEG) THEN
               WRITE (MSGTXT,1100) I, NCOMP(I)
            ELSE
               WRITE (MSGTXT,1110) I, NCOMP(I)
               END IF
            END IF
         CALL MSGWRT(3)
 20      CONTINUE
C                                       Model divided by data now
C                                       in scratch file.
      VISDSK = 0
      VISCNO = ISCR
      GO TO 999
C                                       Multiple sources, use point
C                                       source at phase center only.
 300  MSGTXT = 'Doing cal transfer mode with point model for each' //
     *   ' source'
      CALL MSGWRT (3)
      MSGTXT = 'This is not self-calibration'
      CALL MSGWRT (3)
      CALL CLBDIV (IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Field',I5,' used',I8,' CCs before 1st neg with flux>',
     *   F9.6)
 1010 FORMAT ('Field',I5,' used',I8,' CCs with flux>',F9.6)
 1100 FORMAT ('Field',I5,' used',I8,' CCs before 1st negative')
 1110 FORMAT ('Field',I5,' used',I8,' CCs')
      END
      SUBROUTINE CLBDIV (IRET)
C-----------------------------------------------------------------------
C   CLBDIV divides multisource data in a scratch file  by the
C   calibrator flux densities given in the source table; if 0, 1.0 is
C   used.  If all calibrator flux densities are 1.0 then no operation
C   is performed.
C   Input from common:
C    NSOUWD        I    Number of sources included or excluded; if
C                       0 all sources are included.
C    DOSWNT        L    If .TRUE. then sources in SOUWAN are included
C                       If .FALSE. then excluded.
C    SOUWAN(30)    I    The source numbers of sources included or
C                       excluded.
C    DISKIN        I    Disk number of the input multisource data file
C                       whose SU table is to be used.
C    CNOIN         I    Catalog slot number for SU file.
C    VISCNO        I    /CFILES/ number of the scratch file
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C   Note: also uses buffers, BUFF1, BUFF2, UBUFF, NXBUFF
C   Note: assumes that IF  is the most slowly variable axis.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LENVIS, BO,
     *   ILENBU, KBIND, IBIND, I, J, IVIS, IOFF, NOVIS, VO, ISURNO,
     *   SUKOLS(MAXSUC), SUNUMV(MAXSUC), IDSOU, QUAL, SULUN, IFNO, INDX,
     *   NVPIF, NUMSOU, LOOP, SUFQID, NUMSTK, ISTKNO
      LOGICAL   T, F, DOIT
      REAL      XSFLUX, RRFLUX(32768), LLFLUX(32768), RLFLUX(32768),
     *   LRFLUX(32768), PFLUX(32768,4)
      CHARACTER IFILE*48
      DOUBLE PRECISION BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'CALIB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      DOUBLE PRECISION LSRVEL(MAXIF), RESTFQ(MAXIF), FREQO(MAXIF)
      REAL     FLUX(4,MAXIF)
      EQUIVALENCE (PFLUX(1,1), RRFLUX), (PFLUX(1,2), LLFLUX)
      EQUIVALENCE (PFLUX(1,3), RLFLUX), (PFLUX(1,4), LRFLUX)
      DATA SULUN, LUNI, LUNO /27, 16,17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (SOUWAN(1).LE.0) SOUWAN(1) = 1
C                                       Message
      MSGTXT = 'Dividing data by source flux densities'
      CALL MSGWRT (2)
C                                       UVGET forces: complex, stokes
C                                       freq, IF axis order
      LENVIS = CATBLK(KINAX)
      NUMSTK = CATBLK(KINAX+1)
C                                       Open source (SU) table
      CALL SOUINI ('READ', NXBUFF, DISKIN, CNOIN, 1, CATUV,
     *   SULUN, NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Get number of sources
      NUMSOU = NXBUFF(5)
C                                       Read flux array
      DOIT = F
      DO 30 LOOP = 1,NUMSOU
         ISURNO = LOOP
         CALL TABSOU ('READ', NXBUFF, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *      PMRA, PMDEC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Get flux densities
         INDX = (IDSOU-1) * NUMIF
         DO 25 I = 1,NUMIF
            IF (FLUX(1,I).LE.1.0E-10) FLUX(1,I) = 1.0
            DOIT = DOIT .OR. (ABS (FLUX(1,I)-1.0).GT.1.0E-10)
            IF (FLUX(1,I).NE.FLUX(4,I)) THEN
               RRFLUX(INDX+I) = 1.0 / (FLUX(1,I) + FLUX(4,I))
               LLFLUX(INDX+I) = 1.0 / (FLUX(1,I) - FLUX(4,I))
            ELSE
               RRFLUX(INDX+I) = 1.0 / FLUX(1,I)
               LLFLUX(INDX+I) = 1.0 / FLUX(1,I)
               WRITE (MSGTXT,1025) LOOP, I
               CALL MSGWRT (8)
               END IF
            RLFLUX(INDX+I) = 1.0 / FLUX(1,I)
            LRFLUX(INDX+I) = 1.0 / FLUX(1,I)
 25         CONTINUE
 30      CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ISURNO, BUFF1, NXBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                       Check if work to be done.
      IF (.NOT.DOIT) GO TO 999
C                                       Do divisions
      NOVIS = (LREC - NRPARM) / LENVIS
      NVPIF = NOVIS / NUMIF
      IF (NVPIF.LT.1) NVPIF = 1
C                                       Open and init for write
C                                       visibility file
      CALL ZPHFIL ('SC', SCRVOL(VISCNO), SCRCNO(VISCNO), 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, SCRVOL(VISCNO), IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1040) IRET, 'WRIT'
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IPTRO = KBIND
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET, 'WRIT'
         GO TO 990
         END IF
C                                       Open and init for read
C                                       visibility file
      CALL ZOPEN (LUNI, INDI, SCRVOL(VISCNO), IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1040) IRET, 'READ'
         GO TO 990
         END IF
C                                       Init vis file for read
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET, 'READ'
         GO TO 990
         END IF
C                                       Loop
C                                       Read vis. record.
 100  CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET, 'READ'
            GO TO 990
            END IF
         IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
C                                       loop thru buffer full
         DO 180 I = 1,INIO
            DO 120 J = 1,LREC
               BUFF2(IPTRO+J-1) = BUFF1(IPTRI+J-1)
 120           CONTINUE
C                                       Trap single source
            IDSOU = SOUWAN(1)
            IF (ILOCSU.GE.0) IDSOU = BUFF2(IPTRO+ILOCSU) + 0.5
            IOFF = NRPARM
C                                       This assumes that IF is axis 4 as
C                                       is forced by UVGET
            DO 140 IVIS = 1,NOVIS
               ISTKNO = MOD (IVIS-1, NUMSTK) + 1
               IFNO = ((IVIS-1) / NVPIF) + 1
               INDX = (IDSOU-1) * NUMIF + IFNO
               XSFLUX = PFLUX(INDX,ISTKNO)
               BUFF2(IPTRO+IOFF) = BUFF2(IPTRO+IOFF) * XSFLUX
               BUFF2(IPTRO+IOFF+1) = BUFF2(IPTRO+IOFF+1) * XSFLUX
               IOFF = IOFF + LENVIS
 140           CONTINUE
            IPTRI = IPTRI + LREC
            IPTRO = IPTRO + LREC
 180        CONTINUE
C                                       Write vis. record.
         CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, INIO, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET, 'WRIT'
            GO TO 990
            END IF
         IPTRO = KBIND
         GO TO 100
C                                       Done
C                                       Flush buffer
 200  INIO = 0
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, INIO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, 'FLSH'
         GO TO 990
         END IF
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1210) IRET, 'READ'
         GO TO 990
         END IF
      CALL ZCLOSE (LUNO, INDO, IRET)
      IF (IRET.EQ.0) GO TO 999
         WRITE (MSGTXT,1210) IRET, 'WRIT'
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLBDIV: ERROR',I3,' OPENING SOURCE TABLE')
 1010 FORMAT ('CLBDIV: ERROR',I3,' READING SOURCE TABLE')
 1025 FORMAT ('CLBDIV IGNORING EQUAL I AND V FLUX: SOURCE',I4,' IF',I3)
 1030 FORMAT ('CLBDIV: ERROR',I3,' CLOSING SOURCE TABLE')
 1040 FORMAT ('CLBDIV: ERROR',I3,' OPEN-FOR-',A4,' VIS FILE')
 1050 FORMAT ('CLBDIV: ERROR',I3,' INIT-FOR-',A4,' VIS FILE')
 1100 FORMAT ('CLBDIV: ERROR',I3,1X,A4,'ING VIS FILE')
 1210 FORMAT ('CLBDIV: ERROR',I3,'CLOSING ',A4,' VIS FILE')
      END
      SUBROUTINE CLBHIS
C-----------------------------------------------------------------------
C   CLBHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(7)*2, CTIME(2)*12, HILINE*72, WTCOD(4)*4
      INTEGER   LUN1, LUN2, LIMIT, LIMIT2, IERR, ITEMP, I, J, TIME(3),
     *   DATE(3), NONOT, I1, I2, SOLSUB, SOLMIN
      LOGICAL   T, ONEWTT
      COMMON /SOLCOM/ SOLSUB, SOLMIN
      INCLUDE 'CALIB.INC'
      INCLUDE 'CALFLAG'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA WTCOD /'1.00', '0.50', '0.25', '0.0'/
      DATA NONOT, NOTTYP /7,'SN','NX','CL','CH','BP','FQ','BL'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      IF ((SINGLE) .AND. (DOMODL) .AND. (APPLY.GE.0.0)) THEN
         CALL HISCOP (LUN1, LUN2, DISKIN, DISOUT, CNOIN, CNOOUT, CATBLK,
     *      BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1100) IERR
            CALL MSGWRT (6)
            GO TO 190
            END IF
C                                       New history
         CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISOUT, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Multisource - open old history
      ELSE
         CALL HIOPEN (LUN2, DISKIN, CNOIN, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Task message
         CALL ZDATE (DATE)
         CALL ZTIME (TIME)
         CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
         WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       calibration data
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       General information
C                                       Write control info.
C                                       Write CC tables if used
      IF ((DOMODL) .AND. (SMODEL(1).LE.0.0) .AND.
     *    ((NAME2.NE.' ') .OR. (CLAS2.NE.' '))) THEN
C                                       CC File Name etc.
         CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 190
C                                        CCfile version no.
         WRITE (HILINE,2001) TSKNAM, CCTVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                        Number of images
         WRITE (HILINE,2002) TSKNAM, MFIELD
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Number of CLEAN components.
         DO 140 I = 1,MFIELD
            WRITE (HILINE,2003) TSKNAM, I, NCOMP(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
 140     CONTINUE
         END IF
C                                       Soln. interval.
      XSOLIN = SOLINT * 24.0 * 60.0
      IF (XSOLIN.GT.9999.99) XSOLIN = 9999.99
      WRITE (HILINE,2010) TSKNAM, XSOLIN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Soln. sub and min interval.
      IF (SOLMIN.NE.0 .OR. SOLSUB.NE.0) THEN
         WRITE (HILINE,2024) TSKNAM, SOLSUB
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2026) TSKNAM, SOLMIN
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         ENDIF
C                                        Min. no. antennas
      WRITE (HILINE,2011) TSKNAM, MINNO
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Average RR,LL
      ITEMP = -1
      IF (AVGPOL) ITEMP = 1
      WRITE (HILINE,2013) TSKNAM, ITEMP
      IF (NCOR.GT.1) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Average freq in IF
      ITEMP = -1
      IF (AVGIF) ITEMP = 1
      WRITE (HILINE,2015) TSKNAM, ITEMP
      IF (EIF.GT.BIF) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        SNR cutoff.
      WRITE (HILINE,2016) TSKNAM, SNRMIN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Pass failed solutions?
      IF (DOPASS) THEN
         WRITE (HILINE,2017) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Point source model
      IF (ABS (SMODEL(1)).GT.0.0) THEN
         WRITE (HILINE,2020) TSKNAM, SMODEL(1), SMODEL(2), SMODEL(3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Other parameters
         WRITE (HILINE,2021) TSKNAM, SMODEL(4), SMODEL(5),
     *      SMODEL(6), SMODEL(7)
         IF (SMODEL(4).GT.0.01) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         END IF
      IF (IERR.NE.0) GO TO 190
C                                        Already divided by model
      WRITE (HILINE,2022) TSKNAM
      IF (APARM(2).GT.0.0) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Solution type
      WRITE (HILINE,2030) TSKNAM, XSOLTY
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Mode
      WRITE (HILINE,2031) TSKNAM, XSOLMO
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
      ITEMP = MODE / 10
      IF (ITEMP.GE.0) THEN
         WRITE (HILINE,2032) TSKNAM, ITEMP, WTCOD(ITEMP+1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Normalize gain?
      ITEMP = -1
      IF (DOMGM.GT.1) ITEMP = DOMGM-1
      WRITE (HILINE,2033) TSKNAM, ITEMP
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Scalar average
      IF (CPARM(5).GT.0.0) THEN
         I = MIN (MAX (1.0, CPARM(5)+0.5), 2.0) + 0.01
         IF (I.EQ.1) THEN
            WRITE (HILINE,3033) TSKNAM, I
         ELSE
            WRITE (HILINE,3034) TSKNAM, I
            END IF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Robust soln limit
      IF (CPARM(6).GT.2.0) THEN
         WRITE (HILINE,3035) TSKNAM, CPARM(6)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      WRITE (HILINE,3036) TSKNAM, CPARM(7)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Output SN table
      WRITE (HILINE,2062) TSKNAM, SNVER
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Gain constraint factor
      IF (DOGCON) THEN
         WRITE (HILINE,2034) TSKNAM, CONFAC
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                        Full weight annulus
      WRITE (HILINE,2035) TSKNAM, MNPABL, MXPABL
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        If ants outside annulus used
      IF (WTPABL.GT.0.0) THEN
         WRITE (HILINE,2037) TSKNAM, WTPABL
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Antennas
      IF (NANTSL.LE.0) THEN
         WRITE (HILINE,2038) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,2039) TSKNAM
         IF (DOAWNT) WRITE (HILINE,2040) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       1st 12 and label.
         LIMIT = MIN (12, NANTSL)
         WRITE (HILINE,2041) TSKNAM, (ANTENS(J),J=1,LIMIT)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         IF (NANTSL.GT.12) THEN
C                                       Rest of antennas
            DO 180 I = 13,NANTSL,12
               LIMIT = I
               LIMIT2 = I + 11
               LIMIT2 = MIN (NANTSL, LIMIT2)
               WRITE (HILINE,2042) TSKNAM, (ANTENS(J),J=LIMIT,LIMIT2)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
 180           CONTINUE
            END IF
         END IF
C
      IF (DOMGM.GT.1) THEN
         IF (ALZERO) THEN
            WRITE (HILINE,2044) TSKNAM
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
         ELSE
            WRITE (HILINE,2046) TSKNAM
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            LIMIT = MIN (15, NUSE)
            WRITE (HILINE,2048) TSKNAM, (LSTUSE(I), I=1,LIMIT)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            IF (NUSE.GT.15) THEN
C                                       Rest of antennas
               DO 181 I = 16, NUSE, 15
                  LIMIT = I
                  LIMIT2 = I + 14
                  LIMIT2 = MIN (NUSE, LIMIT2)
                  WRITE (HILINE,2048) TSKNAM, (LSTUSE(J),J=LIMIT,LIMIT2)
                  CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
  181             CONTINUE
               END IF
            END IF
C                                       Minimum elevation for the
C                                       gain normalization.
         WRITE (HILINE,2049) TSKNAM, GMINEL
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                        Antenna weights.
C                                        Are they all the same?
      ONEWTT = .TRUE.
      DO 182 I = 2, NANT
         IF (ANTWT(1).NE.ANTWT(I)) ONEWTT = .FALSE.
 182     CONTINUE
C                                        All ants have same weight
      IF (ONEWTT) THEN
         WRITE (HILINE,2052) TSKNAM, ANTWT(1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      ELSE
C                                        Else have different weights
         LIMIT2 = MIN(9, NANT)
         WRITE (HILINE,2050) TSKNAM, (ANTWT(I), I = 1,LIMIT2)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
C                                        for rest of antennas
         DO 184 J = 2,4
            LIMIT  = 1 + (J*9)
            LIMIT2 = MIN( 9*J, NANT)
C                                        If not out of antennas
            IF (LIMIT.LE.LIMIT2) THEN
               WRITE (HILINE,2051) TSKNAM, (ANTWT(I),I=LIMIT,LIMIT2)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               END IF
 184        CONTINUE
         END IF
      IF (IERR.NE.0) GO TO 190
C                                        GAINERR
      IF ((DOGCON) .AND. ((MOD(MODE,10).EQ.3) .OR.(MOD(MODE,10).EQ.7)))
     *   THEN
         I1 = 1
 185     I2 = I1 + 8
         I2 = MIN (I2, NANT)
         IF (I2.GE.I1) THEN
            IF (I1.EQ.1) THEN
               WRITE (HILINE,2060) TSKNAM, (GAERR(I), I = I1,I2)
            ELSE
               WRITE (HILINE,2061) TSKNAM, (GAERR(I), I = I1,I2)
               END IF
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            I1 = I2 + 1
            GO TO 185
            END IF
         END IF
      IF ((LBCHAN.GT.1) .OR. (LECHAN.GT.LBCHAN)) THEN
         DO 187 I = BIF,EIF
            DO 186 J = 1,20
               IF (CHNSEL(1,J,I).GT.0) THEN
                  CHNSEL(1,J,I) = CHNSEL(1,J,I) + LBCHAN - 1
                  CHNSEL(2,J,I) = CHNSEL(2,J,I) + LBCHAN - 1
                  WRITE (HILINE,2185) TSKNAM, J, I, CHNSEL(1,J,I),
     *               CHNSEL(2,J,I), CHNSEL(3,J,I)
                  CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  END IF
 186           CONTINUE
 187        CONTINUE
         END IF

C                                       Add solution summary.
C                                       If not all zero
      IF (TOTREC(1,1).NE.0 .OR. TOTREC(1,2).NE.0 .OR.
     *    TOTREC(1,3).NE.0 .OR. TOTREC(2,1).NE.0 .OR.
     *    TOTREC(2,2).NE.0 .OR. TOTREC(2,3).NE.0) THEN
         WRITE (HILINE, 3200) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE, 3201) TSKNAM, (TOTREC (1,I), I = 1,3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE, 3202) TSKNAM, (TOTREC (2,I), I = 1,3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       End if summary data present
         END IF
C
      IF (CNTOK.LE.0) THEN
C                                       Nothing worked
         WRITE (HILINE, 3210) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
      ELSE
         WRITE (HILINE, 3220) TSKNAM, CNTOK
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         IF (CNTBAD.GT.0) THEN
            WRITE (HILINE, 3221) TSKNAM, CNTBAD
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            END IF
         IF (CNTFEW.GT.0) THEN
            WRITE (HILINE, 3222) TSKNAM, CNTFEW
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            END IF
         IF (CNTSAD.GT.0) THEN
            WRITE (HILINE, 3223) TSKNAM, CNTSAD
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            END IF
         END IF
C                                       flags
      IF (FLAGIT) THEN
         WRITE (HILINE,3300) TSKNAM, FGVERO
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,3301) TSKNAM, NFLAGD
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Close HI file
 190  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       a little for old HI file
      IF ((SINGLE) .AND. (DOMODL) .AND. (APPLY.GE.0.0)) THEN
         CALL HIOPEN (LUN2, DISKIN, CNOIN, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Task message
         CALL ZDATE (DATE)
         CALL ZTIME (TIME)
         CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
         WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISOUT, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Output SN table
         WRITE (HILINE,2062) TSKNAM, SNVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables.
         CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISOUT,
     *      CNOIN, CNOOUT, CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1190)
            CALL MSGWRT (6)
            END IF
C                                        Update CATBLK.
         CALL CATIO ('UPDT', DISOUT, CNOOUT, CATBLK, 'REST', BUFF1,
     *      IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE   =''',A7,' ''  /******* Start ',A12,2X,A8)
 1100 FORMAT ('CLBHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1190 FORMAT ('CLBHIS: ERROR COPYING TABLES')
 2001 FORMAT (A6,'INVER    =',I5,' /CC file version no.')
 2002 FORMAT (A6,'NMAPS    =',I5,' /Number of clean images used')
 2003 FORMAT (A6,'NCOMP(',I2,')=',I8,' /Number of clean comps.')
 2010 FORMAT (A6,'SOLINT   =',F8.2,' /Soln. inter. (min)')
 2011 FORMAT (A6,'APARM(1) =',I5,' /Min. no antennas')
 2013 FORMAT (A6,'APARM(3) =',I5,' />0 => avg. RR,LL')
 2015 FORMAT (A6,'APARM(5) =',I5,' />0 => average IFs')
 2016 FORMAT (A6,'APARM(7) =',F5.1,' /SNR cutoff')
 2017 FORMAT (A6,'APARM(9) =  1.0 /Pass failed solutions')
 2020 FORMAT (A6,'SMODEL   = ',2(F10.3,','),F10.3,
     *   ' /Pt. model parameters')
 2021 FORMAT (A6,'        ',4F10.3,' /Other parms.')
 2022 FORMAT (A6,'APARM(2) =    1 /Data already divided by model')
 2024 FORMAT (A6,'SOLSUB =',I5,' /Soln. subinterval')
 2026 FORMAT (A6,'SOLMIN =',I5,' /Minimum soln. int. (min)')
 2030 FORMAT (A6,'SOLTYPE  =''',A4,'''/Solution type')
 2031 FORMAT (A6,'SOLMODE  =''',A4,'''/Solution mode')
 2032 FORMAT (A6,'WEIGHTIT =',I2,' / fit wts = wt**',A)
 2033 FORMAT (A6,'NORMALIZ =',I5,' />=1 => Normalize gain')
 3033 FORMAT (A6,'CPARM(5) =',I5,' /1 => Scalar average in time only')
 3034 FORMAT (A6,'CPARM(5) =',I5,' /2 => Scalar average everything')
 3035 FORMAT (A6,'CPARM(6) =',F5.2,' /limit robust discards')
 3036 FORMAT (A6,'CPARM(7) =',F6.3,' /limit closure displays')
 2034 FORMAT (A6,'SOLCON   =   ',1PE10.3,' /Gain constraint factor')
 2035 FORMAT (A6,'UVRANGE  =   ',1PE10.3,',',1E10.3,
     *           ' /Min, Max Baseline')
 2037 FORMAT (A6,'WTUV     =   ',1PE10.3,' /Weight outside annulus')
 2038 FORMAT (A6,'ANTENNAS =    0 /All antennas selected')
 2039 FORMAT (A6,'/Antennas excluded:')
 2040 FORMAT (A6,'/Antennas included:')
 2041 FORMAT (A6,'ANTENNAS =',12(I3,' '))
 2042 FORMAT (A6,'          ',12(I3,' '))
 2044 FORMAT (A6,'ANTUSE = 0 /All antennas were used in the gain',
     *   ' normalization')
 2046 FORMAT (A6,'/The following antennas were used in the gain',
     *   ' normalization')
 2048 FORMAT (A6, 15I3)
 2049 FORMAT (A6,'/Minimum elevation for gain normalization:',F7.2 )
 2050 FORMAT (A6,'ANTWT  =',9F5.1,'/Ant. wt')
 2051 FORMAT (A6,'        ',9F5.1)
 2052 FORMAT (A6,'ANTWT    =',F5.1,' /Weight same for all Ant')
 2060 FORMAT (A6,'GAINERR=',9F5.2)
 2061 FORMAT (A6,'        ',9F5.2)
 2062 FORMAT (A6,'SNVER    =',I5,' /Output SN table version')
 2185 FORMAT (A6,'CHANSEL(,',I2,',',I2,') =',I5,',',I5,',',I2)
 3200 FORMAT (A6,'/SUMMARY: ','  Previously flagged ',
     *   '    Flagged by gain   ','   Kept')
 3201 FORMAT (A6,'/Partially ',2I20,I10)
 3202 FORMAT (A6,'/Fully     ',2I20,I10)
 3210 FORMAT (A6,'/No valid solutions found!')
 3220 FORMAT (A6,'/Found     ',I8,' good solutions')
 3221 FORMAT (A6,'/Failed on ',I8,' solutions')
 3222 FORMAT (A6,'/          ',I8,' solutions had insufficient data')
 3223 FORMAT (A6,'/          ',I8,' solutions had no data')
 3300 FORMAT (A6,'FLAGVERO =',I5,' /Output flag table version')
 3301 FORMAT (A6,'NFLAGD   =',I8,' /Number flags added to FG table')
      END
      SUBROUTINE CLBADJ (IRET)
C-----------------------------------------------------------------------
C   CLBADJ massages the solutions so that interpolation between points
C   is reasonable.
C   Output:
C   IRET   I     Return error code. 0 => OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER KEYS(18)*24
      HOLLERITH CATUVH(256)
      INTEGER   ANT, REF, KOLS(18), LUN, LOOP, NKEY, LKEY, IFLOOP,
     *   KEY(2,2), IPNT, IIVER, NIF, IERR, MXINDX, REFTMP, NUMSUB,
     *   LIMS1, LIMS2, LOOPSA, KEYSUB(2,2)
      LOGICAL   T, ISAPPL, DOIT
      INTEGER   ISNRNO, MXCNT, NUMROW, NWORDS
      LONGINT   OFFSET
      REAL      WORK(2), SMOTIM(3), FKEY(2,2)
      LOGICAL   NOTREF
      INCLUDE 'CALIB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DOUBLE PRECISION FOFF(MAXIF), FREQIF
      EQUIVALENCE (CATUVH, CATUV)
      DATA NKEY, LKEY /9,24/
      DATA T /.TRUE./
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA KEYS /'ANTENNA NO.             ',
     *   'REFANT 1                ', 'SUBARRAY                ',
     *   'WEIGHT 1                ', 'TIME                    ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'DELAY 1                 ', 'RATE 1                  ',
     *   'ANTENNA NO.             ',
     *   'REFANT 2                ', 'SUBARRAY                ',
     *   'WEIGHT 2                ', 'TIME                    ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'DELAY 2                 ', 'RATE 2                  '/
C-----------------------------------------------------------------------
C                                       Initialize OFFSET; if OFFSET = 0
C                                       then workspace has not been
C                                       allocated
      OFFSET = 0
C                                       NOTREF indicates that some
C                                       antenna solutions were not
C                                       adjusted.
      NOTREF = .FALSE.
C                                       See if any work to be done.
      DOIT = .FALSE.
      MXCNT = 0
      DO 5 LOOP = 1,NUMANT
         DOIT = DOIT .OR. ((REFUSE(LOOP).GT.0) .AND. (LOOP.NE.REFANT))
         IF (REFUSE(LOOP).GT.MXCNT) MXINDX = LOOP
         IF (REFUSE(LOOP).GT.MXCNT) MXCNT = REFUSE(LOOP)
 5       CONTINUE
      IF (.NOT.DOIT) GO TO 999
C                                       Message
      WRITE (MSGTXT,2000)
      CALL MSGWRT (2)
C                                       If no REFANT specified pick the
C                                       one with the most solutions.
      REFTMP = REFANT
      IF (REFTMP.LE.0) REFTMP = MXINDX
      LUN = 29
C                                       Get IF frequencies
      FOFF(1) = 0.0
      IIVER = 1
      IF (EIF.GT.1)
     *   CALL CHNDAT ('READ', CLBUFF, DISKIN, CNOIN, IIVER, CATIN, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
C                                       Open solution table
      CALL SNINI ('READ', CLBUFF, DISKIN, CNOIN, SNVER, CATIN, LUN,
     *   ISNRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) GO TO 990
C
      NUMROW = CLBUFF(5)
C                                       Set column pointers for sort
      CALL FNDCOL (NKEY, KEYS, LKEY, T, CLBUFF, KOLS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 980
         END IF
C                                       Close
      CALL TABIO ('CLOS', 0, ISNRNO, WORK, CLBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 980
         END IF
C                                       Allocate workspace for CALREF.
C                                       If each of the 5 scratch arrays
C                                       has NUMROW then CALREF is
C                                       guaranteed to have enough space
C                                       to re-refence the table. In most
C                                       cases this will be less than a
C                                       megabyte of data so the
C                                       allocation should rarely fail.
C                                       A single array is allocated and
C                                       divided up for the CALREF calls.
      NWORDS = (5 * NUMROW - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'CLBADJ', NWORDS, WORK, OFFSET, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'Could not allocate enough workspace to reference'
         CALL MSGWRT (6)
         MSGTXT = 'solutions to a common antenna. Solutions will not'
         CALL MSGWRT (6)
         MSGTXT = 'be adjusted. If this is not acceptable then either'
         CALL MSGWRT (6)
         MSGTXT = 'increase SOLINT or shutdown some applications and'
         CALL MSGWRT (6)
         MSGTXT = 'run CALIB again.'
         IRET = 0
         GO TO 999
         END IF
C                                       Sort to time-ant order.
      KEY(1,1) = KOLS(5)
      KEY(2,1) = KOLS(5)
      KEY(1,2) = KOLS(1)
      KEY(2,2) = KOLS(1)
      CALL TABSRT (DISKIN, CNOIN, 'SN', SNVER, SNVER, KEY, KEYSUB, FKEY,
     *   CLBUFF, CATIN, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 980
         END IF
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATUV, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
      LIMS1 = 1
      LIMS2 = NUMSUB
      IF (SUBARR.GT.0) LIMS1 = SUBARR
      IF (SUBARR.GT.0) LIMS2 = SUBARR
C                                       Open for write
      CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, CATIN, LUN,
     *   ISNRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Set column pointers
      DO 40 LOOP = 1,9
         IPNT = KOLS(LOOP)
         KOLS(LOOP) = CLKOLS(IPNT)
 40      CONTINUE
      IF (NUMPOL.GT.1) THEN
C                                       Second Stokes'
         CALL FNDCOL (NKEY, KEYS(10), LKEY, T, CLBUFF, KOLS(10), IRET)
         IF (IRET.EQ.0) GO TO 50
         WRITE (MSGTXT,1000) IRET
         GO TO 980
 50      DO 60 LOOP = 10,18
            IPNT = KOLS(LOOP)
            KOLS(LOOP) = CLKOLS(IPNT)
 60         CONTINUE
         END IF
C                                       Smoothing times
      SMOTIM(1) = 1.0E-6
      SMOTIM(2) = 1.0E-6
      SMOTIM(3) = 1.0E-6
C                                       Loop over IFs
      DO 200 IFLOOP = 1,NUMIF
         FREQIF = FREQ + FOFF(IFLOOP-BIF+1)
C                                       Loop over subarrays
         DO 100 LOOPSA = LIMS1,LIMS2
C                                       Loop over reference antennas
C                                       used.
            REF = REFTMP
            DO 100 LOOP = 1,NUMANT
               IF ((REFUSS(LOOP,LOOPSA).GT.0) .AND. (LOOP.NE.REF)) THEN
                  ANT = LOOP
C                                       First Stokes'
                  CALL CALREF (ANT, REF, LOOPSA, KOLS(1), FREQIF,
     *               SMOTIM, NUMROW, CLBUFF, WORK(OFFSET + 1),
     *               WORK(OFFSET + NUMROW + 1),
     *               WORK(OFFSET + 2 * NUMROW + 1),
     *               WORK(OFFSET + 3 * NUMROW + 1),
     *               WORK(OFFSET + 4 * NUMROW + 1), IRET)
C                                       Inability to connect ANT to
C                                       REF is not necessarily an
C                                       error
                  IF (IRET.EQ.1) THEN
                     IRET = 0
                     NOTREF = .TRUE.
                  END IF
                  IF (IRET.GT.0) GO TO 990
                  IF (NUMPOL.LE.1) GO TO 100
C                                       Second Stokes'
                  CALL CALREF (ANT, REF, LOOPSA, KOLS(10), FREQIF,
     *               SMOTIM, NUMROW, CLBUFF, WORK(OFFSET + 1),
     *               WORK(OFFSET + NUMROW + 1),
     *               WORK(OFFSET + 2 * NUMROW + 1),
     *               WORK(OFFSET + 3 * NUMROW + 1),
     *               WORK(OFFSET + 4 * NUMROW + 1), IRET)
C                                       Inability to connect ANT to
C                                       REF is not necessarily an
C                                       error
                  IF (IRET.EQ.1) THEN
                     IRET = 0
                     NOTREF = .TRUE.
                  END IF
                  IF (IRET.GT.0) GO TO 990
                  END IF
 100           CONTINUE
C                                       Update column pointers for IF
         KOLS(2) = KOLS(2) + 1
         KOLS(4) = KOLS(4) + 1
         KOLS(6) = KOLS(6) + 1
         KOLS(7) = KOLS(7) + 1
         KOLS(8) = KOLS(8) + 1
         KOLS(9) = KOLS(9) + 1
         KOLS(11) = KOLS(11) + 1
         KOLS(13) = KOLS(13) + 1
         KOLS(15) = KOLS(15) + 1
         KOLS(16) = KOLS(16) + 1
         KOLS(17) = KOLS(17) + 1
         KOLS(18) = KOLS(18) + 1
 200     CONTINUE
C                                       Warn user if re-referencing was
C                                       partially successful
      IF (NOTREF) THEN
         MSGTXT = 'Some solutions were not adjusted to the common'
         CALL MSGWRT (6)
         MSGTXT = 'reference antenna. This may be because one or more'
         CALL MSGWRT (6)
         MSGTXT = 'antennas were not used. Check your data before'
         CALL MSGWRT (6)
         MSGTXT = 'proceeding.'
         CALL MSGWRT (6)
      END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, WORK, CLBUFF, IRET)
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
C
 999  CONTINUE
C                                       De-allocate any dynamic
C                                       workspace. Ignore errors.
      IF (OFFSET.NE.0) THEN
         CALL ZMEMRY ('FREE', 'CLBADJ', 5 * NUMROW, WORK, OFFSET, IERR)
         END IF
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I5,' FINDING SN TABLE COLUMNS')
 1010 FORMAT ('TABIO ERROR ',I5,' CLOSING SN TABLE')
 1020 FORMAT ('TABSRT ERROR ',I5,' SORTING SN TABLE')
 1990 FORMAT ('ERROR OCCURED IN CLBADJ')
 2000 FORMAT ('Adjusting solutions to a common reference antenna')
      END
      SUBROUTINE CLBAPL (IRET)
C-----------------------------------------------------------------------
C   CLBAPL corrects single source data files.
C   Input from common:
C      DISOUT I     Output disk number.
C      CNOOUT I     Output catalog slot number.
C      INCF   I     Increment in freq. of data from UVGET
C      INCIF  I     Increment in IF of data from UVGET
C      INCS   I     Increment in Stokes' of data from UVGET
C      JLOCF  I     Offset of freq. of data from UVGET
C      JLOCIF I     Offset of IF of data from UVGET
C      JLOCS  I     Offset of Stokes' of data from UVGET
C   Output:
C      IRET   I     Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER NAME*48
      HOLLERITH CATH(256), CATUVH(256)
      INTEGER   LUN, FIND, BIND, LENBU, NIO, JNCIF, JNCS, LRECO, NUMFRQ,
     *   NUMSUB, SUB, LIMS1, LIMS2, SUBTMP, LUN1, LUN2, NUMPRM, BO, I,
     *   XCOUNT, IIVER, OOVER, NUMVIS, OLDVIS, JERR,MMVIS, MMCOR, WTOFF,
     *   RNXRET
      LOGICAL   T, F, DOCMP
      REAL      DUM
      INCLUDE 'CALIB.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      REAL    VIS(XBFSZ), VIS2(XBFSZ)
      DOUBLE PRECISION FOFF(MAXIF)
      EQUIVALENCE (CATBLK, CATH),  (CATUV, CATUVH)
      EQUIVALENCE (BUFF2, VIS)
      EQUIVALENCE (BUFFS, VIS2)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, LUN1, LUN2, BO /27,27,28,1/
C-----------------------------------------------------------------------
C                                       Message
      WRITE (MSGTXT,2000)
      CALL MSGWRT (2)
C                                       set DSEL parameters
      DOCAL = T
      DOWTCL = (XDOCAL.LE.99.0) .AND. (APARM(10).LE.99.0)
      CLUSE = SNVER
      CLVER = SNVER
      STOKES = '    '
C                                       Zero flag counts
      TOTREC(1,1) = 0
      TOTREC(2,1) = 0
      TOTREC(1,2) = 0
      TOTREC(2,2) = 0
      TOTREC(1,3) = 0
      TOTREC(2,3) = 0
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATUV, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
      LIMS1 = 1
      LIMS2 = NUMSUB
      IF (SUBARR.GT.0) LIMS1 = SUBARR
      IF (SUBARR.GT.0) LIMS2 = SUBARR
      SUBTMP = SUBARR
      NUMVIS = 0
C                                       Get output CATBLK from DISK
      MSGSUP = 32000
      CALL CATIO ('READ', DISOUT, CNOOUT, CATBLK, 'REST', BUFF1, IRET)
      MSGSUP = 0
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      CALL UVPGET (JERR)
C                                       Set lengths of input axes.
      OLDVIS = CATBLK(KIGCN)
      NUMFRQ = CATBLK(KINAX+JLOCF)
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      NUMPOL = CATBLK(KINAX+JLOCS)
C                                       Set output increments
C                                       (averaging)
      JNCIF = INCIF
      IF (JLOCF.LT.JLOCIF) JNCIF = INCIF / NUMFRQ
      JNCS = INCS
      IF (JLOCF.LT.JLOCS) JNCS = INCS / NUMFRQ
C                                       Compressed data?
      DOCMP = CATBLK(KINAX).EQ.1
      NUMPRM = NRPARM
      IF (DOCMP) THEN
         MMVIS = LREC - NUMPRM
         MMCOR = LREC - NUMPRM
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), WTOFF,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            IRET = 9
            GO TO 990
            END IF
      ELSE
C                                       Un compressed
         MMVIS = LREC - NUMPRM
         MMCOR = (LREC - NUMPRM) / 3
         WTOFF = 0
         END IF
C                                       make an index table
      CALL RNXGET (DISKIN, CNOIN, CATIN)
      CALL RNXINI (DISOUT, CNOOUT, CATBLK, RNXRET)
C                                       Set output file name
      CALL ZPHFIL ('UV', DISOUT, CNOOUT, 1, NAME, IRET)
C                                       Open output file.
      CALL ZOPEN (LUN, FIND, DISOUT, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      LENBU = 1
      LRECO = LREC
      CALL UVINIT ('WRIT', LUN, FIND, CATBLK(KIGCN), NUMVIS, LRECO,
     *   LENBU, JBUFSZ, BUFF1, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                       Remove channel selection
C                                       constraints
      BCHAN = 0
      ECHAN = 0
C                                       Loop over subarray
      XCOUNT = 0
      DO 200 SUB = LIMS1,LIMS2
         SUBARR = SUB
C                                       Setup
C                                       Save CATBLK
         CALL COPY (256, CATBLK, BUFF2)
         CALL UVGET ('INIT', DUM, DUM, IRET)
C                                       Restore CATBLK
         CALL COPY (256, BUFF2, CATBLK)
C                                       Check buffer size for use of
C                                       VIS, VIS2
         IF ((LREC.GT.XBFSZ) .AND. DOCMP) THEN
            MSGTXT = 'CLBAPL: BUFFER SIZE TOO SMALL FOR VISIBILITY ' //
     *         ' RECORD'
            IRET = 1
            GO TO 990
            END IF
         IF (IRET.GT.0) GO TO 999
         IF ((NVIS.LE.0) .OR. (IRET.LT.0)) THEN
            NVIS = 0
            GO TO 120
            END IF
C                                       If multiple subarrays mark
C                                       unsorted
         IF (NUMVIS.GT.0) CALL CHR2H (2, '**', 1, CATH(KITYP))
C                                       Copy file
         DO 100 I = 1,NVIS
C                                       Write/compress to output buffer
            IF (DOCMP) THEN
               CALL UVGET ('READ', BUFF1(BIND), VIS2, IRET)
               CALL ZUVPAK (MMCOR, VIS2, BUFF1(BIND+WTOFF),
     *            BUFF1(BIND+NUMPRM))
            ELSE
               CALL UVGET ('READ', BUFF1(BIND), BUFF1(BIND+NUMPRM),
     *            IRET)
               END IF
            IF (IRET.LT.0) GO TO 120
            IF (IRET.NE.0) GO TO 999
C                                       update NX table
            CALL RNXUPD (BUFF1(BIND), RNXRET)
C                                       Write new
            NIO = 1
            XCOUNT = XCOUNT + 1
            CALL UVDISK ('WRIT', LUN, FIND, BUFF1, NIO, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1090) IRET
               GO TO 990
               END IF
 100           CONTINUE
C                                       Close Input file
 120     CALL UVGET ('CLOS', DUM, DUM, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Sum flag counts
         TOTREC(1,1) = TOTREC(1,1) + CNTREC(1,1)
         TOTREC(2,1) = TOTREC(2,1) + CNTREC(2,1)
         TOTREC(1,2) = TOTREC(1,2) + CNTREC(1,2)
         TOTREC(2,2) = TOTREC(2,2) + CNTREC(2,2)
         TOTREC(1,3) = TOTREC(1,3) + CNTREC(1,3)
         TOTREC(2,3) = TOTREC(2,3) + CNTREC(2,3)
 200     CONTINUE
      SUBARR = SUBTMP
C                                       Flush output
      NIO = 0
      CALL UVDISK ('FLSH', LUN, FIND, BUFF1, NIO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISOUT, CNOOUT, LUN, CATBLK, IRET)
C                                       close NX table
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         CALL MSGWRT (7)
         END IF
C                                      Put vis. count in CATBLK
      CATBLK(KIGCN) = NVIS
C                                       Copy relevant portion of IF
C                                       table.
      IIVER = 1
      OOVER = 1
      CALL CHNCOP (IIVER, OOVER, LUN1, LUN2, DISKIN, DISOUT, CNOIN,
     *   CNOOUT, CATIN, CATBLK, BIF, EIF, FRQSEL, SFREQS, BUFF1, FOFF,
     *   ISBAND, FINC, IRET)
      IRET = 0
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISOUT, CNOOUT, CATBLK, 'REST', BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1230) IRET
         CALL MSGWRT (6)
         END IF
C                                       Close output
      CALL ZCLOSE (LUN, FIND, IRET)
C                                       Give data summary
      WRITE (MSGTXT,2800)
      CALL MSGWRT (5)
      WRITE (MSGTXT,2801) TOTREC(1,1), TOTREC(1,2), TOTREC(1,3)
      CALL MSGWRT (5)
      WRITE (MSGTXT,2802) TOTREC(2,1), TOTREC(2,2), TOTREC(2,3)
      CALL MSGWRT (5)
C                                       No data found.
      IF (NVIS.GT.0) GO TO 999
         IRET = 9
         WRITE (MSGTXT,1800)
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CLBAPL: ERROR',I5,' READING OUTPUT CATBLK')
 1020 FORMAT ('CLBAPL: ERROR',I5,' OPENING OUTPUT FILE')
 1030 FORMAT ('CLBAPL: ERROR',I5,' INIT. OUTPUT FILE')
 1090 FORMAT ('CLBAPL: ERROR',I5,' WRITING OUTPUT FILE')
 1230 FORMAT ('CLBAPL: ERROR',I3,' UPDATING CATALOG HEADER')
 1800 FORMAT ('CLBAPL: ERROR - NO DATA WRITTEN')
 2000 FORMAT ('Applying solutions to data')
 2800 FORMAT (10X,' Previously flagged ','  Flagged by gain   ',
     *   '      Kept')
 2801 FORMAT ('Partially ',2I20,I10)
 2802 FORMAT ('Fully     ',2I20,I10)
      END
      SUBROUTINE CLBSOL (MAXBL, MAXFRQ, IRET)
C-----------------------------------------------------------------------
C   CLBSOL calls GASOLV.  Its only purpose is to declare various arrays
C   outside of the main routine, to allow dynamic memory to be used.
C-----------------------------------------------------------------------
      INTEGER   MAXBL, MAXFRQ, IRET
C
      INTEGER   MAXIFS, NWORDS, SOLSUB, SOLMIN
      LONGINT   PVOBS, PVWT
      INCLUDE 'CALIB.INC'
      INCLUDE 'INCS:DGDS.INC'
      REAL      CREAL(2*MAXIF*MAXANT), CIMAG(2*MAXIF*MAXANT),
     *   CDELY(2*MAXIF*MAXANT), CRATE(2*MAXIF*MAXANT),
     *   CWT(2*MAXIF*MAXANT), VOBS(2), VWT(2)
      COMMON /SOLCOM/ SOLSUB, SOLMIN
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      MAXIFS = 1
      IF (JLOCIF.GT.0) MAXIFS = CATBLK(KINAX+JLOCIF)
C                                       sub interval parameters
      SOLMIN = XSOLM + 0.1
      SOLSUB = XSOLS + 0.1
      IF (SOLSUB.LE.0) SOLSUB = 1
      IF (SOLSUB.GT.10) SOLSUB = 10
      IF ((SOLMIN.LE.0) .OR. (SOLMIN.GT.SOLSUB)) SOLMIN = SOLSUB
C                                       allocate memory
      NWORDS = (2 * MAXBL * MAXFRQ * SOLSUB - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'CLBSOL', NWORDS, VOBS, PVOBS, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'CLBSOL', NWORDS, VWT, PVWT,
     *   IRET)
C                                       solve
      IF (IRET.EQ.0) CALL GASOLV (VOBS(1+PVOBS), VWT(1+PVWT), CREAL,
     *   CIMAG, CDELY, CRATE, CWT, MAXBL, MAXFRQ, MAXIFS, SOLSUB,
     *   SOLMIN, IRET)
C                                       free memory
      CALL ZMEMRY ('FRAL', 'CLBSOL', NWORDS, VOBS, PVOBS, PVWT)
C
 999  RETURN
      END
      SUBROUTINE GASOLV (VOBS, VWT, CREAL, CIMAG, CDELY, CRATE, CWT,
     *   MAXBL, MAXFRQ, MAXIFS, SOLSUB, SOLMIN, IERR)
C-----------------------------------------------------------------------
C   GASOLV reads thru a data file which has been divided by the model
C   and makes the requested solutions which are written into a solution
C   (SN) table.
C   Notes:
C    1) When more than one polarization (max of 2) is to be processed
C       then a second set of frequency slots in VOBS is used for this
C       data.
C   In/out
C      VOBS     R(*)   Work array: (2,MAXBL,MAXFRQ,SOLSUB)
C      VWT      R(*)   Work array: (2,MAXBL,MAXFRQ,SOLSUB)
C      CREAL    R(*)   Work array. (2,MAXIFS,NUMANT)
C      CIMAG    R(*)   Work array. (2,MAXIFS,NUMANT)
C      CDELY    R(*)   Work array. (2,MAXIFS,NUMANT)
C      CRATE    R(*)   Work array. (2,MAXIFS,NUMANT)
C      CWT      R(*)   Work array. (2,MAXIFS,NUMANT)
C      MAXBL    I      Max. number of baselines in data.
C      MAXFRQ   I      Maximum number of frequency channels * pols
C      MAXIFS   I      Max number of IFs
C      SOLSUB   I      Number of sub-intervals to use
C      SOLMIN   I      Minimum number subintervals for solution
C   From common:
C    SOLINT        R    Solution interval (days).
C    REFANT        I    Ref ant to use.
C    DOL1          L    True if L1 solution wanted.
C    DOGCON        L    True if gain constrained soln. wanted.
C    DOMGM         L    True then find the mean gain modulus and save it
C    DOSAVG        I    True if amp-scalar averaging requested.
C    GAERR(*)      R    Gain error for constraints.
C    CONFAC        R    Factor for penalty term
C    AVGPOL        L    True if RR and LL to be averaged
C    AVGIF         L    True if all IFs to be averaged
C    GSOLV(*)      L    List of antennas to solve for.
C    NUMBL         I    Number of baselines
C    NUMIF         I    Number of IFs
C    SNRMIN        R    Minimum acceptable SNR
C    PRTLV         I    Print level
C    MODE          I    0=full gain, 1=phase only, 2 phase(no amp),
C                       3 = amp. constr
C    MINNO         I    Min. no. antennas.
C    ANTWT(20)     R    Antenna weights.
C    CATBLK(256)   I    Output catalog header.
C    CATIN(256)    I    Input catalog header.
C    CNOIN         I    Input data cat. #.
C    CNOOUT        I    Output data cat #.
C    DISKIN        I    Input data disk number.
C    DISOUT        I    Output data disk number.
C    SNVER         I    Version of SN table to use
C    JBUFSZ        I    Buffer size.
C    BUFF1(*)      I    Work buffer
C    BUFF2(*)      I    Work buffer. Used for EQUIVALENCEs.
C   Output:
C      IERR     I      Return code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   MAXBL, MAXFRQ, MAXIFS, SOLSUB, SOLMIN, IERR
      REAL      VOBS(2,MAXBL,MAXFRQ,SOLSUB), VWT(2,MAXBL,MAXFRQ,SOLSUB),
     *   CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*), CDELY(2,MAXIFS,*),
     *   CRATE(2,MAXIFS,*), CWT(2,MAXIFS,*)
C
      INTEGER   HDIM
      PARAMETER (HDIM = 50)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUNI, LUNSS, FINDI, BINDI, NIN, NBL, NTIM, IC, NRMS,
     *   NBLANK, I, IBL, DISK, INDEX, JBL, KK1, KK2, KK3, INCIII, II,
     *   NANTM1, I1P1, I1, I2, IROUND, SCNSOU, SCNSUB, SUBA, NUMINT,
     *   NFPIF, NUMFRQ, JERR, NOIF, MFRQ, SNKOLS(MAXSNC), IBASE,
     *   SNNUMV(MAXSNC), NODENO, JFRQ, JIF, NXVER, FREQID, NNSOU,
     *   OFFIF, LLL, BO, VO, VISCNT, ISNRNO, IDUM1, IDUM2, VCNO, IMGM,
     *   KEYLOC, KEYTYP, ORIGIN, NUMKEY, LIMIT, LIMIT2, J, IRET, ISUB,
     *   MSUB, HFRAC(HDIM+1), NFLAGS, NSNREC, NWORDS, KK4, IPOL
      REAL      DELT, CATR(128), WT, CATIR(256), SCNTIM, SCNDT, BASEL,
     *   MX2BAS, MN2BAS, BLFACT, XXAMP, YYAMP, IFRM, ELSOL, HASOL,
     *   ZASOL, CURSUB, SSRMS, SRMS, RATE, CHFLGS(MAXCIF), SUMR, SUMI,
     *   SUMA, SUMW, GMMO(MAXIF,2), SNWORK(2), SNDATA(2), TEMP
      LONGINT   PSNDAT, PSNWRK
      CHARACTER IFILE*48, KEYWRD*8, CPOL(2,2)*1
      LOGICAL   T, F, JUSRED, DONDX, TWOSTO, ISAPPL, ISIQUV
      DOUBLE PRECISION TIMEC, CATD(128), TIMRA(2), LASTIM, STTIME(10),
     *   CURTIM, ENDTIM, CURINT, CUREND, SCNEND, SIUSE, DEPS
      INCLUDE 'CALIB.INC'
      INCLUDE 'CALFLAG'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   BLCODE(MXBASE), IS(MXBASE), JS(MXBASE), REFAN(2,MAXIF)
      LOGICAL   GOTANT(MAXANT,10), GOTDAT(2,MAXIF,MAXANT,10)
      EQUIVALENCE (CATIN, CATIR),        (CATBLK, CATR, CATD)
      DATA ISAPPL /.FALSE./
      DATA KEYWRD /'MGMOD   '/
      DATA T,F /.TRUE.,.FALSE./
      DATA LUNI, LUNSS /16, 27/
      DATA BO, VO /1,0/
      DATA CPOL /'R','L','X','Y'/
C-----------------------------------------------------------------------
C                                       0.001 sec
      DEPS = 1.157407D-9
      SOLSUB = MAX (1, MIN (10, SOLSUB))
      CALL FILL (HDIM+1, 0, HFRAC)
      NFLAGS = 0
      NRMS = 0
      SRMS = 0
      SSRMS = 0
      NSNREC = 0
C                                       Message(s)
      IF (DOSAVG.EQ.2) THEN
         MSGTXT = 'Determining solutions using full amp-scalar' //
     *      ' averaging'
      ELSE IF (DOSAVG.EQ.1) THEN
         MSGTXT = 'Determining solutions using amp-scalar' //
     *      ' averaging only in time'
      ELSE
         MSGTXT = 'Determining solutions using full vector averaging'
         END IF
      CALL MSGWRT (2)
      CNTOK = 0
      CNTBAD = 0
      CNTFEW = 0
      CNTSAD = 0
C                                       Square baseline limits
      MX2BAS = MXPABL * MXPABL * 1.0E6
      MN2BAS = MNPABL * MNPABL * 1.0E6
      NOIF = EIF - BIF + 1
      NUMFRQ = NOIF * (ECHAN - BCHAN + 1)
      NFPIF = NUMFRQ / NUMIF
      CALL CHWANT (NFPIF, NOIF, CHNSEL, CHFLGS)
      IMGM = 0
      NUMBL = (NUMANT * (NUMANT-1)) / 2
C                                       Purge old SN table?
      IF (SNVER.GT.0) THEN
         NNSOU = NSOUWD
         IF (SINGLE) NNSOU = 0
         TIMRA(1) = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0)
     *      + (XTIME(4) / (24.0*60.0*60.0))
         TIMRA(2) = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0)
     *      + (XTIME(8) / (24.0*60.0*60.0))
         IF ((TIMRA(2).LT.TIMRA(1)) .OR. (TIMRA(2).LT.1.0E-5))
     *      TIMRA(2) = 1.0E20
         CALL CALSEL (DISKIN, CNOIN, 'SN', SNVER, CATIN, CLBUFF, NNSOU,
     *      SOUWAN, DOSWNT, NANTSL, ANTENS, DOAWNT, TIMRA, SUBARR,
     *      FRQSEL, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Init. Gain. file.
      NUMNOD = 0
      NODENO = 0
      NUMPOL = 1
      GMMOD = 1.0
      IF (NCOR.GT.1) NUMPOL = 2
C                                       protect catalog status
      CALL CATFIX (DISKIN, CNOIN, 'NOTR')
C                                        IQUV?
      ISIQUV = CATD(KDCRV+JLOCS).GT.0.0
      IF (ISIQUV) NUMPOL = 1
      CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, CATIN, LUNSS,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGTXT,1140) SNVER
      CALL MSGWRT (3)
      IFRM = 0.0
C                                       get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBARR, CATIN, BUFFS, IRET)
C                                       Add the ORIGIN keyword
      KEYLOC = 1
      KEYTYP = 4
      ORIGIN = 0
      NUMKEY = 1
      IF (SINGLE) ORIGIN = 1
      CALL TABKEY ('WRIT', 'SNORIGIN', NUMKEY, CLBUFF, KEYLOC, ORIGIN,
     *   KEYTYP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1150) IERR
         GO TO 990
         END IF
C                                        Open vis. file
      DISK = VISDSK
      VCNO = VISCNO
      IF (VISDSK.EQ.0) THEN
         DISK = SCRVOL(VISCNO)
         VCNO = SCRCNO(VISCNO)
         CALL ZPHFIL ('SC', DISK, VCNO, 1, IFILE, IRET)
      ELSE
         CALL ZPHFIL ('UV', DISK, VCNO, 1, IFILE, IRET)
         END IF
      CALL ZOPEN (LUNI, FINDI, DISK, IFILE, T, F, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR
         GO TO 990
         END IF
C                                        Initialize UV file
      NIN = 1
      VO = 0
      CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC, NIN, JBUFSZ,
     *   BUFF1, BO, BINDI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Setup.
      VISCNT = 0
C                                        Clear "got data" flag
      JUSRED = F
C                                        Set baseline arrays.
      NANTM1 = NUMANT - 1
      NBL = 0
      IF (ILOCB.GE.0) THEN
         IBASE = 256
      ELSE
         IBASE = 32768
         END IF
      DO 20 I1 = 1, NANTM1
         I1P1 = I1 + 1
         DO 10 I2 = I1P1, NUMANT
            NBL = NBL + 1
            BLCODE(NBL) = I1 * IBASE + I2
            IS(NBL) = I1
            JS(NBL) = I2
 10         CONTINUE
 20      CONTINUE
C                                       Initialize I/O to INDEX file
      MSGSUP = 32000
      NXVER = 1
      CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATIN,
     *   IXLUN, INXRNO, NXKOLS, NXNUMV, IRET)
      MSGSUP = 0
      DONDX = IRET.EQ.0
C                                       Dummy if no scans
      SCNTIM = -1.0E10
      SCNEND =  1.0E10
      SCNSOU = 0
      SCNSUB = 0
      SUBA = SUBARR
C                                       Read first scan info
      IF (DONDX) THEN
 30      IF (INXRNO.LE.NXBUFF(5)) THEN
            CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, SCNTIM,
     *         SCNDT, SCNSOU, SCNSUB, IDUM1, IDUM2, FREQID, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Scan match current data?
            IF ((SCNSUB.GT.0) .AND. (SUBARR.GT.0) .AND.
     *         (SUBARR.NE.SCNSUB)) GO TO 30
            IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *         (FRQSEL.NE.FREQID)) GO TO 30
            SCNTIM = SCNTIM - 0.5D0 * SCNDT
            SCNEND = SCNTIM + SCNDT + 5.0D0*DEPS
            IF (SCNTIM.LT.TSTART) THEN
               SCNDT = SCNDT - (TSTART - SCNTIM)
               SCNTIM = TSTART
               END IF
            IF (SCNEND.GT.TEND) THEN
               SCNDT = SCNDT - (SCNEND - TEND)
               SCNEND = TEND + 10.0D0*DEPS
               END IF
            NUMINT = IROUND (SCNDT / SOLINT)
            NUMINT = MAX (NUMINT, 1)
            SIUSE = SCNDT / NUMINT
            IF (SIUSE.LE.0.0) SIUSE = SOLINT
            END IF
      ELSE
         FREQID = FRQSEL
         SIUSE = SOLINT
         END IF
C                                       Stokes' parameter
      ICOR0 = ABS (ICOR0)
      INCS = ABS (INCS)
C                                       See if can use spare frequency
C                                       for 2nd Stokes (e.g. RR & LL)
C                                       Don't if IQUV poln.
      TWOSTO = (NCOR.GT.1) .AND. (.NOT.AVGPOL) .AND. (.NOT.ISIQUV)
C                                       Offset in IF for Stokes no. 2
      OFFIF = NOIF + 1
      IF (AVGPOL) OFFIF = 1
C                                       Increment for IF
      INCIII = 1
      IF (AVGIF) INCIII = 0
C                                       Begin Loop in time.
 80   NIN = 1
C                                       Clear "Got data" flags
      J = 2 * NUMIF
      DO 90 ISUB = 1,SOLSUB
         DO 85 LLL = 1,NUMANT
            GOTANT(LLL,ISUB) = F
            CALL LFILL (J, F, GOTDAT(1,1,LLL,ISUB))
 85         CONTINUE
 90      CONTINUE
C                                       Blank/zero solution values
      NBLANK = 2 * NUMIF * NUMANT
      CALL RFILL (NBLANK, FBLANK, CREAL)
      CALL RFILL (NBLANK, FBLANK, CIMAG)
      CALL RFILL (NBLANK, 0.0, CDELY)
      CALL RFILL (NBLANK, 0.0, CRATE)
      CALL RFILL (NBLANK, 0.0, CWT)

      IC = 1
C                                       IC=0 means averaged Stokes
      IF (AVGPOL) IC = 0
C                                       IC=-1 means both Stokes
      IF (TWOSTO) IC = -1
C                                       IC=3 means Ipol
      IF (ISIQUV) IC = 3
C                                       Init. for sol. interval.
      VISCNT = 0
C                                        Zero fill VOBS
      DO 110 ISUB = 1,SOLSUB
         DO 105 KK3 = 1,MAXFRQ
            DO 100 KK2 = 1,NUMBL
               VOBS(1,KK2,KK3,ISUB) = 0.0
               VOBS(2,KK2,KK3,ISUB) = 0.0
               VWT(1,KK2,KK3,ISUB) = 0.0
               VWT(2,KK2,KK3,ISUB) = 0.0
 100           CONTINUE
 105        CONTINUE
 110     CONTINUE
C                                       Read first record (if nec)
C                                       and setup
      IF (.NOT.JUSRED) THEN
         NIN = 0
         CALL UVDISK ('READ', LUNI, FINDI, BUFF1, NIN, BINDI, IERR)
         IF ((NIN.LE.0) .OR. (IERR.EQ.4)) THEN
            IERR = 0
            NIN = 0
            GO TO 300
         ELSE IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'READ'
            GO TO 990
            END IF
         JUSRED = T
         END IF
C                                       parameters of data point
      CURTIM = BUFF1(BINDI+ILOCT)
      GO TO 130
C                                       Find index record
 120  IF (INXRNO.LE.NXBUFF(5)) THEN
         CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, SCNTIM,
     *      SCNDT, SCNSOU, SCNSUB, IDUM1, IDUM2, FREQID, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Scan match current data?
         IF ((SCNSUB.GT.0) .AND. (SUBARR.GT.0) .AND.
     *      (SUBARR.NE.SCNSUB)) GO TO 120
         IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *      (FRQSEL.NE.FREQID)) GO TO 120
         SCNTIM = SCNTIM - 0.5D0 * SCNDT
         SCNEND = SCNTIM + SCNDT + 5.0*DEPS
         IF (SCNTIM.LT.TSTART) THEN
            SCNDT = SCNDT - (TSTART - SCNTIM)
            SCNTIM = TSTART
            END IF
         IF (SCNEND.GT.TEND) THEN
            SCNDT = SCNDT - (SCNEND - TEND)
            SCNEND = TEND + 10.0D0*DEPS
            END IF
         NUMINT = IROUND (SCNDT / SOLINT)
         NUMINT = MAX (NUMINT, 1)
         SIUSE = SCNDT / NUMINT
         IF (SIUSE.LE.0.0) SIUSE = SOLINT
      ELSE
         SCNEND = CURTIM + SOLINT
         SIUSE = SOLINT
         END IF
C                                       done already?
 130  IF (CURTIM.GT.SCNEND+195.0D0*DEPS) GO TO 120
      LASTIM = CURTIM + SIUSE
C                                       if this leave a dangle interval
C                                       < 0.6 * SIUSE go to the end
C                                       extend a ways for time accuracy
C                                       issues but not more than 1 min
      IF (LASTIM.GT.SCNEND-0.6D0*SIUSE) LASTIM = SCNEND +
     *   MIN (0.5D0 * SIUSE, 0.0007D0)
      NTIM = 1
      STTIME(1) = CURTIM
      CURINT = SIUSE
C                                       make array of subinterval start
C                                       times
      CURSUB = CURINT / SOLSUB
      DO 140 ISUB = 2,SOLSUB
         STTIME(ISUB) = STTIME(ISUB-1) + CURSUB
 140     CONTINUE
      MSUB = 0
C                                       Load data into array.
C                                       Begin Loop.
C                                       If next point already read,
C                                       skip read.
      IF (JUSRED) GO TO 210
 200     NIN = 0
         CALL UVDISK ('READ', LUNI, FINDI, BUFF1, NIN, BINDI, IERR)
         IF ((NIN.LE.0) .OR. (IERR.EQ.4)) THEN
            IERR = 0
            NIN = 0
            GO TO 300
         ELSE IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'READ'
            GO TO 990
            END IF
 210     CURTIM = BUFF1(BINDI+ILOCT)
C                                       Check for last time.
         IF (CURTIM.GT.LASTIM-DEPS) GO TO 300
C                                       Check if finished.
         IF (NIN.LE.0) GO TO 300
C                                       Determine baseline code.
         IF (ILOCB.GE.0) THEN
            JBL = BUFF1(BINDI+ILOCB) + 0.1
            I1 = JBL / 256
            I2 = JBL - I1 * 256
         ELSE
            I1 = BUFF1(BINDI+ILOCA1) + 0.1
            I2 = BUFF1(BINDI+ILOCA2) + 0.1
            JBL = 32768 * I1 + I2
            END IF
C                                       Look for match.
         DO 220 I = 1,NBL
            IBL = I
            IF (JBL.EQ.BLCODE(I)) GO TO 240
 220        CONTINUE
C                                       Bad baseline code.
C                                       No message on AC data
         IF (I2.NE.I1) THEN
            WRITE (MSGTXT,1130) I1, I2, NUMANT
            CALL MSGWRT (6)
            END IF
C                                       Go to next data point
         GO TO 200
C                                       Datum OK
 240     ENDTIM = CURTIM
C                                       find sub interval
         DO 250 I1 = 1,SOLSUB
            IF (CURTIM.GE.STTIME(I1)) ISUB = I1
 250        CONTINUE
         MSUB = MAX (MSUB, ISUB)
C                                       Baseline factors
         I1 = IS(IBL)
         I2 = JS(IBL)
         GOTANT(I1,ISUB) = T
         GOTANT(I2,ISUB) = T
         BLFACT = ANTWT(I1) * ANTWT(I2)
         BASEL = BUFF1(BINDI+ILOCU) * BUFF1(BINDI+ILOCU) +
     *      BUFF1(BINDI+ILOCV) * BUFF1(BINDI+ILOCV)
         IF ((BASEL.LT.MN2BAS) .OR. (BASEL.GT.MX2BAS))
     *      BLFACT = BLFACT * WTPABL
C                                       Accumulate
         VISCNT = VISCNT + 1
         I = 0
         DO 260 JIF = 1,NUMIF
            INDEX = BINDI + NRPARM + (JIF-1) * INCIF
            II = (JIF-1) * INCIII + 1
            SUMR = 0.0
            SUMI = 0.0
            SUMA = 0.0
            SUMW = 0.0
            DO 255 JFRQ = 1, NFPIF
               I = I + 1
               WT = BLFACT * BUFF1(INDEX+2) * CHFLGS(I)
               IF (WT.GT.0.0) THEN
                  GOTDAT(1,JIF,I1,ISUB) = T
                  GOTDAT(1,JIF,I2,ISUB) = T
                  SUMR = SUMR + WT * BUFF1(INDEX)
                  SUMI = SUMI + WT * BUFF1(INDEX+1)
                  SUMW = SUMW + WT
                  IF (DOSAVG.EQ.2) SUMA = SUMA +
     *               WT * SQRT (BUFF1(INDEX)*BUFF1(INDEX) +
     *               BUFF1(INDEX+1)*BUFF1(INDEX+1))
                  END IF
               INDEX = INDEX + INCF
 255           CONTINUE
            VOBS(1,IBL,II,ISUB) = VOBS(1,IBL,II,ISUB) + SUMR
            VOBS(2,IBL,II,ISUB) = VOBS(2,IBL,II,ISUB) + SUMI
            VWT(1,IBL,II,ISUB) = VWT(1,IBL,II,ISUB) + SUMW
            IF (DOSAVG.EQ.1) SUMA = SQRT (SUMR*SUMR + SUMI*SUMI)
            IF (DOSAVG.GT.0) VWT(2,IBL,II,ISUB) = VWT(2,IBL,II,ISUB) +
     *         SUMA
 260        CONTINUE
C                                       Sum or include 2nd Stokes'
         IF (AVGPOL .OR. TWOSTO) THEN
            I = 0
            DO 270 JIF = 1,NUMIF
               INDEX = BINDI + NRPARM + (JIF-1) * INCIF + INCS
               II = (JIF-1) * INCIII + OFFIF
               SUMR = 0.0
               SUMI = 0.0
               SUMA = 0.0
               SUMW = 0.0
               DO 265 JFRQ = 1,NFPIF
                  I = I + 1
                  WT = BLFACT * BUFF1(INDEX+2) * CHFLGS(I)
                  IF (WT.GT.0.0) THEN
                     GOTDAT(2,JIF,I1,ISUB) = T
                     GOTDAT(2,JIF,I2,ISUB) = T
                     SUMR = SUMR + WT * BUFF1(INDEX)
                     SUMI = SUMI + WT * BUFF1(INDEX+1)
                     SUMW = SUMW + WT
                     IF (DOSAVG.EQ.2) SUMA = SUMA +
     *                  WT * SQRT (BUFF1(INDEX)*BUFF1(INDEX) +
     *                  BUFF1(INDEX+1)*BUFF1(INDEX+1))
                     END IF
                  INDEX = INDEX + INCF
 265              CONTINUE
               VOBS(1,IBL,II,ISUB) = VOBS(1,IBL,II,ISUB) + SUMR
               VOBS(2,IBL,II,ISUB) = VOBS(2,IBL,II,ISUB) + SUMI
               VWT(1,IBL,II,ISUB) = VWT(1,IBL,II,ISUB) + SUMW
               IF (DOSAVG.EQ.1) SUMA = SQRT (SUMR*SUMR + SUMI*SUMI)
               IF (DOSAVG.GT.0) VWT(2,IBL,II,ISUB) = VWT(2,IBL,II,ISUB)
     *            + SUMA
 270           CONTINUE
            END IF
C                                       Loop back for next time
         GO TO 200
C                                       End of solution interval.
 300  JUSRED = T
      MFRQ = NOIF
      IF (TWOSTO) MFRQ = 2 * NOIF
C                                       Compress subintervals into 1
      DO 330 ISUB = 2,MSUB
         DO 320 I1 = 1,MFRQ
            DO 310 I2 = 1,NUMBL
               VOBS(1,I2,I1,1) = VOBS(1,I2,I1,1) + VOBS(1,I2,I1,ISUB)
               VOBS(2,I2,I1,1) = VOBS(2,I2,I1,1) + VOBS(2,I2,I1,ISUB)
               VWT(1,I2,I1,1) = VWT(1,I2,I1,1) + VWT(1,I2,I1,ISUB)
               VWT(2,I2,I1,1) = VWT(2,I2,I1,1) + VWT(2,I2,I1,ISUB)
 310           CONTINUE
 320        CONTINUE
         DO 325 LLL = 1,NUMANT
            IF (GOTANT(LLL,ISUB)) GOTANT(LLL,1) = T
            DO 324 JIF = 1,NUMIF
               IF (GOTDAT(1,JIF,LLL,ISUB)) GOTDAT(1,JIF,LLL,1) = T
               IF (GOTDAT(2,JIF,LLL,ISUB)) GOTDAT(2,JIF,LLL,1) = T
 324           CONTINUE
 325        CONTINUE
 330     CONTINUE
C                                       Normalize visibility array
      DO 360 I1 = 1,MFRQ
         DO 340 I2 = 1,NUMBL
            IF (VWT(1,I2,I1,1).GT.1.0E-20) THEN
               VOBS(1,I2,I1,1) = VOBS(1,I2,I1,1) / VWT(1,I2,I1,1)
               VOBS(2,I2,I1,1) = VOBS(2,I2,I1,1) / VWT(1,I2,I1,1)
               END IF
 340        CONTINUE
C                                       Amp-scalar averaging
         IF (DOSAVG.GT.0) THEN
            DO 350 I2 = 1,NUMBL
               IF (VWT(1,I2,I1,1).GT.1.0E-20) THEN
                  XXAMP = SQRT ((VOBS(1,I2,I1,1)**2) +
     *               (VOBS(2,I2,I1,1)**2)) * VWT(1,I2,I1,1)
                  YYAMP = 1.0
                  IF (XXAMP.GT.1.0E-20)
     *               YYAMP = VWT(2,I2,I1,1) / XXAMP
                  VOBS(1,I2,I1,1) = VOBS(1,I2,I1,1) * YYAMP
                  VOBS(2,I2,I1,1) = VOBS(2,I2,I1,1) * YYAMP
                  END IF
 350           CONTINUE
            END IF
 360     CONTINUE
C                                       Lookup source name
      CALL GETSOU (SCNSOU, DISKIN, CNOIN, CATUV, LUNS, JERR)
C                                       Amplitude, phase soln.
      TIMEC = (STTIME(1) + ENDTIM) * 0.5D0
      CALL CLBPA (IS, JS, VOBS, VWT, CREAL, CIMAG, CWT, REFAN,
     *   MAXFRQ, MAXIFS, MAXBL, NUMANT, NUMBL, NOIF, REFANT, AVGIF,
     *   DOL1, DOGCON, GAERR, MODE, MINNO, CONFAC, SNRMIN, PRTLV,
     *   CPARM(7), CPARM(6), XDOFL, CLOSER, TIMEC, SNAME, IC, GSOLV,
     *   HDIM, HFRAC, NRMS, SRMS, SSRMS)
C                                       Write solution record.
      DELT = ENDTIM - STTIME(1)
      IF (DELT.LT.0) DELT = 0.0
      CALL CLSNOU (TIMEC, DELT, SCNSOU, IFRM, NODENO, SUBA, MAXIFS,
     *   GOTANT, GOTDAT, ISNRNO, SNKOLS, SNNUMV, CREAL, CIMAG, CDELY,
     *   CRATE, CWT, REFAN, NSNREC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       write FG table
      IF (FLAGIT) THEN
         CALL FLAGWT ('WRIT', STTIME(1), ENDTIM, IS, JS, VWT, MAXBL,
     *      NUMBL, NOIF, AVGIF, IC, DISKIN, CNOIN, CATIN, NFLAGS, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Sum mean gain modulus
      IF (DOMGM.GT.0) THEN
         DO 430 KK1 = 1,NUMANT
C                                       Select on antenna.
            IF (SLMEAN(KK1)) THEN
C                                       Select on elevation.
               CALL SOUELV (KK1, TIMEC, HASOL, ELSOL, ZASOL)
               IF (ELSOL/DG2RAD.GE.GMINEL) THEN
                  DO 420 KK2 = BIF,EIF
                     DO 410 KK3 = 1,NUMPOL
                        IPOLIF = KK3 + (KK2 - 1) * NUMPOL
                        IF ((GOTANT(KK1,1)) .AND.
     *                     (CWT(KK3,KK2,KK1).GT.1.0E-20) .AND.
     *                     (CREAL(KK3,KK2,KK1).NE.FBLANK) .AND.
     *                     (CIMAG(KK3,KK2,KK1).NE.FBLANK)) THEN
                           CNTMG(IPOLIF) = CNTMG(IPOLIF) + 1
                           SUMMG(IPOLIF) = SUMMG(IPOLIF) +
     *                        SQRT(CREAL(KK3,KK2,KK1)*CREAL(KK3,KK2,KK1)
     *                        + CIMAG(KK3,KK2,KK1)*CIMAG(KK3,KK2,KK1))
                           CNTMGM = CNTMGM + 1
                           SUMMGM = SUMMGM +
     *                        SQRT(CREAL(KK3,KK2,KK1)*CREAL(KK3,KK2,KK1)
     *                        + CIMAG(KK3,KK2,KK1)*CIMAG(KK3,KK2,KK1))
                           END IF
 410                    CONTINUE
 420                 CONTINUE
                  END IF
               END IF
 430        CONTINUE
         END IF
C                                       Shift subintervals
      DO 470 ISUB = 2,MSUB
         DO 450 I1 = 1,MFRQ
            DO 440 I2 = 1,NUMBL
               VOBS(1,I2,I1,ISUB-1) = VOBS(1,I2,I1,ISUB)
               VOBS(2,I2,I1,ISUB-1) = VOBS(2,I2,I1,ISUB)
               VWT(1,I2,I1,ISUB-1) = VWT(1,I2,I1,ISUB)
               VWT(2,I2,I1,ISUB-1) = VWT(2,I2,I1,ISUB)
               IF (ISUB.EQ.MSUB) THEN
                  VOBS(1,I2,I1,ISUB) = 0.0
                  VOBS(2,I2,I1,ISUB) = 0.0
                  VWT(1,I2,I1,ISUB) = 0.0
                  VWT(2,I2,I1,ISUB) = 0.0
                  END IF
 440           CONTINUE
 450        CONTINUE
         DO 460 LLL = 1,NUMANT
            GOTANT(LLL,ISUB-1) = GOTANT(LLL,ISUB)
            GOTANT(LLL,ISUB) = .FALSE.
            DO 459 JIF = 1,NUMIF
               GOTDAT(1,JIF,LLL,ISUB-1) = GOTDAT(1,JIF,LLL,ISUB)
               GOTDAT(2,JIF,LLL,ISUB-1) = GOTDAT(2,JIF,LLL,ISUB)
               GOTDAT(1,JIF,LLL,ISUB) = .FALSE.
               GOTDAT(2,JIF,LLL,ISUB) = .FALSE.
 459           CONTINUE
 460        CONTINUE
         STTIME(ISUB-1) = STTIME(ISUB)
 470     CONTINUE
      MSUB = MSUB - 1
      IF (MSUB.GT.0) THEN
         DO 475 ISUB = MSUB+1,SOLSUB
            STTIME(ISUB) = STTIME(ISUB-1) + CURSUB
 475        CONTINUE
         END IF
      CUREND = STTIME(SOLSUB) + CURSUB
      CUREND = MIN (CUREND, SCNEND)
C                                       If not finished, loop back.
      IF (NIN.GT.0) THEN
         IF (MSUB.LE.0) GO TO 80
         IF ((CURTIM.GE.CUREND) .AND. (MSUB.LT.SOLMIN)) GO TO 80
         IF (CURTIM.GE.CUREND) GO TO 300
         LASTIM = CUREND
         GO TO 210
      ELSE
         IF (MSUB.GE.SOLMIN) GO TO 300
         END IF
C                                       Done: check gain normalization
      IF (DOMGM.GT.1) THEN
         IF (ALZERO) THEN
            MSGTXT = 'All selected antennas are used in the' //
     *         ' gain normalization'
            CALL MSGWRT (5)
         ELSE
            MSGTXT = 'The following antennas are used in the' //
     *         ' gain normalization'
            CALL MSGWRT (5)
            LIMIT = MIN (15, NUSE)
            WRITE (MSGTXT,1215) (LSTUSE(I), I=1,LIMIT)
            CALL MSGWRT (5)
            IF (NUSE.GT.15) THEN
C                                       Rest of antennas
               DO 480 I = 16,NUSE,15
                  LIMIT = I
                  LIMIT2 = I + 14
                  LIMIT2 = MIN (NUSE, LIMIT2)
                  WRITE (MSGTXT,1215) (LSTUSE(J),J=LIMIT,LIMIT2)
                  CALL MSGWRT (5)
 480              CONTINUE
               END IF
            END IF
         END IF
      IF (DOMGM.GT.0) THEN
         KK4 = 1
         IF (CATD(KDCRV+JLOCS).LT.-4.5) KK4 = 2
         CALL RFILL (MAXIF*NUMPOL, 1.0, GMMO)
         DO 490 KK3 = 1,NUMPOL
            DO 485 KK2 = BIF,EIF
               IPOLIF = KK3 + (KK2 - 1) * NUMPOL
C                                       Update mean gain modulus
C                                       for each IF and polarization
               GMMO(KK2,KK3) = 1.0
               IF (CNTMG(IPOLIF).GT.0)
     *            GMMO(KK2,KK3) = SUMMG(IPOLIF) / CNTMG(IPOLIF)
               WRITE (MSGTXT,1200) CPOL(KK3,KK4), KK2, GMMO(KK2,KK3)
               CALL MSGWRT (5)
 485           CONTINUE
 490        CONTINUE
         IF (CNTMGM.GT.0) GMMOD = SUMMGM / CNTMGM
         END IF
C                                       close uv file.
      CALL ZCLOSE (LUNI, FINDI, IERR)
C                                       Close solution file
      CALL TABIO ('CLOS', 0, ISNRNO, TIMEC, CLBUFF, IERR)
C                                       Close index file
      IF (DONDX) CALL TABIO ('CLOS', 0, INXRNO, TIMEC, NXBUFF, IERR)
      IERR = 0
C                                       Nothing worked
      IF (CNTOK.LE.0) THEN
         MSGTXT = 'ERROR: NO VALID SOLUTIONS FOUND'
         IERR = 8
         GO TO 990
C                                       Give body count
      ELSE
         RATE = 0.0
         WRITE (MSGTXT,1490) CNTOK
         CALL MSGWRT (5)
         IF (CNTBAD.GT.0) THEN
            WRITE (MSGTXT,1491) CNTBAD
            CALL MSGWRT (5)
            RATE = CNTBAD * 100.0
            RATE = (RATE / CNTOK) * (1 + CNTBAD/1000)
            END IF
         IF (CNTFEW.GT.0) THEN
            WRITE (MSGTXT,1492) CNTFEW
            CALL MSGWRT (5)
            END IF
         IF (CNTSAD.GT.0) THEN
            WRITE (MSGTXT,1493) CNTSAD
            CALL MSGWRT (5)
            END IF
         IF (RATE.GE.10.0) THEN
            MSGTXT = '********** THIS IS A PRETTY HIGH FAILURE RATE' //
     *         ' **********'
            CALL MSGWRT (3)
            MSGTXT = '********** Consider WEIGHTIT = 1 or higher   ' //
     *         ' **********'
            CALL MSGWRT (3)
            MSGTXT = '********** and/or SOLTYPE = ''L1R'' and/or     '//
     *         ' **********'
            CALL MSGWRT (3)
            MSGTXT = '********** UVRANGE limits or larger SOLINT   ' //
     *         ' **********'
            CALL MSGWRT (3)
            MSGTXT = '********** or redo the NX table with INDXR   ' //
     *         ' **********'
            IF (DONDX) CALL MSGWRT (3)
            END IF
         END IF
C                                       rms of closure
      IF ((NRMS.GT.0) .AND. (SRMS.GT.0.0)) THEN
         SRMS = SRMS / NRMS
         SSRMS = SSRMS / NRMS - SRMS*SRMS
         SSRMS = SQRT (MAX (0.0, SSRMS))
         WRITE (MSGTXT,1494) SRMS, SSRMS
         CALL MSGWRT (4)
         END IF
C                                       fractional discards
      IF (HFRAC(1).GT.0) THEN
         II = 0
         DO 494 I = 1,HDIM
            IF (HFRAC(I+1).GT.0.0) II = I
 494        CONTINUE
         IF (II.GT.0) THEN
            WRITE (MSGTXT,1495) XDOFL
            CALL MSGWRT (4)
            WT = HFRAC(1)
            DO 495 I = 1,II
               I1 = (100/HDIM) * (I - 1)
               I2 = I1 + (100/HDIM)
               XXAMP = HFRAC(I+1) / WT
               WRITE (MSGTXT,1496) XXAMP, I1, I2, XDOFL
               IF (XXAMP.GT.0.0) CALL MSGWRT (4)
 495           CONTINUE
         ELSE
            WRITE (MSGTXT,1497) XDOFL
            CALL MSGWRT (4)
            END IF
         END IF
      IF (NFLAGS.GT.0) THEN
         NFLAGD = NFLAGD + NFLAGS
         WRITE (MSGTXT,1500) NFLAGS, FGVERO
         CALL MSGWRT (4)
         CALL FLAGWT ('CLOS', STTIME(1), ENDTIM, IS, JS, VWT, MAXBL,
     *      NUMBL, NOIF, AVGIF, IC, DISKIN, CNOIN, CATIN, NFLAGS, IERR)
         END IF
      IF (DOMGM.GT.1) THEN
         IF (CPARM(2).GT.0.0) THEN
            MSGTXT = 'Finding median amplitudes in SN table'
            CALL MSGWRT (2)
            NWORDS = (NUMPOL * EIF * NSNREC - 1) / 1024 + 3
            CALL ZMEMRY ('GET ', 'GASOLV', NWORDS, SNDATA, PSNDAT, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY FOR MEDIANS'
               GO TO 990
               END IF
            CALL ZMEMRY ('GET ', 'GASOLV', NWORDS, SNWORK, PSNWRK, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY FOR MEDIANS'
               GO TO 990
               END IF
            CALL GMEDIA (DISKIN, CNOIN, SNVER, SUBARR, BIF, EIF, NUMPOL,
     *         CATIN, NSNREC, DOMGM, SNDATA(1+PSNDAT), SNWORK(1+PSNWRK),
     *         GMMO, GMMOD, IERR)
C                                       Update GLOBAL mean gain modulus
            IF (DOMGM.EQ.2) THEN
               SUMMMM = SUMMMM + GMMOD
               CNTMMM = CNTMMM + 1
               GMMOD = SUMMMM / CNTMMM
               END IF
         ELSE
            IF (DOMGM.EQ.3) THEN
               MSGTXT = 'Apply global mean gain modulus to SN table'
               CALL MSGWRT (3)
               WRITE (MSGTXT,1510) SUBARR, GMMOD
               CALL MSGWRT (3)
               II = EIF - BIF + 1
               CALL RFILL (II, GMMOD, GMMO(BIF,1))
               IF (NUMPOL.EQ.2) CALL RFILL (II, GMMOD, GMMO(BIF,2))
            ELSE IF ((DOMGM.EQ.4) .AND. (NUMPOL.GT.1)) THEN
               MSGTXT = 'Apply mean gain modulus to SN table by IF'
               CALL MSGWRT (3)
               DO 496 KK2 = BIF,EIF
                  IPOLIF = 1 + (KK2 - 1) * NUMPOL
                  GMMO(KK2,1) = 1.0
                  IF ((CNTMG(IPOLIF)+CNTMG(IPOLIF+1)).GT.0)
     *               GMMO(KK2,1) = (SUMMG(IPOLIF) + SUMMG(IPOLIF+1)) /
     *               (CNTMG(IPOLIF) + CNTMG(IPOLIF+1))
                  GMMO(KK2,2) = GMMO(KK2,1)
 496              CONTINUE
            ELSE IF (DOMGM.EQ.5) THEN
               MSGTXT = 'Apply mean gain modulus to SN table by IF/pol'
               CALL MSGWRT (3)
               END IF
            END IF
         IF (DOMGM.GE.6) THEN
            DO 498 KK2 = BIF,EIF
               DO 497 IPOL = 1,NUMPOL
                  IPOLIF = IPOL + (KK2 - 1) * NUMPOL
                  IF (CNTMG(IPOLIF).GT.0) THEN
                     AGMMO(KK2,IPOL,SUBARR) = SUMMG(IPOLIF) /
     *                  CNTMG(IPOLIF)
                  ELSE
                     AGMMO(KK2,IPOL,SUBARR) = FBLANK
                     END IF
 497              CONTINUE
 498           CONTINUE
         ELSE IF (DOMGM.GT.2) THEN
            CALL GMODIT (DISKIN, CNOIN, SNVER, SUBARR, BIF, EIF, CATIN,
     *         GMMO, IERR)
         ELSE
            MSGTXT = 'The global average gain which is actually '
            CALL MSGWRT (5)
            WRITE (MSGTXT,1300) GMMOD
            CALL MSGWRT (5)
            CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, CATIN,
     *         LUNSS, ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF,
     *         NUMNOD, TEMP, RANOD, DECNOD, ISAPPL, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABKEY ('WRIT', KEYWRD, 1, CLBUFF, 1, GMMOD, 2,
     *         IERR)
            CALL TABIO ('CLOS', 0, ISNRNO, TIMEC, CLBUFF, IERR)
            END IF
         END IF
      GO TO 999
C                                        Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('GASOLV: ERROR',I3,' OPENING INPUT UV FILE')
 1010 FORMAT ('GASOLV: ERROR',I3,' INITING UV FILE')
 1100 FORMAT ('GASOLV: ERROR',I3,1X,A4,'ING UV FILE')
 1130 FORMAT ('BAD BASELINE CODE=',I4,'-',I4,' NO. ANT.=',I4)
 1140 FORMAT ('Writing SN table ',I4)
 1150 FORMAT ('GASOLV: ERROR ',I3,' WRITING ORIGIN KEYWORD TO SN TABLE')
 1200 FORMAT (A1,'POL, IF=', I2,
     *   ' The average gain over these antennas is ', 1PE10.3)
 1215 FORMAT (15I3)
 1300 FORMAT ('applied to all data, is  ', 1PE11.4)
 1490 FORMAT ('Found     ',I8,' good solutions')
 1491 FORMAT ('Failed on ',I8,' solutions')
 1492 FORMAT ('          ',I8,' solutions had insufficient data')
 1493 FORMAT ('          ',I8,' solutions had no data')
 1494 FORMAT ('Average closure rms =',F9.5,' +-',F9.5)
 1495 FORMAT ('Fraction of times having data >',F5.1,
     *   ' rms from solution')
 1496 FORMAT (F7.5,' of the times had',I3,' -',I3,' percent outside',
     *   F5.1,' times rms')
 1497 FORMAT ('No data were found >',F5.1,' rms from solution')
 1500 FORMAT (I6,' flags added to the output FG table version',I4)
 1510 FORMAT ('Subarray',I3,' global mean gain modulus is',1PE12.4)
      END
      SUBROUTINE CLSNOU (TIMEC, DELT, SCNSOU, IFRM, NODENO, SUBA,
     *   MAXIFS, GOTANT, GOTDAT, ISNRNO, SNKOLS, SNNUMV, CREAL, CIMAG,
     *   CDELY, CRATE, CWT, REFAN, NSNREC, IERR)
C-----------------------------------------------------------------------
C   CLSNOU prepares a set of SN table entries and writes them to an SN
C   table.  If the weight of a solution is 0.0 it is assumed that there
C   was insufficient data for the solution.  CLBPA sets weights to -1 if
C   the solution fails.
C   Input:
C      TIMEC    D     Time in days
C      DELT     R     Solution interval in days
C      SCNSOU   I     Source number
C      IFRM     R     Ionospheric Faraday Rotation
C      NODENO   I     Node numbe.
C      SUBA     I     Subarray number
C      MAXIFS   I     Maximum number of IFs
C      GOTANT   L(*)  Flags indicating if there was data for each ant.
C      GOTDAT   L(*)  Flags indicating if there was data for each pol,
C                     IF, ant.
C      ISNRNO   I     TABSN counter.
C      SNKOLS   I(*)  SN table column pointers
C      SNNUMV   I(*)  SN table element counts.
C      CREAL    R(2,NUMIF,NUMANT) Real part of solution
C      CIMAG    R(2,NUMIF,NUMANT) Imag part of solution
C      CDELY    R(2,NUMIF,NUMANT) delays in seconds.
C      CRATE    R(2,NUMIF,NUMANT) Rates in Hz.
C      CWT      R(2,NUMIF,NUMANT) Weights = SNR
C      REFAN    I(2,NUMIF) Reference antennas used
C      NSNREC   I          Count of SN records this subarray
C   Input in common:
C      NUMANT   I     The number of antennas.
C      NUMIF    I     The number of IFs.
C      NUMPOL   I     The number of polarizations.
C      SNRMIN   R     Minimum allowed SNR for a solution.
C      DOPASS   L     If true write (1,0) for failed solutions.
C   Input/output in common:
C      REFUSE   I(*)  The number of useages of each antenna as reference
C                     antenna.
C      CNTOK    I     Count of good solutions
C      CNTBAD   I     Count of failed solutions.
C      CNTFEW   I     Count of solutions with too little data.
C   Output:
C      IERR     I     Return code, 0=>OK, else TABSN error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION TIMEC
      INTEGER   SCNSOU, NODENO, SUBA, MAXIFS, ISNRNO, SNKOLS(*),
     *   SNNUMV(*), REFAN(2,*), NSNREC, IERR
      REAL      DELT, IFRM, CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*),
     *   CDELY(2,MAXIFS,*), CRATE(2,MAXIFS,*), CWT(2,MAXIFS,*)
      LOGICAL   GOTANT(*), GOTDAT(2,MAXIF,*)
C
      INTEGER   IANT, IIF, IREF
      REAL      MBDELY(2), DISP(2), DDISP(2)
      INCLUDE 'CALIB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA MBDELY, DISP, DDISP /6*0.0/
C-----------------------------------------------------------------------
C                                       Loop over antennae
      DO 420 IANT = 1,NUMANT
         DO 410 IIF = 1,NUMIF
C                                       Keep track of reference ants
            IREF = REFAN(1,IIF)
            IF ((IREF.GT.0) .AND. (IREF.LE.NUMANT)) THEN
               REFUSE(IREF) = REFUSE(IREF) + 1
               REFUSS(IREF,SUBA) = REFUSS(IREF,SUBA) + 1
C                                       Deal with ref SNR
C              IF (CWT(1,IIF,IREF).LT.(SNRMIN+1.0))
C    *            CWT(1,IIF,IREF) = SNRMIN + 1.0
               END IF
C                                        Second polarization
            IREF = REFAN(2,IIF)
            IF ((IREF.GT.0) .AND. (IREF.LE.NUMANT)) THEN
               REFUSE(IREF) = REFUSE(IREF) + 1
               REFUSS(IREF,SUBA) = REFUSS(IREF,SUBA) + 1
C              IF (CWT(2,IIF,IREF).LT.(SNRMIN+1.0))
C    *            CWT(2,IIF,IREF) = SNRMIN + 1.0
               END IF
C                                       Count good and bad solns
            IF (GOTANT(IANT)) THEN
               IF (CWT(1,IIF,IANT).GE.SNRMIN) THEN
                  CNTOK = CNTOK + 1
               ELSE IF ((ABS (CWT(1,IIF,IANT)).LT.0.1) .AND.
     *            (CWT(1,IIF,IANT).NE.0.0)) THEN
                  CNTFEW = CNTFEW + 1
               ELSE IF (CWT(1,IIF,IANT).GT.-1.1) THEN
                  IF (GOTDAT(1,IIF,IANT)) THEN
                     CNTBAD = CNTBAD + 1
                  ELSE
                     CNTSAD = CNTSAD + 1
                     END IF
                  END IF
C                                       Second poln.
               IF (NUMPOL.GT.1) THEN
                  IF (CWT(2,IIF,IANT).GE.SNRMIN) THEN
                     CNTOK = CNTOK + 1
                  ELSE IF ((ABS (CWT(2,IIF,IANT)).LT.0.1) .AND.
     *               (CWT(2,IIF,IANT).NE.0.0)) THEN
                     CNTFEW = CNTFEW + 1
                  ELSE IF ((CWT(2,IIF,IANT).GT.-1.1) .AND.
     *               (GOTDAT(2,IIF,IANT))) THEN
                     CNTBAD = CNTBAD + 1
                  ELSE
                     CNTSAD = CNTSAD + 1
                     END IF
                  END IF
               END IF
C                                       Keep bad solutions?
            IF (DOPASS) THEN
               IF (CWT(1,IIF,IANT).LT.SNRMIN) THEN
                  CREAL(1,IIF,IANT) = 1.0
                  CIMAG(1,IIF,IANT) = 0.0
                  CWT(1,IIF,IANT) = 1.0
                  END IF
               IF (CWT(2,IIF,IANT).LT.SNRMIN) THEN
                  CREAL(2,IIF,IANT) = 1.0
                  CIMAG(2,IIF,IANT) = 0.0
                  CWT(2,IIF,IANT) = 1.0
                  END IF
               END IF
 410        CONTINUE
         IF (GOTANT(IANT)) THEN
            CALL TABSN ('WRIT', CLBUFF, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *         TIMEC, DELT, SCNSOU, IANT, SUBA, FRQSEL, IFRM, NODENO,
     *         MBDELY, DISP, DDISP, CREAL(1,1,IANT), CIMAG(1,1,IANT),
     *         CDELY(1,1,IANT), CRATE(1,1,IANT), CWT(1,1,IANT), REFAN,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            NSNREC = NSNREC + 1
            END IF
 420     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GMEDIA (DISK, CNO, VER, SUBARR, BIF, EIF, NPOL, CATIN,
     *   NSNREC, DOMGM, SNDATA, SNWORK, GMMO, GMMOD, IERR)
C-----------------------------------------------------------------------
C   GMODIT applies the mean gain modulus to the SN table in place.
C   Inputs:
C      DISK     I      Disk
C      CNO      I      Catalog number
C      VER      I      SN table version #
C      SUBARR   I      Subarray number
C      BIF      I      Begin IF
C      EIF      I      End IF
C      NPOL     I      Number polarizations (1 or 2)
C      CATIN    I(*)   Input file catalog header
C      NSNREC   I      Number records this subarray in SN table
C      DOMGM    I      2 global, 3 subarray, 4 subarray & IF,
C                      5 subarray, IF, pol
C   Output:
C      GMMO     R(*)   Mean gain modulus (MAXIF,2) set for DOMGM type
C      GMMOD    R      global mean modulus
C      IERR     I      Error code
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISK, CNO, VER, SUBARR, BIF, EIF, NPOL, CATIN(*), DOMGM,
     *   NSNREC, IERR
      REAL      SNDATA(NSNREC,NPOL,*), SNWORK(*), GMMO(MAXIF,2), GMMOD
C
      INTEGER   LUN, ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT,
     *   NUMPOL, NUMIF, NUMNOD, IANT, SUBA, LF, SNBUFF(512), I, LUNTMP,
     *   NUMROW,  ANTKOL, SUBKOL, RE1KOL, IM1KOL, RE2KOL, IM2KOL,
     *   RECI(XCLRSZ), NVAL(2,MAXIF), NN, LP
      REAL      RECR(XCLRSZ), IGMMOD, AMP, MEDIAN
      LOGICAL   ISAPPL, OPEN
      DOUBLE PRECISION  RANOD, DECNOD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      EQUIVALENCE (RECI, RECR)
C-----------------------------------------------------------------------
      OPEN = .FALSE.
      LUN = LUNTMP (1)
      I = 2 * MAXIF
      CALL FILL (I, 0, NVAL)
C                                       open SN table to get parms
C                                       Open solution table
      CALL SNINI ('READ', SNBUFF, DISK, CNO, VER, CATIN, LUN, ISNRNO,
     *   SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, IGMMOD, RANOD,
     *   DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN SN TABLE FOR READ'
         GO TO 990
         END IF
      OPEN = .NOT.OPEN
C                                       pointers, number rows
      NUMROW = SNBUFF(5)
      ANTKOL = SNKOLS(SNIANT)
      SUBKOL = SNKOLS(SNISUB)
      RE1KOL = SNKOLS(SNRRE1)
      IM1KOL = SNKOLS(SNRIM1)
      RE2KOL = SNKOLS(SNRRE2)
      IM2KOL = SNKOLS(SNRIM2)
      DO 100 I = 1,NUMROW
         ISNRNO = I
         CALL TABIO ('READ', 0, ISNRNO, RECR, SNBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ SN TABLE'
            GO TO 990
            END IF
         IANT = RECI(ANTKOL)
         SUBA = RECI(SUBKOL)
         IF (SUBA.EQ.SUBARR) THEN
            DO 20 LF = BIF,EIF
               IF (RECR(RE1KOL+LF-1).NE.FBLANK) THEN
                  AMP = RECR(RE1KOL+LF-1)**2 + RECR(IM1KOL+LF-1)**2
                  NVAL(1,LF) = NVAL(1,LF) + 1
                  SNDATA(NVAL(1,LF),1,LF) = SQRT (AMP)
                  END IF
               IF (NUMPOL.GT.1) THEN
                  IF (RECR(RE2KOL+LF-1).NE.FBLANK) THEN
                     AMP = RECR(RE2KOL+LF-1)**2 + RECR(IM2KOL+LF-1)**2
                     NVAL(2,LF) = NVAL(2,LF) + 1
                     SNDATA(NVAL(2,LF),2,LF) = SQRT (AMP)
                     END IF
                  END IF
 20            CONTINUE
            END IF
 100     CONTINUE
C                                       Close
      CALL TABIO ('CLOS', 0, ISNRNO, SNBUFF, SNBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSE SN WRITE'
         GO TO 990
         END IF
      OPEN = .FALSE.
C                                       find medians
C                                       each individually
      IF ((DOMGM.GE.5) .OR. ((DOMGM.GE.4) .AND. (NUMPOL.EQ.1))) THEN
         MSGTXT = 'Apply median gain modulus to SN table by IF/pol'
         CALL MSGWRT (3)
         DO 120 LF = BIF,EIF
            DO 110 LP = 1,NUMPOL
               GMMO(LF,LP) = MEDIAN (NVAL(LP,LF), SNDATA(1,LP,LF))
 110           CONTINUE
            WRITE (MSGTXT,1110) LF, (GMMO(LF,LP), LP = 1,NUMPOL)
            CALL MSGWRT (4)
 120        CONTINUE
C                                       polarizations blended
      ELSE IF (DOMGM.GE.4) THEN
         MSGTXT = 'Apply median gain modulus to SN table by IF'
         CALL MSGWRT (3)
         DO 140 LF = BIF,EIF
            CALL RCOPY (NVAL(1,LF), SNDATA(1,1,LF), SNWORK(1))
            NN = NVAL(1,LF)
            CALL RCOPY (NVAL(2,LF), SNDATA(1,2,LF), SNWORK(NN+1))
            NN = NN + NVAL(2,LF)
            GMMO(LF,1) = MEDIAN (NN, SNWORK)
            GMMO(LF,2) = GMMO(LF,1)
            WRITE (MSGTXT,1130) LF, GMMO(LF,1)
            CALL MSGWRT (4)
 140        CONTINUE
C                                       all together
      ELSE
         MSGTXT = 'Apply global median gain modulus to SN table'
         IF (DOMGM.GT.2) CALL MSGWRT (3)
         NN = 0
         DO 160 LF = BIF,EIF
            DO 150 LP = 1,NUMPOL
               CALL RCOPY (NVAL(LP,LF), SNDATA(1,LP,LF), SNWORK(NN+1))
               NN = NN + NVAL(LP,LF)
 150           CONTINUE
 160        CONTINUE
         GMMOD = MEDIAN (NN, SNWORK)
         WRITE (MSGTXT,1160) GMMOD
         CALL MSGWRT (4)
         NN = EIF - BIF + 1
         CALL RCOPY (NN, GMMOD, GMMO(BIF,1))
         CALL RCOPY (NN, GMMOD, GMMO(BIF,2))
         END IF
      GO TO 999
C
 990  CALL MSGWRT (7)
      IF (OPEN) CALL TABIO ('CLOS', 0, ISNRNO, SNBUFF, SNBUFF, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GMEDIA ERROR',I4,' ON ',A)
 1110 FORMAT ('Median gain modulus IF',I3,' R',1PE12.4,' L',1PE12.4)
 1130 FORMAT ('Median gain modulus IF',I3,1PE12.4,' both pols')
 1160 FORMAT ('Median gain modulus overall',1PE12.4)
      END
      SUBROUTINE GMODIT (DISK, CNO, VER, SUBARR, BIF, EIF, CATIN, GMMO,
     *   IERR)
C-----------------------------------------------------------------------
C   GMODIT applies the mean gain modulus to the SN table in place.
C   Inputs:
C      DISK     I      Disk
C      CNO      I      Catalog number
C      VER      I      SN table version #
C      SUBARR   I      Subarray number
C      BIF      I      Begin IF
C      EIF      I      End IF
C      GMMO     R(*)   Mean gain modulus (MAXIF,2)
C      CATIN    I(*)   Input file catalog header
C   Output:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISK, CNO, VER, SUBARR, BIF, EIF, CATIN(*), IERR
      REAL      GMMO(MAXIF,2)
C
      INTEGER   LUN, ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT,
     *   NUMPOL, NUMIF, NUMNOD, IANT, SUBA, LF, SNBUFF(512), I, LUNTMP,
     *   NUMROW,  DATP(128,2), ANTKOL, SUBKOL, RE1KOL, IM1KOL, RE2KOL,
     *   IM2KOL, RECI(XCLRSZ), NCOL, NREC, NKEY
      REAL      GMMOD, RECR(XCLRSZ)
      LOGICAL   ISAPPL, OPEN
      DOUBLE PRECISION  RANOD, DECNOD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      EQUIVALENCE (RECI, RECR)
C-----------------------------------------------------------------------
      OPEN = .FALSE.
      LUN = LUNTMP (1)
C                                       open SN table to get parms
C                                       Open solution table
      CALL SNINI ('READ', SNBUFF, DISK, CNO, VER, CATIN, LUN, ISNRNO,
     *   SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD, RANOD,
     *   DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN SN TABLE FOR READ'
         GO TO 990
         END IF
      OPEN = .NOT.OPEN
C                                       pointers, number rows
      NUMROW = SNBUFF(5)
      ANTKOL = SNKOLS(SNIANT)
      SUBKOL = SNKOLS(SNISUB)
      RE1KOL = SNKOLS(SNRRE1)
      IM1KOL = SNKOLS(SNRIM1)
      RE2KOL = SNKOLS(SNRRE2)
      IM2KOL = SNKOLS(SNRIM2)
      CALL TABIO ('CLOS', 0, ISNRNO, SNBUFF, SNBUFF, I)
      OPEN = .NOT.OPEN
C                                       re-open for write
      CALL TABINI ('WRIT', 'SN', DISK, CNO, VER, CATIN, LUN, NKEY,
     *   NREC, NCOL, DATP, SNBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN SN TABLE FOR WRITE'
         GO TO 990
         END IF
      OPEN = .NOT.OPEN
      DO 100 I = 1,NUMROW
         ISNRNO = I
         CALL TABIO ('READ', 0, ISNRNO, RECR, SNBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ SN TABLE'
            GO TO 990
            END IF
         IANT = RECI(ANTKOL)
         SUBA = RECI(SUBKOL)
         IF ((SUBA.EQ.SUBARR) .OR. (SUBARR.LE.0)) THEN
            DO 20 LF = BIF,EIF
               IF (GMMO(LF,1).LE.0.0) GMMO(LF,1) = 1.0
               IF (RECR(RE1KOL+LF-1).NE.FBLANK) THEN
                  RECR(RE1KOL+LF-1) = RECR(RE1KOL+LF-1) / GMMO(LF,1)
                  RECR(IM1KOL+LF-1) = RECR(IM1KOL+LF-1) / GMMO(LF,1)
                  END IF
               IF (NUMPOL.GT.1) THEN
                  IF (GMMO(LF,2).LE.0.0) GMMO(LF,2) = 1.0
                  IF (RECR(RE2KOL+LF-1).NE.FBLANK) THEN
                     RECR(RE2KOL+LF-1) = RECR(RE2KOL+LF-1) / GMMO(LF,2)
                     RECR(IM2KOL+LF-1) = RECR(IM2KOL+LF-1) / GMMO(LF,2)
                     END IF
                  END IF
 20            CONTINUE
            END IF
         ISNRNO = I
         CALL TABIO ('WRIT', 0, ISNRNO, RECR, SNBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE SN TABLE'
            GO TO 990
            END IF
 100     CONTINUE
C                                       Close
      CALL TABIO ('CLOS', 0, ISNRNO, SNBUFF, SNBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSE SN WRITE'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (7)
      IF (OPEN) CALL TABIO ('CLOS', 0, ISNRNO, SNBUFF, SNBUFF, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GMODIT ERROR',I4,' ON ',A)
      END
      SUBROUTINE FLAGWT (OPCODE, BTIME, ETIME, IS, JS, VWT, MAXBL,
     *   NUMBL, NOIF, AVGIF, IC, DISK, CNO, CATIN, NFLAGS, IERR)
C-----------------------------------------------------------------------
C   Goes through weight array and flags data with wt < 0.  That with
C   wt=0 already flagged presumably
C   Inputs:
C      OPCODE    C*4    'CLOS' to close file, else write
C      BTIME     R      Interval begin time
C      ETIME     R      Interval end time
C      IS        I(*)   First ant. of baseline numbers
C      JS        I(*)   2nd ant. of baseline numbers
C      VWT       R(2,MAXBL,MAXFRQ) Visibility weights
C      MAXBL     I      Maximum number of baselines
C      NUMBL     I      Number of baselines
C      NOIF      I      Number of IFs
C      AVGIF     L      T => IFs averaged
C      IC        I      Stokes' number passed, -1 => both as separate
C                       sets of frequencies; 0=> data for poln.
C                       averaged. 1 = R, 2 = L, 3 = I
C      DISK      I      UV file disk number
C      CNO       I      UV file cno
C   In/out:
C      CATIN     I(*)   Header
C      NFLAGS    I      Count of records written to FG table
C   Outputs:
C      IERR      I      Error code
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   MAXBL, IS(*), JS(*), NUMBL, NOIF, IC, DISK, CNO,
     *   CATIN(*), NFLAGS, IERR
      DOUBLE PRECISION  BTIME, ETIME
      REAL      VWT(2,MAXBL,*)
      LOGICAL   AVGIF
C
      INTEGER   IA1, IA2, CIF, LIF1, LIF2, IST, JST, NST, ISTOFF, IBL
      LOGICAL   PFLAGS(4)
      REAL      WGT, T1, T2, DT
      INCLUDE 'CALFLAG'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C                                       (Loop) over Stokes' type
      IF (OPCODE.NE.'CLOS') THEN
         DT = 0.05 / (3600.0 * 24.0)
         IF (IC.LT.0) THEN
            NST = 2
         ELSE
            NST = 1
            END IF
         PFLAGS(3) = .TRUE.
         PFLAGS(4) = .TRUE.
         T1 = BTIME - DT
         T2 = ETIME + DT
         DO 60 JST = 1,NST
C                                       Ipol soln:
            IF ((IC.EQ.3) .OR. (IC.EQ.0)) THEN
               IST = 1
               PFLAGS(1) = .TRUE.
               PFLAGS(2) = .TRUE.
            ELSE IF (IC.LT.0) THEN
               IST = JST
               PFLAGS(IST) = .TRUE.
               PFLAGS(3-IST) = .FALSE.
            ELSE
               IST = IC
               PFLAGS(IST) = .TRUE.
               PFLAGS(3-IST) = .FALSE.
               END IF
C                                       Trap funny business with Stokes'
            ISTOFF = 0
            IF ((IC.LT.0) .AND. (JST.EQ.2)) ISTOFF = NOIF
C                                       Loop over IF
            DO 40 CIF = 1,NOIF
C                                       Copy data to XOBS
               IF (AVGIF) THEN
                  LIF1 = BIF
                  LIF2 = EIF
               ELSE
                  LIF1 = CIF + BIF - 1
                  LIF2 = LIF1
                  END IF
               DO 20 IBL = 1,NUMBL
                  WGT = VWT(1,IBL,CIF+ISTOFF)
                  IF (WGT.LT.0.0) THEN
                     IA1 = IS(IBL)
                     IA2 = JS(IBL)
                     CALL FLAGER ('WRIT', FLGLUN, DISK, CNO, FGVER,
     *                  FGVERO, LFGRNO, FGKOLS, FGNUMV, 0, SUBARR,
     *                  FRQSEL, IA1, IA2, T1, T2, LIF1, LIF2, 1, 0,
     *                  PFLAGS, REASON, 0.0, CATIN, FGBUFL, IERR)
                     IF (IERR.NE.0) GO TO 999
                     NFLAGS = NFLAGS + 1
                     END IF
 20               CONTINUE
 40            CONTINUE
 60         CONTINUE
      ELSE
         CALL FLAGER ('CLOS', FLGLUN, DISK, CNO, FGVER, FGVERO, LFGRNO,
     *      FGKOLS, FGNUMV, 0, SUBARR, FRQSEL,IA1, IA2, T1, T2, LIF1,
     *      LIF2, 1, 0, PFLAGS, REASON,0.0, CATIN, FGBUFL, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE FLAGER (OPCODE, LUN, DISK, CNO, VERI, VER, LFGRNO,
     *   FGKOLS, FGNUMV, ID, SUBA, FQID, ANT1, ANT2, BTIME, ETIME, BIF,
     *   EIF, BCHAN, ECHAN, PFLAGS, REASON, DOIFS, CATUV, BUFF, IRET)
C-----------------------------------------------------------------------
C   Updates the Flag (FG) table. Adapted from FLAGUP
C   One entry is made indicating a visibility to be rejected.
C   The FLAG table will be opened on the first call but a final call
C   with OPCODE='CLOS' is required to close the file.
C   Inputs:
C      OPCODE   C*4      Operation desired, 'CLOS'=>close file
C                        Anything else = 'FLAG'
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VERI     I        Input version number
C      VER      I        FG file version
C      LUN      I        Logical unit number to use
C      ID       I(NID)   List of source ID as defined in SOURCE table
C      NID      I        Number of elements in ID
C      SUBA     I        Subarray number.
C      FQID     I        Freqid number
C      ANT1     I        First antenna number in baseline
C      ANT2     I        Second antenna number in baseline
C      BTIME    R        Start time of data to be flagged (Days)
C      ETIME    R        End time of data to be flagged (Days)
C      BIF      I        First IF number to flag. 0=>all
C      EIF      I        Last IF number to flag. 0=>all higher than IFS(1)
C      BCHAN    I        First channel number to flag. 0=>all
C      ECHAN    I        Last channel number to flag. 0=>all higher.
C      PFLAGS   L(4)     Correlator flags
C      REASON   C*24     Reason for flagging blank => ignore for unflag.
C      DOIFS    R        > 0 flag all IFs
C   Input/Output:
C      CATUV    I(256)   Header for disk file to get FG table
C      BUFF     I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      LFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(*)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(*)     Element count in each column.
C   Output:
C      IRET     I        Error code, 0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, REASON*24
      INTEGER   LUN, DISK, CNO, VERI, VER, LFGRNO, FGKOLS(*), FGNUMV(*),
     *   ID, SUBA, FQID, ANT1, ANT2, BIF, EIF, BCHAN, ECHAN, CATUV(256),
     *   BUFF(*), IRET
      REAL      BTIME, ETIME, DOIFS
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TREAS*24, CTEMP*12
      INTEGER   IDT, SUBT, ANTS(2), IFS(2), CHANS(2), IDUM, FIND, I,
     *   BUFF2(512), LUN2, IFGKOL(MAXFGC), IFGNUM(MAXFGC), NROW, IFQ,
     *   IFGRNO
      LOGICAL   PFLAGS(4), TFLAGS(4), FIRST
      REAL      TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE FIRST
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
C                                       See if table open - check FTAB
      IF (OPCODE.NE.'CLOS') THEN
         FIND = BUFF(82)
C                                       Open file
         IF ((FIND.LT.0) .OR. (FIND.GT.10000) .OR. (LUN.NE.FTAB(FIND)))
     *      THEN
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'WRIT', BUFF, IRET)
C
            CALL FLGINI ('WRIT', BUFF, DISK, CNO, VER, CATUV, LUN,
     *         LFGRNO, FGKOLS, FGNUMV, IRET)
C                                       Report on the need for flagging
            WRITE (MSGTXT,1000) VER
C            IF (.NOT.FIRST) WRITE (MSGTXT,1001) VER
            IF (FIRST) CALL MSGWRT (2)
            IF (IRET.NE.0) GO TO 999
C                                       Copy the old file
            IF ((FIRST) .AND. (VERI.GT.0)) THEN
               LUN2 = LUN + 1
               CALL FLGINI ('READ', BUFF2, DISK, CNO, VERI, CATUV, LUN2,
     *            IFGRNO, IFGKOL, IFGNUM, IRET)
               IF (IRET.NE.0) GO TO 999
               NROW = BUFF2(5)
               WRITE (MSGTXT,1002) NROW, VERI, VER
               CALL MSGWRT (2)
               DO 20 I = 1,NROW
                  CALL TABFLG ('READ', BUFF2, IFGRNO, IFGKOL, IFGNUM,
     *               IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *               TREAS, IRET)
                  IF (IRET.GT.0) GO TO 999
                  IF (IRET.EQ.0) THEN
                     CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV,
     *                  IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *                  TREAS, IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
 20               CONTINUE
               CALL TABIO ('CLOS', 0, IFGRNO, BUFF2, BUFF2, I)
               END IF
            FIRST = .FALSE.
C                                       Mark as unsorted
            BUFF(43) = 0
            BUFF(44) = 0
            END IF
C                                       Set up for flagging
         ANTS(1) = ANT1
         ANTS(2) = ANT2
         TIMER(1) = BTIME
         TIMER(2) = ETIME
         IF (DOIFS.LE.0.0) THEN
            IFS(1) = BIF
            IFS(2) = EIF
         ELSE
            IFS(1) = 1
            IFS(2) = 0
            END IF
         CHANS(1) = BCHAN
         CHANS(2) = ECHAN
C                                       Flag table entry.
         CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV, ID, SUBA,
     *      FQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
C                                       Close
      ELSE
         CALL TABFLG ('CLOS', BUFF, LFGRNO, FGKOLS, FGNUMV, IDT, SUBT,
     *      FQID, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
C                                       Clear write status
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'CLWR', BUFF, IRET)
         BUFF(82) = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Found some bad data, will write flags to table FG', I4)
C1001 FORMAT ('Found some bad data, will add   flags to table FG', I4)
 1002 FORMAT ('Copy',I8,' rows from FG vers',I3,' to',I3)
      END
      SUBROUTINE CHWANT (NCH, NIF, CHNSEL, CHFLGS)
C-----------------------------------------------------------------------
C   Makes a mask of the desired channels
C   Inputs:
C      NCH      I            Number spectral chans
C      NIF      I            Number IFs
C      CHNSEL   I(3,20,*)    Start, stop, incr 20 sets per IF
C   Outputs
C      CHFLGS   R(*,*)       1.0 => use, 0.0 => don't use
C-----------------------------------------------------------------------
      INTEGER   NCH, NIF, CHNSEL(3,20,*)
      REAL      CHFLGS(NCH,NIF)
C
      INTEGER   I, J, K
C-----------------------------------------------------------------------
      J = NCH * NIF
      CALL RFILL (J, 0.0, CHFLGS)
      DO 30 K = 1,NIF
         DO 20 J = 1,20
            IF ((CHNSEL(1,J,K).GT.0) .AND. (CHNSEL(3,J,K).GT.0) .AND.
     *         (CHNSEL(2,J,K).GE.CHNSEL(1,J,K))) THEN
               DO 10 I = CHNSEL(1,J,K),CHNSEL(2,J,K),CHNSEL(3,J,K)
                  CHFLGS(I,K) = 1.0
 10               CONTINUE
               END IF
 20         CONTINUE
 30      CONTINUE
C
 999  RETURN
      END
