LOCAL INCLUDE 'CPASS.INC'
C                                                         Include CPASS
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       Local include for CPASS
      CHARACTER NAMEIN*12, CLAIN*6, XSTOK*4, NAME2*12, CLAS2*6,
     *   XCALCO*4, NAME3*12, CLAS3*6, CMETH*4, CMOD*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSTOK, XNAME2(3),
     *   XCLAS2(2), XXCALC, XNAME3(3), XCLAS3(2), XMETH, XCMOD,
     *   CATH3(256)
      INTEGER   SEQIN, SEQ2, SEQ3, DISKIN, DISK2, DISK3, CNOIN,
     *   CNOIN2, CNOIN3, REFANT, CATIN(256), CATI3(256), PRTLV, VISDSK,
     *   VISCNO, CCTVER, VER, NUMHIS, CHNSEL(3,20,MAXIF), NUMBL,
     *   IS(MXBASE), JS(MXBASE), LUNI, FINDI, LUN3, FIND3, LBCHAN,
     *   LECHAN, BPOVER
      REAL     XSIN, XDISIN, XQUAL, XUVR(2), XTIME(8), XBAND, XFREQ,
     *   XFQID, XBIF, XEIF, XBCHAN, XECHAN, XSUBA, XANTNS(50), XS2, XD2,
     *   XVER, XNCOMP(MAXAFL), XFLUX, XNMAP, SMODEL(7), XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSOLIN, XREFAN,
     *   XBOVER, XSMOTH(3), XANTW(30), APARM(11), CPARM(10),
     *   XCHNS(4,20), DOSCAL, XSPEC, XCURVE(3), XS3, XD3, XBADD(10)
      REAL   SOLINT, BUFF1(UVBFSL), BUFF2(UVBFSL), ANTWT(MAXANT),
     *   AMPMAX, PHMAX, CATR3(256), PREAVG, FINC(MAXIF), ACURVE(4)
      DOUBLE PRECISION CATD3(128), DTOLB, FOFF(MAXIF)
      INTEGER   REFUSE(MAXANT), NCOMP(MAXFLD), NPOLYB, NITERB,
     *   INITVR, IBSOLV, NCHMAX, NIFMAX, ISBAND(MAXIF)
      LOGICAL ACONLY, DOMODL, SINGLE, DONDX, PHSONL, DIVCH0,
     *   AVGALL, LINEAR, IQUV, SCALAR, BPNORM, DOCHN, EXTCH0, VLBA,
     *   WSCALE
      INTEGER   JBUFSZ, NUMFRQ, BNUMIF, IKLOCF
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XXCALC, XUVR, XTIME, XXSTOK, XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XBCHAN, XECHAN, XSUBA, XANTNS, XNAME2, XCLAS2, XS2, XD2, XVER,
     *   XNCOMP, XFLUX, XNMAP, XMETH, XCMOD, SMODEL, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSOLIN, XREFAN,
     *   XBOVER, XSMOTH, XANTW, APARM, CPARM, XCHNS, DOSCAL, XSPEC,
     *   XCURVE, XNAME3, XCLAS3, XS3, XD3, XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN, XSTOK, NAME2, CLAS2, XCALCO,
     *   NAME3, CLAS3, CMETH, CMOD
      COMMON /BPPARM/ DTOLB, NUMHIS, CHNSEL, BUFF1, BUFF2,
     *   SOLINT, ANTWT, PREAVG, AMPMAX, PHMAX, REFUSE, NCOMP, ACONLY,
     *   DOMODL, SINGLE, DONDX, EXTCH0, PHSONL, DIVCH0, ACURVE,
     *   AVGALL, LINEAR, IQUV, SCALAR, BPNORM, REFANT, CATIN, PRTLV,
     *   VISDSK, VISCNO, JBUFSZ, VER, NUMFRQ, NUMBL, IS, JS, LUNI,
     *   FINDI, LUN3, FIND3, BNUMIF, IKLOCF, NPOLYB, NITERB, INITVR,
     *   IBSOLV, NCHMAX, NIFMAX, VLBA, WSCALE, DOCHN, LBCHAN, LECHAN,
     *   BPOVER, SEQIN, SEQ2, SEQ3, DISK2, DISK3, DISKIN, CNOIN, CNOIN2,
     *   CNOIN3, CCTVER
      COMMON /MAP3HD/ CATI3
      COMMON /BPFRQS/ FOFF, FINC, ISBAND
      EQUIVALENCE (CATI3, CATR3, CATH3, CATD3)
C                                                         End CPASS
LOCAL END
LOCAL INCLUDE 'EZERO.INC'
      CHARACTER ISORT3*2
      REAL      BUFF3(UVBFSL)
      INTEGER   ILOCT3, ILOCB3, ILOCS3, ILOCQ3, ILCA13, ILCA23, ILCSA3,
     *   JLOCS3, JLOCF3, JLOCI3, INCS3, INCF3, INCIF3, ICOR03, NPARM3,
     *   NCHAN3, NVIS3, NCOR3, BUFSZ3, NIO3, INCOR, KNIF
      COMMON /EZCHR/ ISORT3
      COMMON /EZPARM/ BUFF3, ILOCT3, ILOCB3, ILOCS3, ILOCQ3, ILCA13,
     *   ILCA23, ILCSA3, JLOCS3,JLOCF3, JLOCI3, INCS3, INCF3, INCIF3,
     *   ICOR03, NPARM3, NVIS3, NCOR3, NCHAN3, BUFSZ3, NIO3, INCOR, KNIF
LOCAL END
LOCAL INCLUDE 'PPDAT.INC'
      INTEGER MXSPEC, MXTERM, NSIZBP, MXPARM
C                                       Max # BP entries per soln.
C                                       interval
      PARAMETER (MXSPEC = 13000)
C                                       Max no. polynomial terms
      PARAMETER (MXTERM = MAXCHA)
C                                       Size of BP visibility arrays
      PARAMETER (NSIZBP = 1500000)
C                                       Max # parameters in fit
      PARAMETER (MXPARM = 2500)
LOCAL END
LOCAL INCLUDE 'BPDAT.INC'
C                                           BP data common
      REAL XREDAT(NSIZBP), XIMDAT(NSIZBP), XWGTBP(NSIZBP),
     *   SHIFTA(MXSPEC), SHIFTB(MXSPEC), BPZERO(MXPARM)
      INTEGER IBASL(MXSPEC), JBASL(MXSPEC), IFITAN(MAXANT),
     *   IANDX(MAXANT), ISTRE(MAXANT), ISTIM(MAXANT),
     *   NBASL, NBPCHN, NBPANT, NTERMS, NFIT, IFITYP, INITYP, OFORM
      COMMON /BPDAT/ XREDAT, XIMDAT, XWGTBP, SHIFTA, SHIFTB,
     *   BPZERO, IBASL, JBASL, IFITAN, IANDX, ISTRE, ISTIM,
     *   NBASL, NBPCHN, NBPANT, NTERMS, NFIT, IFITYP, INITYP, OFORM
LOCAL END
      PROGRAM CPASS
C-----------------------------------------------------------------------
C! CPASS generates polynomial BandPass tables from uv-data.
C# Calibration Spectral VLA VLB EXT-appl AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2003, 2005-2010, 2012-2015, 2017-2018
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
C   Task:  To create a 'BP' (bandpass) table which will contain
C   a polynomial approximation to the complex bandpass response
C   function at each antenna.
C     INNAME.....Input UV file name (name).      Standard defaults.
C     INCLASS....Input UV file name (class).     Standard defaults.
C     INSEQ......Input UV file name (seq. #).    0 => highest.
C     INDISK.....Disk drive # of input UV file.  0 => any.
C     SOURCES....Source list.  If the data is a multi-source file
C                CPASS will form the cross-power spectrum for the
C                first source specified. If the data is a single
C                source file no source name need be specified.
C     UVRANGE....Range (min, max) of projected baselines to include
C                0,0 => all baselines (units: klamda)
C     TIMERANG...Time range of the data to be selected. In order:
C                Start day, hour, min. sec,
C                end day, hour, min. sec. Days relative to ref.
C                date.
C     STOKES.....The desired Stokes type of the output data:
C                'I','V','Q','U','IQU','IQUV','IV','RR','LL','RL',
C                'LR','HALF' (=RR,LL), 'FULL' (=RR,LL,RL,LR)
C     BIF........First IF to copy. 0=>all.
C     EIF........Highest IF to copy. 0=>all higher than BIF
C     BCHAN......First channel to select. 0=>all.
C     ECHAN......Highest channel to select.
C     SUBARRAY...Subarray number to select. 0=>all.
C     ANTENNAS...A list of the antennas to be plotted.
C                If any number is negative then all antennas listed
C                are NOT to be selected and all others are.
C                                      CLEAN map (optional)
C     IN2NAME....Cleaned map name (name)
C     IN2CLASS...Cleaned map name (class)
C     IN2SEQ.....Cleaned map name (seq. #)
C     IN2DISK....Cleaned map disk unit #
C     INVERS.....CC file version #.
C     NCOMP......# comps to use for model.
C                1 value per field
C     FLUX       Lowest CC comp to use
C     NMAPS......No. Clean map files
C     SMODEL.....Source model to use instead of CLEAN map
C     DOCALIB....If true (>0) then calibrate the data using
C                information in the specified Cal (CL or SN).
C     GAINUSE....version number of the CL table to apply to
C                multisource files or the SN table for single
C                source files.  0 => highest.
C     FLAGVER....specifies the version of the flagging table to be
C                applied. 0 => highest numbered table.
C                <0 => no flagging to be applied.
C     SOLINT.....the interval over which to average the data
C                before solving for the bandpasses. (0 => scan)
C                -1 => whole timerange
C     REFANT.....the antenna to use as a reference in the
C                least squares solution.
C     BPVER......the version of the BP table to fill. (0 => 1)
C     SMOOTH.....Smoothing function.
C     ANTWT......Antenna weights for up to 30 antennas. (0=>1.0)
C     APARM......Control information:
C                (1) > 0 => fill BP table with autocorrelation
C                      data only, ignoring the phases.
C                (2)   print level
C                (3) > 0 => do not divide visibility data by
C                      a model of the source.
C                (4) > 0 => store phases only in the BP table.
C                (5) = 0 => divide by channel 0
C                (6) = min. amp. closure error to print
C                (7) = min phase closure error to print
C                (8) > 0 do scalar average
C                (10) > 0 => normalized BP table.
C                (11) > 0 => special mode for VLBI
C     CPARM      (1) No. of polynomial coefficients.
C                (2) Maximum number of iterations.
C                (3) Convergence tolerance.
C                (4) Pre-average interval (seconds)
C                (5) Fit type (1= Re/Im; 2= Ampl./Phase)
C                (6) BP table for initial values.
C                (7) > 0 => fit phase only.
C                (8) > 0 => Do not autoscale
C                (9) > 0 no local rms, > 1.5 weights -> 1.0
C     CHANSEL....Array of up to ten sets of start,stop and increment
C                channel numbers.
C     BADDISK....A list of disks on which scratch files are not to
C                be placed.  This will not affect the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET, MAXBL, MAXFRQ, MAXIFS, MAXTEL
      INCLUDE 'CPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'CPASS '/
C-----------------------------------------------------------------------
C                                               Get input parameters
      CALL BPSIN (PRGM, MAXBL, MAXFRQ, MAXIFS, MAXTEL, IRET)
      IF (IRET.NE.0) GO TO 990
C                                               Transfer needed data to
C                                               scratch with optional
C                                               calibration.
      CALL BPSEL (IRET)
      IF (IRET.NE.0) GO TO 990
C                                               Divide data by model if
C                                               neccessary.
      IF (DOMODL .AND. (.NOT.DIVCH0)) CALL BPMOD (IRET)
      IF (IRET.NE.0) GO TO 990
C                                               Calculate the bandpass
C                                               functions and write them
C                                               to the BP table
      CALL BPSOL (MAXFRQ, MAXTEL, IRET)
      IF (IRET.NE.0) GO TO 990
C                                               Write history
      CALL BPHIS
C                                               Close down files etc.
 990  CALL DIE (IRET, BUFF1)
C
      STOP
      END
      SUBROUTINE BPSIN (PRGN, MAXBL, MAXFRQ, MAXIFS, MAXTEL, JERR)
C-----------------------------------------------------------------------
C  BPSIN get input parameters for CPASS and finds input file.
C  All selection criteria are filled into commons in D/CSEL.INC
C
C  Inputs:
C      PRGN      C*6)     Program name (2chars/word)
C  Outputs:
C      MAXBL     I        Max possible # of baselines
C      MAXFRQ    I        Max possible # of channels
C      MAXIFS    I        Max possible # of IFs
C      MAXTEL    I        Max possible # of antennas
C      JERR      I        Error code : 0 => OK
C                                      5 => catalog troubles
C                                      8 => can't start
C-----------------------------------------------------------------------
      CHARACTER  STAT*4, PRGN*6, UTYPE*2
      INTEGER  JERR,  NPARM, IROUND, IERR, IRET, I, LUNTB, NANT, NFREQ,
     *   MXANT, MXFLD, IUSER, MAXBL, MAXFRQ, MAXIFS, MAXTEL, LUN, J,
     *   LCOR0, ANVER, K, K1, K2
      REAL      CATR(256)
      LOGICAL   T, F, TABLE, MULTI, FITASC, MATCH, SINGL3, FAIL, NXPRS
      DOUBLE PRECISION CATD(128)
      INCLUDE 'CPASS.INC'
      INTEGER   NW(MAXIF)
      INCLUDE 'PPDAT.INC'
      INCLUDE 'EZERO.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'BPDAT.INC'
      EQUIVALENCE (CATR,CATD,CATBLK)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNTB / 29 /
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSL*2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      MXFLD = MAXAFL
      NPARM = 384 + MXFLD
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
      MSGTXT = 'You are using a non-standard program'
      CALL MSGWRT (2)
      MSGTXT = 'Bandpass response function generator'
      CALL MSGWRT (2)
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAME3, NAME3)
      CALL H2CHR (6, 1, XCLAS3, CLAS3)
      CALL H2CHR (4, 1, XMETH, CMETH)
      CALL H2CHR (4, 1, XCMOD, CMOD)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQ2  = IROUND (XS2)
      SEQ3  = IROUND (XS3)
      DISKIN = IROUND (XDISIN)
      DISK2  = IROUND (XD2)
      DISK3  = IROUND (XD3)
      CCTVER = IROUND (XVER)
      CCTVER = MAX (0, CCTVER)
      IUSER = NLUSER
      IF (DOSCAL.LT.0.0) THEN
         XSPEC = 0.0
         XCURVE(1) = 0.0
         XCURVE(2) = 0.0
         XCURVE(3) = 0.0
         END IF
      CALL RCOPY (3, XCURVE, ACURVE)
      ACURVE(4) = 0
C                                       Do we get external channel 0
      EXTCH0 = T
      IF ((NAME3.EQ.' ') .AND. (CLAS3.EQ.' ')) EXTCH0 = F
      IF (EXTCH0) THEN
         CNOIN3 = 1
         UTYPE = 'UV'
         CALL CATDIR ('SRCH', DISK3, CNOIN3, NAME3, CLAS3, SEQ3, UTYPE,
     *      NLUSER, STAT, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR, NAME3, CLAS3, SEQ3, DISK3,
     *         NLUSER
            GO TO 990
            END IF
C                                       Read catalogue header
         CALL CATIO ('READ', DISK3, CNOIN3, CATBLK, 'REST',
     *         BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
C                                       Determine if multi-source file
         CALL ISTAB ('SU', DISK3, CNOIN3, 1, LUNTB, BUFF1, TABLE,
     *      MULTI, FITASC, IERR)
C                                       Get uv header info.
         CALL UVPGET (JERR)
         IF (JERR.NE.0) GO TO 999
         SINGL3 = .NOT. MULTI .OR. (ILOCSU.LT.0)
         CALL COPY (256, CATBLK, CATI3)
         NCHAN3 = CATBLK(KINAX+JLOCF)
         IF (NCHAN3.GT.1) THEN
            WRITE (MSGTXT,1150) NCHAN3
            JERR = 1
            GO TO 990
            END IF
         ISORT3 = ISORT
         IF (ISORT3.NE.'TB') THEN
            MSGTXT = 'EXTERNAL CHANNEL 0 FILE FILE NOT IN TB SORT ORDER'
            JERR = 1
            GO TO 990
            END IF
         ILOCT3 = ILOCT
         ILOCB3 = ILOCB
         ILOCS3 = ILOCSU
         ILOCQ3 = ILOCFQ
         ILCA13 = ILOCA1
         ILCA23 = ILOCA2
         ILCSA3 = ILOCSA
         JLOCS3 = JLOCS
         JLOCF3 = JLOCF
         JLOCI3 = JLOCIF
         INCS3 = INCS
         INCF3 = INCF
         INCIF3 = INCIF
         IF (CATBLK(KINAX).LE.1) THEN
            INCS3 = INCS3 * 3
            INCF3 = INCF3 * 3
            INCIF3 = INCIF3 * 3
            END IF
         ICOR03 = ICOR0
         NPARM3 = NRPARM
         LREC3 = LREC
         NVIS3 = NVIS
         NCOR3 = NCOR
         LUN3 = 49
         END IF
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
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Determine if multi-source file
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNTB, BUFF1, TABLE,
     *   MULTI, FITASC, IERR)
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      SINGLE = .NOT. MULTI .OR. (ILOCSU.LT.0)
C                                       Go no further if there isn't an
C                                       NX table for multi source
      IF (.NOT.SINGLE) THEN
         CALL ISTAB ('NX', DISKIN, CNOIN, 1, LUNTB, BUFF1, TABLE,
     *      NXPRS, FITASC, IERR)
         IF (.NOT.NXPRS) THEN
            MSGTXT = 'There is no NX table for the multi-channel'
            CALL MSGWRT (8)
            MSGTXT = 'multi-source file; run INDXR'
            JERR = 1
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      INCOR = NCOR
      IKLOCF = JLOCF
      NCHMAX = CATBLK(KINAX+JLOCF)
      NIFMAX = CATBLK(KINAX+JLOCIF)
C                                       Check compatibility of two files
      IF (EXTCH0) THEN
         IF ((ICOR03.NE.ICOR0) .OR. (NCOR3.NE.NCOR)) THEN
            MSGTXT = 'LINE & CHANNEL 0 FILES HAVE INCOMPATIBLE' //
     *         ' POLARIZATION'
            JERR = 1
            GO TO 990
            END IF
         IF (NVIS3.NE.NVIS) THEN
            MSGTXT = 'LINE & CHANNEL 0 FILES HAVE DIFFERENT NO. VIS.' //
     *         ' POINTS'
            JERR = 1
            GO TO 990
            END IF
         FAIL = T
         IF (SINGLE .AND. SINGL3) FAIL = F
         IF ((.NOT.SINGLE) .AND. (.NOT.SINGL3)) FAIL = F
         IF (FAIL) THEN
            MSGTXT = 'LINE & CHANNEL 0 FILES NOT BOTH MULTI-SOURCE' //
     *         ' FORMAT'
            JERR = 1
            GO TO 990
            END IF
         END IF
C                                       BADDISK
      DO 10 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 10      CONTINUE
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 20 I= 1, 30
         CALL H2CHR (16, 1, XXSOUR(1,I), SOURCS(I))
         CALSOU(I) = SOURCS(I)
 20      CONTINUE
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
      CALL RCOPY (8, XTIME, TIMRNG)
      CALL RCOPY (2, XUVR, UVRNG)
      DO 30 I = 1, 50
         ANTENS(I) = IROUND (XANTNS(I))
 30      CONTINUE
      STOKES = XSTOK
      NFREQ = CATBLK(KINAX+JLOCF)
      LBCHAN = IROUND (XBCHAN)
      LBCHAN = MAX (1, MIN (LBCHAN, CATBLK(KINAX+JLOCF)))
      LECHAN = IROUND (XECHAN)
      IF (LECHAN.LT.LBCHAN) LECHAN = CATBLK(KINAX+JLOCF)
      LECHAN = MAX (1, MIN (LECHAN, CATBLK(KINAX+JLOCF)))
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
      NUMFRQ = ECHAN - BCHAN + 1
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
         END IF
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOAPPL = F
      DOPOL = IROUND (XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      BLVER = IROUND (XBLVER)
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      DXTIME = 0.0
C                                       Check type of polarization
      IF (CATD(KDCRV+JLOCS).GT.0.0D0) LCOR0 = CATD(KDCRV+JLOCS) + 0.5D0
      IF (CATD(KDCRV+JLOCS).LT.0.0D0) LCOR0 = CATD(KDCRV+JLOCS) - 0.5D0
C                                       Linear polarized data (X-Y)
C                                       assume that it is being
C                                       calibrated and will be changed
C                                       to RR,LL,RL,LR data.
      LINEAR = LCOR0.LE.-5
      IQUV = LCOR0.GT.1
C     IF (LINEAR .AND. (DOPOL.LE.0)) THEN
C        MSGTXT = 'Your data are linearly polarized, but DOPOL=.FALSE.'
C        CALL MSGWRT (6)
C        MSGTXT = 'The results of CPASS will be incorrect'
C        CALL MSGWRT (6)
C        JERR = 1
C        GO TO 999
C        END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                        Reference antenna
      REFANT = IROUND (XREFAN)
C                                        BP table version to generate
      BPOVER = IROUND (XBOVER)
C                                        Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                        Solution interval
      AVGALL = XSOLIN .LT. 0.0
      IF (XSOLIN.LT.0.0) THEN
         SOLINT = 1.0E10
      ELSE
         SOLINT = XSOLIN / (24.0 * 60.0)
         END IF
C                                       Default multisource = scan
      IF (SOLINT.LE.1.0E-10) THEN
         SOLINT = 0.1
         IF (SINGLE) SOLINT = 1.0E10
         END IF
C                                        Use autocorrelations only ?
      ACONLY = APARM(1) .EQ. 1
      DOACOR = ACONLY
      IF (DOACOR) DOXCOR = .FALSE.
C                                        Print level
      PRTLV = IROUND (APARM(2))
C                                        Divide by model ?
      DOMODL = F
      DOMODL = APARM(3) .LE. 0.0
C                                        Phases only ?
      PHSONL = F
      PHSONL = APARM(4) .GT. 0.0
C                                       Closure errors
      AMPMAX = APARM(6)
      PHMAX = APARM(7)
C                                       Scalar average?
      SCALAR = APARM(8).GT.0.0
C                                       VLBI style normalization
      BPNORM = APARM(10).GT.0.0
      IF (BPNORM) THEN
         MSGTXT = 'Resultant bandpass table will be normalized'
         CALL MSGWRT (4)
         END IF
C                                       Divide by channel 0?
      DIVCH0 = APARM(5) .EQ. 0.0
      IF (ACONLY) DIVCH0 = F
      IF (DIVCH0 .OR. EXTCH0) THEN
         IF (DOCAL .AND. (APARM(11).LE.0)) THEN
            MSGTXT = 'DOCALIB=1 & BPASSPRM(5)=0 incompatible, setting '
     *         // 'DOCALIB=-1'
            CALL MSGWRT (6)
            DOCAL = .FALSE.
            END IF
         END IF
      IF (DIVCH0 .AND. (.NOT.EXTCH0)) THEN
         IF ((BCHAN.NE.1) .AND. (ECHAN.NE.CATBLK(KINAX+JLOCF))) THEN
            BCHAN = 1
            ECHAN = CATBLK(KINAX+JLOCF)
            MSGTXT = 'Divide by chn 0 requires full spectrum'
            CALL MSGWRT (6)
            WRITE (MSGTXT,1125) BCHAN, ECHAN
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Additional parameters for
C                                       the polynomial fit.
C
C                                       No. of terms in fit:
C                                       def: 20
      NPOLYB = CPARM(1) + 0.1
      IF (NPOLYB.LE.0) NPOLYB = 20
C                                       Max. no. of iterations
C                                       def: 50
      NITERB = CPARM(2) + 0.1
      IF (NITERB.LE.0) NITERB = 50
C                                       Convergence tolerance:
C                                       def: 0.001
      DTOLB = CPARM(3)
      IF (DTOLB.LE.0.0) DTOLB = 1.0D-3
C                                       Pre-average interval (s)
C                                       Default (AC: 5 min;
C                                       XC 15 min)
      PREAVG = CPARM(4)
      IF (PREAVG.LE.0.0) THEN
         PREAVG = 900.0
         IF (ACONLY) PREAVG = 300.0
         END IF
      PREAVG = PREAVG / 86400.0
C                                       Fit type: def: A&P
      IFITYP = CPARM(5)
      IF (IFITYP.LE.0) IFITYP = 2
      IFITYP = MIN (IFITYP, 2)
      IFITYP = MAX (IFITYP, 1)
C                                       BP table for initial val.
      INITVR = CPARM(6)
C                                       Solve for phase only ?
      IBSOLV = CPARM(7)
C                                       Autoscale ? Def: true.
      WSCALE = (CPARM(8).GT.0)
      IF (CPARM(8).EQ.0.0) WSCALE = .TRUE.
C                                       Format as BP table?
      OFORM = 1
      IF (CPARM(10).GT.0.0) OFORM = 0
C                                       Channel selection for
C                                       channel 0
      IF (.NOT.EXTCH0) THEN
         I = 60 * MAXIF
         CALL FILL (I, 0, CHNSEL)
         CALL FILL (MAXIF, 0, NW)
         DO 60 I = 1,20
            K = IROUND (XCHNS(2,I))
            IF (K.LE.0) GO TO 65
            K = IROUND (XCHNS(4,I))
            IF ((K.LE.0) .OR. (K.GT.MAXIF)) THEN
               K1 = 1
               K2 = MAXIF
            ELSE
               K1 = K
               K2 = K
               END IF
            DO 55 K = K1,K2
               NW(K) = NW(K) + 1
               DO 50 J = 1,3
                  CHNSEL(J,NW(K),K) = IROUND (XCHNS(J,I))
                  IF (CHNSEL(J,NW(K),K).LT.0) CHNSEL(J,NW(K),K) = 0
 50               CONTINUE
               IF (CHNSEL(3,NW(K),K).EQ.0) CHNSEL(3,NW(K),K) = 1
 55            CONTINUE
 60         CONTINUE
 65      J = CATBLK(KINAX+JLOCF)
         DO 75 K = 1,MAXIF
            IF (NW(K).LE.0) THEN
               NW(K) = 1
               CHNSEL(1,1,K) = (J + 1) / 8
               CHNSEL(2,1,K) = (((J+1)*7)/8) + 1
               CHNSEL(3,1,K) = 1
            ELSE
               DO 70 I = 1,NW(K)
                  CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), J))
                  IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K)) CHNSEL(2,I,K) = J
                  CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), J))
 70               CONTINUE
               END IF
 75         CONTINUE
         END IF
C                                        Default ant. wt = 1.0
      MXANT = MAXANT
      CALL RFILL (MXANT, 1.0, ANTWT)
      DO 100 I = 1, 30
         ANTWT(I) = XANTW(I)
         IF (XANTW(I).LE.0.0) ANTWT(I) = 1.0
 100     CONTINUE
C                                        Fill in ref ant use
      CALL FILL (MXANT, 0, REFUSE)
C                                        Get # ant. from AN file.
      ANVER = MAX (1, SUBARR)
      CALL ANMAXA (DISKIN, CNOIN, ANVER, CATBLK, NANT, IRET)
      IF ((NANT.LE.0) .OR. (IRET.NE.0)) THEN
         WRITE (MSGTXT,1100) NANT, IRET
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Determine max. bl., time.
      NUMANT = NANT
      BNUMIF = EIF - BIF + 1
      KNIF = BNUMIF
      MAXBL = (NANT * (NANT - 1)) / 2
      MAXTEL = NANT
      MAXFRQ = NUMFRQ
      MAXIFS = BNUMIF
C                                       VLBA ?
      VLBA = ANAME(1:4) .EQ. 'VLBA'
      ISVLBA = VLBA
      IF (VLBA) THEN
         MSGTXT = 'Array name in AN table is VLBA'
         CALL MSGWRT (4)
         MSGTXT = 'Will assume this is data from the VLBA correlator'
         CALL MSGWRT (4)
         MSGTXT = 'and that it is all fringe-rotated to Earth Centre'
         CALL MSGWRT (4)
         MSGTXT = 'Will remove effects of that fringe-rotation'
         CALL MSGWRT (4)
         MSGTXT = 'to provide consistent bandpass calibration'
         CALL MSGWRT (4)
         MSGTXT = 'If incorrect, abort and change array name keyword'
         CALL MSGWRT (4)
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATUV)
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPSIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1100 FORMAT ('BPSIN: ANMAXA RETURNS NANT=',I3,' ERROR',I5)
 1125 FORMAT ('Resetting BCHAN = ',I3,' ECHAN = ',I4)
 1150 FORMAT ('EXTERNAL CHANNEL 0 FILE HAS ',I3,' CHANNELS ',
     *   '- CHECK INPUTS')
      END
      SUBROUTINE BPHIS
C-----------------------------------------------------------------------
C   BPHIS copies and updates the history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72
      INTEGER   LUN2, LIMIT, IERR, I, TIME(3), DATE(3), J, I1, K
      LOGICAL   T
      REAL      TEMP
      INCLUDE 'CPASS.INC'
      INCLUDE 'PPDAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'BPDAT.INC'
      DATA LUN2 /26/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Multisource - open old history
      CALL HIOPEN (LUN2, DISKIN, CNOIN, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Write control info.
C                                       CC tables
      IF ((DOMODL) .AND. (SMODEL(1).LE.0.0)) THEN
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
 140        CONTINUE
         END IF
C                                       General information
C                                       Soln. interval.
      IF (XSOLIN.EQ.0.0) THEN
         WRITE (HILINE,2011) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
      ELSE
         XSOLIN = SOLINT * 24.0 * 60.0
         IF (XSOLIN.GT.9999.99) XSOLIN = 9999.99
         IF (.NOT.AVGALL) WRITE (HILINE,2010) TSKNAM, XSOLIN
         IF (AVGALL) WRITE (HILINE,2013) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                        Reference ant
      WRITE (HILINE,2012) TSKNAM, REFANT
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Channel selection
      WRITE (HILINE,3030) TSKNAM, LBCHAN, LECHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Divide by channel 0
      IF (DIVCH0) THEN
         WRITE (HILINE,3040) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Channel selection
      DO 65 K = BIF,EIF
         DO 60 I = 1,20
            IF ((CHNSEL(1,I,K).GT.0) .AND.
     *         (CHNSEL(2,I,K).GE.CHNSEL(1,I,K))) THEN
               WRITE (HILINE,3050) TSKNAM, (CHNSEL(J,I,K), J = 1,3), K
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
 60         CONTINUE
 65      CONTINUE
C                                       Add external channel 0
C                                       name
      IF (EXTCH0) THEN
         WRITE (HILINE,3051) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         CALL HENCO3 (TSKNAM, NAME3, CLAS3, SEQ3, DISK3, LUN2, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 190
         END IF

C                                       Autocorrelation data
      IF (ACONLY) THEN
         WRITE (HILINE,2014) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Phase only ?
      IF (PHSONL) THEN
         WRITE (HILINE,3021) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Scalar ?
      IF (SCALAR) THEN
         WRITE (HILINE,3060) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Point source model
      IF (ABS (SMODEL(1)) .GT. 0.0) THEN
         WRITE (HILINE,2020) TSKNAM, SMODEL(1), SMODEL(2),
     *      SMODEL(3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Other parameters
         IF (SMODEL(4).GT.0.01) THEN
            WRITE (HILINE,2021) TSKNAM, SMODEL(4), SMODEL(5),
     *         SMODEL(6), SMODEL(7)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            END IF
         END IF
C                                        Already divided by model
      IF (APARM(3).GT.0.0) THEN
         WRITE (HILINE,2022) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      IF (XSPEC.NE.0.0) THEN
         WRITE (HILINE,2040) TSKNAM, XSPEC
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2041) TSKNAM, ACURVE
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                        Antenna weights.
      LIMIT = 0
 170  I1 = LIMIT + 1
      LIMIT = MIN (LIMIT + 9, NUMANT)
      IF (LIMIT.GE.I1) THEN
         IF (I1.EQ.1) THEN
            WRITE (HILINE,2050) TSKNAM, (ANTWT(I),I=I1,LIMIT)
         ELSE
            WRITE (HILINE,2051) TSKNAM, (ANTWT(I),I=I1,LIMIT)
            END IF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         GO TO 170
         END IF
      WRITE (HILINE,3090) TSKNAM, BPOVER
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
C                                       Polynomial fit parameters
      WRITE (HILINE,4000) TSKNAM, NPOLYB
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      WRITE (HILINE,4005) TSKNAM, NITERB
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      WRITE (HILINE,4010) TSKNAM, DTOLB
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      TEMP = PREAVG * 86400.0
      WRITE (HILINE,4015) TSKNAM, TEMP
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      WRITE (HILINE,4020) TSKNAM, IFITYP
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      WRITE (HILINE,4025) TSKNAM, INITVR
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IBSOLV.GT.0) THEN
         WRITE (HILINE,4030) TSKNAM, IBSOLV
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         END IF
      WRITE (HILINE,4035) TSKNAM, WSCALE
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
C                                       Close HI file
 190   CALL HICLOS (LUN2, T, BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'Release =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2001 FORMAT (A6,'INVER = ',I5,' /CC file version no.')
 2002 FORMAT (A6,'NMAPS =',I4,' /Number of clean images used')
 2003 FORMAT (A6,'NCOMP(',I3,') = ',I8,' /Number of clean comps.')
 2010 FORMAT (A6,'SOLINT = ',F7.2,' /Soln. inter. (min)')
 2011 FORMAT (A6,'SOLINT = 0.0 /Scan average')
 2012 FORMAT (A6,'REFANT = ',I4,' /Reference antenna')
 2013 FORMAT (A6,'/ Average over whole time range')
 2014 FORMAT (A6,'/ BP table generated with autocorrelations')
 2020 FORMAT (A6,'SMODEL = ',2(F10.5,','),F10.5,
     *   ' /Pt. model parameters')
 2021 FORMAT (A6,'        ',4F10.5,' / Other parms.')
 2022 FORMAT (A6,'APARM(3) = 1 /Data not divided by model in CPASS')
 2040 FORMAT (A6,'SPECINDX=',F7.3,'  / Corrected by spectral index')
 2041 FORMAT (A6,'SPECURVE=',4F8.4,'  / spectral curv')
 2050 FORMAT (A6,'ANTWT=',9F5.1,' /Ant. wt')
 2051 FORMAT (A6,'      ',9F5.1)
 3021 FORMAT (A6,'/ Phases only written to BP table')
 3030 FORMAT (A6,'BCHAN = ',I4,' ECHAN = ',I4,
     *   ' /Start- stop channels for fit')
 3040 FORMAT (A6,'/ Dividing data by channel 0')
 3050 FORMAT (A6,'/ Ch. 0 Avgd: Start, Stop, Inc ',2I5,I4,'  IF=',I3)
 3051 FORMAT (A6,'/ External channel 0 filename: ')
 3060 FORMAT (A6,'/ Data scalar averaged before determining bandpass')
 3090 FORMAT (A6,'BPVER=',I4,' / Output BP table version number')
 4000 FORMAT (A6,'CPARM(1) =',I4,' / Polynomial order')
 4005 FORMAT (A6,'CPARM(2) =',I8,' / Max. allowed iterations')
 4010 FORMAT (A6,'CPARM(3) =',D13.5,' / Convergence tolerance')
 4015 FORMAT (A6,'CPARM(4) =',F13.5,' / Pre-average interval (s)')
 4020 FORMAT (A6,'CPARM(5) =',I4,' / Fit type (1=Re/Im;2=Amp/phs)')
 4025 FORMAT (A6,'CPARM(6) =',I4,' / BP table for initial values')
 4030 FORMAT (A6,'CPARM(7) =',I2,' / Solve for phase only')
 4035 FORMAT (A6,'CPARM(8) =',L6,' / Autoscaling (T/F)')
      END
      SUBROUTINE BPSEL (IRET)
C-----------------------------------------------------------------------
C   BPSEL will read a multi source data set into a temporary scratch
C   file.  Editing and calibration are applied.
C   Inputs via common /SELCAL/  (Includes DSEL,CSEL.INC)
C      UNAME(3)     R    AIPS name of input file.
C      UCLAS(2)     R    AIPS class of input file.
C      UDISK        R    AIPS disk of input file.
C      USEQ         R    AIPS sequence of input file.
C      SOURCS(30) C*16   Names (16 char) of up to 30 sources, *=>all
C                        First character of name '-' => all except those
C                        specified.
C      CALSOU(30) C*16   Names (16 char) of up to 30 calibrators,
C                        '*' or blank =>all, first character of name '-'
C                        => all except those specified.
C      TIMRNG(8)    R    Start day, hour, min, sec, end day, hour,
C                        min,sec. 0's => all
C      UVRNG(2)     R    Minimum and maximum baseline lengths in
C                        1000's wavelengths. 0's => all
C      STOKES       C*4  Stokes types wanted.
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
C      CLVER        I    Input Cal file version number.
C      CLUSE        I    Cal file version number to put smoothed gains
C                        into and use for calibration. (May be CLVER).
C   Output:
C      IRET         I    Error code: 0 => OK,
C                        -1 => end of data
C                        >0 => failed, abort process.
C-----------------------------------------------------------------------
      INTEGER   IRET, LUN1, LUN2, TVER1, TVER2
      REAL     DUM
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'CPASS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN1, LUN2 /46, 47/
C-----------------------------------------------------------------------
C                                       Setup
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NVIS.LE.0) GO TO 100
      IF (EXTCH0) THEN
         CALL INITEX ('INIT', IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,2005) IRET
            GO TO 990
            END IF
         END IF
C                                       Message
      IF (DOCAL.AND.DOFLAG) THEN
         MSGTXT = 'Selecting, editing and calibrating the data'
      ELSE IF (DOFLAG) THEN
         MSGTXT = 'Selecting and editing the data'
      ELSE IF (DOCAL) THEN
         MSGTXT = 'Selecting and calibrating the data'
      ELSE
         MSGTXT = 'Selecting the data'
         END IF
      CALL MSGWRT (2)
      IF (DIVCH0) THEN
         IF (EXTCH0) THEN
            MSGTXT = 'Dividing the spectral data by external channel 0'
         ELSE
            MSGTXT = 'Dividing the spectral data by channel 0'
            END IF
         CALL MSGWRT (2)
         END IF
C                                       Copy
      VISDSK = 0
      VISCNO = 0
      CALL BPCOPY (VISDSK, VISCNO, BUFF1, JBUFSZ, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy relevant portion of IF
C                                       table.
      TVER1 = 1
      TVER2 = 1
      CALL CHNCOP (TVER1, TVER2, LUN1, LUN2, DISKIN, SCRVOL(VISCNO),
     *   CNOIN, SCRCNO(VISCNO), CATUV, CATBLK, BIF, EIF, FRQSEL,
     *   SFREQS, BUFF1, FOFF, UBUFF, FINC, IRET)
      GO TO 999
C                                       No data
 100  CALL UVGET ('CLOS', DUM, DUM, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 2005 FORMAT ('BPSEL: ERROR ',I3,' OPENING EXTERNAL CHANNEL 0')
      END
      SUBROUTINE BPCOPY (DISK, CNOSCR, BUFFER, BUFSZ, IRET)
C-----------------------------------------------------------------------
C   Routine to copy selected data from one data file to another
C   optionally applying calibration and editing information.  The input
C   file should have been opened with UVGET.  Both files will be closed
C   on return from BPCOPY.
C     Note: UVGET returns the information necessary to catalog the
C   output file.  The output file will be compressed if necessary at
C   completion of BPCOPY.
C    Input:
C      DISK         I    Disk number for catalogd output file.
C                        If .LE. 0 then the output file is a /CFILES/
C                        scratch file.
C      CNOSCR       I    Catalog slot number for if catalogd file;
C                        /CFILES/ scratch file number if a scratch file,
C                        IF DISK=CNOSCR=0 then the scratch is created.
C      BUFFER(*)    R    Work buffer for writing.
C      BUFSZ        I    Size of BUFFER in bytes.
C    Input via common:
C      CATBLK(256)  I    Catalog header block from UVGET
C      NVIS         I    (/UVHDR/) Number of vis. records.
C      LREC         I    (/UVHDR/) length of vis. record in R   words.
C      NRPARM       I    (/UVHDR/) number of (R)   random parameters.
C    Output:
C      CNOSCR       I    Scratch file number if created.
C      IRET         I    Error code: 0 => OK,
C                                   >0 => failed, abort process.
C    Output via common:
C      CATBLK(256)  I    Catalog header block with actual no. records.
C      NVIS         I    (/UVHDR/) Actual number of vis. records.
C   Usage notes:
C    1) UVGET with OPCODE='INIT' MUST be called before BPCOPY to setup
C       for calibration, editing and data translation.  If an output
C       catalogd file is to be created this should be done after the
C       call to UVGET.
C    2) Uses AIPS LUN 24
C-----------------------------------------------------------------------
      CHARACTER NAME*48
      INTEGER   DISK, CNOSCR, IRET, VOL, LUN, FIND, BIND, LENBU,
     *   NIO, BUFSZ, CNO, IA1, IA2, BO, VO, I,  XCOUNT,
     *   YCOUNT, ISIZE
      LOGICAL   T, F
      REAL      BUFFER(*)
      INCLUDE 'CPASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, BO, VO /24,1,0/
C-----------------------------------------------------------------------
      IRET = 0
      LENBU = 1
C                                       Create output file if necessary
      IF ((DISK.GT.0) .OR. (CNOSCR.GT.0)) GO TO 30
C                                       Determine size.
         CALL UVSIZE (LREC, NVIS, ISIZE)
C                                       Create scratch file.
         CALL SCREAT (ISIZE, BUFFER, IRET)
         CNOSCR = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) THEN
               MSGTXT = 'BPCOPY: TOO LITTLE DISK SPACE FOR SCRATCH FILE'
            ELSE
               WRITE (MSGTXT,1011) IRET
               END IF
            GO TO 990
            END IF
C                                       Update CATBLK.
      CALL CATIO ('UPDT', SCRVOL(CNOSCR), SCRCNO(CNOSCR), CATBLK,
     *   'REST', BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         END IF
C                                       Set output file name.
 30   VOL = DISK
      IF (DISK.LE.0) VOL = SCRVOL(CNOSCR)
      IF (DISK.GT.0) CALL ZPHFIL ('UV', VOL, CNOSCR, 1, NAME, IRET)
      IF (DISK.LE.0) CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCR), 1, NAME,
     *   IRET)
      CNO = CNOSCR
      IF (DISK.LE.0) CNO = SCRCNO(CNOSCR)
C                                       Open output file.
      CALL ZOPEN (LUN, FIND, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      CALL UVINIT ('WRIT', LUN, FIND, NVIS, VO, LREC,LENBU, BUFSZ,
     *    BUFFER, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
C                                       Copy file
      YCOUNT = 0
      DO 100 I = 1,NVIS
         XCOUNT = I
C                                       Read old.
         CALL UVGET ('READ', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
         IF (IRET.LT.0) GO TO 110
         IF (IRET.NE.0) GO TO 999
         IF (DIVCH0) THEN
            IF (.NOT.EXTCH0) CALL DIVCHZ (BUFFER(BIND+NRPARM))
            IF (EXTCH0) CALL EXTDIV (BUFFER(BIND),
     *         BUFFER(BIND+NRPARM), IRET)
            END IF
C                                       Write new
         IF (ACONLY) THEN
            IF (ILOCB.GE.0) THEN
               IA1 = BUFFER(BIND+ILOCB)/256 + 0.1
               IA2 = BUFFER(BIND+ILOCB) - IA1*256 + 0.1
            ELSE
               IA1 = BUFFER(BIND+ILOCA1) + 0.1
               IA2 = BUFFER(BIND+ILOCA2) + 0.1
               END IF
            IF (IA1.NE.IA2) GO TO 100
            YCOUNT = YCOUNT + 1
            END IF
C                                       Write data
         NIO = 1
         CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET
            GO TO 990
            END IF
 100        CONTINUE
C                                       Check if last call to UVGET
C                                       returned valid data.
 110     IF (IRET.LT.0) XCOUNT = XCOUNT - 1
C                                       Flush output
         NIO = 0
         CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET
            GO TO 990
            END IF
C                                       Close input
      CALL UVGET ('CLOS', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
      IF (EXTCH0) CALL INITEX ('CLOS', IRET)
C                                       Compress output file.
      NVIS = XCOUNT
      IF (ACONLY) NVIS = YCOUNT
      CALL UCMPRS (NVIS, VOL, CNO, LUN, CATBLK, IRET)
C                                      Put vis. count in CATBLK
      CATBLK(KIGCN) = NVIS
C                                       Update CATBLK.
      CALL CATIO ('UPDT', VOL, CNO, CATBLK, 'REST', BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         END IF
C                                       Close output
      CALL ZCLOSE (LUN, FIND, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1011 FORMAT ('BPCOPY: ERROR ',I5,' CREATING SCRATCH FILE')
 1020 FORMAT ('BPCOPY: ERROR ',I5,' UPDATING SCRATCH FILE CATBLK')
 1030 FORMAT ('BPCOPY: ERROR',I5,' OPENING OUTPUT FILE')
 1040 FORMAT ('BPCOPY: ERROR',I5,' INIT. OUTPUT FILE')
 1060 FORMAT ('BPCOPY: ERROR',I5,' WRITING OUTPUT FILE')
      END
      SUBROUTINE DIVCHZ (VIS)
C-----------------------------------------------------------------------
C   DIVCHZ forms the so-called channel 0 (centre 75% of band) from
C   the visibility data and then the spectral data is divided by the
C   channel 0 data.
C
C   Input/Output
C      VIS(3,*)      R      On input the visibility spectrum, on
C                           output the corrected (ie divided spectrum)
C   Input from common:
C    INCF   I     Increment in freq. of data from UVGET
C    INCIF  I     Increment in IF of data from UVGET
C    INCS   I     Increment in Stokes' of data from UVGET
C-----------------------------------------------------------------------
      REAL      VIS(*)
C
      INTEGER   I, LOOPS, LOOPIF, INDEX, INP, JNCS, JNCIF, JNDEX
      REAL      TEMP, DENOM
      LOGICAL   FLAGD
      INCLUDE 'INCS:DSEL.INC'
      REAL      CHZ(MAXIF*12)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'CPASS.INC'
C-----------------------------------------------------------------------
C                                       Set output increments
C                                       (averaging)
      JNCIF = INCIF
      IF (JLOCF.LT.JLOCIF) JNCIF = INCIF / NUMFRQ
      JNCS = INCS
      IF (JLOCF.LT.JLOCS) JNCS = INCS / NUMFRQ
C                                       Calculate channel 0 visibility
      CALL AVGCHN (VIS, NCOR, 1, NUMFRQ, BIF, EIF, CHNSEL, JNCS, JNCIF,
     *   CHZ)
C                                       Do the division
      DO 300 LOOPS = 1,NCOR
         DO 200 LOOPIF = 1,BNUMIF
            INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-1) * INCIF
            JNDEX = 1 + (LOOPS-1) * JNCS + (LOOPIF-1) * JNCIF
            DENOM = CHZ(JNDEX) * CHZ(JNDEX) +
     *              CHZ(JNDEX+1) * CHZ(JNDEX+1)
            FLAGD = .FALSE.
            IF (DENOM.LE.0) THEN
               DENOM = 1.0
               FLAGD = .TRUE.
               END IF
            DO 100 I = 1,NUMFRQ
               INP = INDEX + (I-1) * INCF
               TEMP = VIS(INP)
               VIS(INP)   = (CHZ(JNDEX)*TEMP +
     *                       CHZ(JNDEX+1)*VIS(INP+1)) / DENOM
               VIS(INP+1) = (CHZ(JNDEX)*VIS(INP+1) -
     *                       CHZ(JNDEX+1)*TEMP) / DENOM
               IF (FLAGD) VIS(INP+2) = -1
 100           CONTINUE
 200        CONTINUE
 300     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BPMOD (IRET)
C-----------------------------------------------------------------------
C   BPMOD divides the CLEAN model visibilities into the data.
C   If no model is found or a point model is specified then the data
C   is divided by the flux density found in the Source (SU) table.
C   Inputs: from commons
C     NCOMP      R    Number of components to be divided.
C     DISKIN    R    Input file disk number.
C     CNOIN     I    Input file catalog number.
C     DISK2     R    CLEAN file disk number.
C     XNMAP     R    Number of model files.
C     CCTVER    I    CC table version number.
C     SMODEL(7) R    If .lt. 0 use no model, if .gt. 0 use point model
C   Output:
C     CNOIN2    I    CLEAN file catalog number.
C     IRET      I    Return code, 0 => ok, otherwise not.
C-----------------------------------------------------------------------
      CHARACTER PBLANK*12
      INTEGER   IRET, MODEL, METHOD, ISTOKE, DISKO, ISCR, CHAN, IIF,
     *   NCHAN, I, IROUND
      LOGICAL   DOMSG, F, NONAM, NOCLAS, WASOME
      INCLUDE 'CPASS.INC'
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
C                                       Check if multiple sources
      IF (SINGLE) NSOUWD = 1
      IF (NSOUWD.NE.1) GO TO 300
C                                       If neither a point (SMODEL) nor
C                                       clean model use source table.
      PBLANK = ' '
      NONAM = PBLANK .EQ. NAME2
      NOCLAS = PBLANK .EQ. CLAS2
      IF (NONAM .AND. NOCLAS .AND. (SMODEL(1).LE.1.0E-20)) GO TO 300
C                                       Set model and method
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
      IF (CMOD.EQ.'SUBI') MODEL = 3
      METHOD = 0
      IF (CMETH.EQ.'DFT ') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
C                                       Point source parameters
      DOPTMD = ABS (SMODEL(1)) .GT. 1.0E-20
      PTFLX = SMODEL(1)
      PTRAOF = SMODEL(2)
      PTDCOF = SMODEL(3)
C                                       Get info on model file(s)
      LIMFLX = XFLUX
      MFIELD = IROUND (XNMAP)
      IF (MFIELD.LE.0) MFIELD = 1
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
      FACGRD(1) = 1.0
      FACGRD(2) = 1.0
      IF (DOPTMD) THEN
         DO3DIM = .FALSE.
      ELSE
         CALL SETGDS (DISKIN, CNOIN, NAME2, CLAS2, SEQ2, DISK2, MFIELD,
     *      CCTVER, NCOMP, BITER, MODEL, METHOD, BUFF1, BUFF2, ISTOKE,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IF (MODEL.GT.0) THEN
            IF (MODEL.EQ.3) THEN
               MSGTXT = 'Using sub-images for the source model'
            ELSE IF (MODEL.EQ.2) THEN
               MSGTXT = 'Using images for the source model'
            ELSE
               MSGTXT = 'Using Clean Component source model'
               END IF
            CALL MSGWRT (3)
            CALL FACSET (DISKIN, CNOIN, 1, SOUWAN(1), MODEL, 1.0, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
      XVER = VER
      CNOIN2 = CCCNO(1)
C                                       Divide data by model
      DISKO = VISDSK
      ISCR = VISCNO
      COMPDT = .FALSE.
      DATDIV = .TRUE.
C                                       Consider whether to process
C                                       1 IF at a time
      IF ((BNUMIF.GT.1) .AND. (MODEL.EQ.1) .AND.
     *   (ABS(FACGRD(1)-1.0).GT.1.E-10)) THEN
C                                       number of channels
         NCHAN = CATBLK(KINAX+JLOCF)
C                                       For each IF
         DO 15 IIF = 1,BNUMIF
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
         NCHAN = CATBLK(KINAX+JLOCF) * BNUMIF
C                                       Div all vis by model
         CALL UVMDIV (VISDSK, VISCNO, DISKO, ISCR, MODEL, METHOD, DOMSG,
     *      CHAN, NCHAN, CATBLK, JBUFSZ, FRQSEL, BUFF1, BUFF2, UBUFF,
     *      RBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (.NOT.DOPTMD) CALL UNSETG (BUFF2)
         END IF
C                                       Get true  values of NCOMP
      DO 20 I = 1,MFIELD
         NCOMP(I) = NSUBG(I) - 1
 20      CONTINUE
C                                       Model divided by data now
C                                       in scratch file.
      VISDSK = 0
      VISCNO = ISCR
      GO TO 999
C                                       Multiple sources, use point
C                                       source at phase center only.
 300  CALL BPDIV (IRET)
C
 999  RETURN
      END
      SUBROUTINE BPDIV (IRET)
C-----------------------------------------------------------------------
C   BPDIV divides multisource data in a scratch file  by the
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
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  VELTYP(2)*8, VELDEF(2)*8, SOUNAM*16, CALCOD*4,
     *   IFILE*48
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNO, INDI, INDO, LENVIS,
     *   ILENBU, KBIND, IBIND, I, J, IVIS, IOFF, NOVIS, SUKOLS(MAXSUC),
     *   SUNUMV(MAXSUC), IDSOU, QUAL, SULUN, IFNO, INDX, NVPIF,
     *   BO, VO, ISURNO, NUMSOU, LOOP, SUFQID, SUNIF
      LOGICAL   T, F, DOIT
      REAL   SFLUX(8192), XSFLUX
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'CPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      DOUBLE PRECISION   LSRVEL(MAXIF), RESTFQ(MAXIF), FREQO(MAXIF)
      REAL     FLUX(4,MAXIF)
      EQUIVALENCE (SFLUX, UBUFF)
      DATA SULUN, LUNO /27, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (SOUWAN(1).LE.0) SOUWAN(1) = 1
      LUNI = 16
C                                       Message
      MSGTXT = 'Dividing data by source flux densities'
      CALL MSGWRT (2)
      LENVIS = CATBLK(KINAX)
C                                       Open source (SU) table
      CALL SOUINI ('READ', NXBUFF, DISKIN, CNOIN, 1, CATUV,
     *   SULUN, SUNIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
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
         INDX = (IDSOU-1) * SUNIF
         DO 25 I = 1,NUMIF
            SFLUX(INDX+I) = FLUX(1,I)
            IF (SFLUX(INDX+I).LE.1.0E-10) SFLUX(INDX+I) = 1.0
            DOIT = DOIT .OR. (ABS (SFLUX(INDX+I)-1.0) .GT. 1.0E-10)
            SFLUX(INDX+I) = 1.0 / SFLUX(INDX+I)
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
      NVPIF = NOVIS / SUNIF
      IF (NVPIF.LT.1) NVPIF = 1
C                                       Open and init for write
C                                       visibility file
      CALL ZPHFIL ('SC', SCRVOL(VISCNO), SCRCNO(VISCNO), 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, SCRVOL(VISCNO), IFILE, T, F, F, IRET)
      IF (IRET.GT.1) THEN
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
      IF (IRET.GT.1) THEN
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
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
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
C                                       This assumes that IF is the
C                                       most slowly varying axis.
            DO 140 IVIS = 1,NOVIS
               IFNO = ((IVIS-1) / NVPIF) + 1
               INDX = (IDSOU-1) * SUNIF + IFNO
               XSFLUX = SFLUX(INDX)
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
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1210) IRET, 'WRIT'
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPDIV: ERROR',I3,' OPENING SOURCE TABLE')
 1010 FORMAT ('BPDIV: ERROR',I3,' READING SOURCE TABLE')
 1030 FORMAT ('BPDIV: ERROR',I3,' CLOSING SOURCE TABLE')
 1040 FORMAT ('BPDIV: ERROR',I3,' OPEN-FOR-',A4,' VIS FILE')
 1050 FORMAT ('BPDIV: ERROR',I3,' INIT-FOR-',A4,' VIS FILE')
 1100 FORMAT ('BPDIV: ERROR',I3,1X,A4,'ING VIS FILE')
 1210 FORMAT ('BPDIV: ERROR',I3,'CLOSING ',A4,' VIS FILE')
      END
      SUBROUTINE BPSOL (MAXFRQ, MAXTEL, IRET)
C-----------------------------------------------------------------------
C   BPSOL is the routine which controls BASOLV (which does all
C   the work).
C   Inputs:
C     MAXFRQ          I   Max possible # of frequency channels
C     MAXTEL          I   Max possible # of antennas
C   Output:
C     IRET            I   Error code. 0 => All OK
C-----------------------------------------------------------------------
      INTEGER   MAXFRQ, MAXTEL, IRET
C
      CHARACTER IFILE*48
      INTEGER  DISK, LUNP, CVER, SCHIF
      INCLUDE 'CPASS.INC'
      INCLUDE 'PPDAT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'BPDAT.INC'
      CHARACTER LSTOK*2, BNDCOD(MAXIF)*8
      REAL      TIMEC
      DOUBLE PRECISION    REFF, CATD(128)
      INTEGER   IPOL, IIF, IVER, IPOL1, IPOL2, SCOR
      LOGICAL   T, F, FIRST
      EQUIVALENCE (CATUV, CATD)
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      LUNI = 16
      LUNP = 49
C                                       check spectral index stuff
      CALL BPSPEC
C                                       Get AN table information
      IVER = MAX (SUBARR, 1)
      CALL GETANT (DISKIN, CNOIN, IVER, CATUV, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, IVER
         GO TO 990
         END IF
C                                       Message
      MSGTXT = 'Determining solutions'
      CALL MSGWRT (6)
      IF (SCALAR) THEN
         MSGTXT = 'Using amp-scalar averaging'
         CALL MSGWRT (6)
         END IF
C                                        Get IF freq. offsets
      CVER = 1
      CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, CVER, CATUV, LUNP,
     *   SCHIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) IRET
         GO TO 990
         END IF
      REFF = CATD(KDCRV+IKLOCF)
C                                        Open vis. file
      DISK = VISDSK
      IF (VISDSK.EQ.0) DISK = SCRVOL(VISCNO)
      IF (VISDSK.EQ.0)
     *   CALL ZPHFIL ('SC', DISK, SCRCNO(VISCNO), 1, IFILE, IRET)
      IF (VISDSK.GT.0)
     *   CALL ZPHFIL ('UV', DISK, VISCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, FINDI, DISK, IFILE, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         GO TO 990
         END IF
C                                       Find BP solutions for each
C                                       IF and polzn. separately
      FIRST = T
C                                       Set Stokes axis range
      IF ((ICOR0.EQ.-1) .OR. (ICOR0.EQ.-5)) THEN
         IPOL1 = ICOR0 - MIN (2, NCOR) + 1
      ELSE
         IPOL1 = ICOR0
         END IF
      IPOL2 = ICOR0
C
      DO 500 SCOR = IPOL1,IPOL2
         DO 400 IIF = BIF,EIF
C                                       Print message
            LSTOK = '??'
            IF (SCOR.EQ.-1) LSTOK = 'RR'
            IF (SCOR.EQ.-2) LSTOK = 'LL'
            IF (SCOR.EQ.-5) LSTOK = 'XX'
            IF (SCOR.EQ.-6) LSTOK = 'YY'
            IF (SCOR.EQ.1) LSTOK = 'I'
            WRITE (MSGTXT,1400) LSTOK, IIF
            CALL MSGWRT (6)
C                                       Find solution
            IPOL = 1
            IF (SCOR.LT.IPOL2) IPOL = 2
            CALL BASOLV (MAXTEL, MAXFRQ, SCOR, IPOL, IIF, FIRST, IRET)
            FIRST = F
 400        CONTINUE
 500     CONTINUE
C                                       Close uv-file
      CALL ZCLOSE (LUNI, FINDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                       Close index file
      IF (DONDX) CALL TABIO ('CLOS', 0, INXRNO, TIMEC, NXBUFF, IRET)
      IRET = 0
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPSOL: ERROR ',I3,' READING AN TABLE: ',I3)
 1005 FORMAT ('BPSOL: ERROR ',I3,' OPENING VISIBILITY FILE')
 1030 FORMAT ('BPSOL: ERROR ',I3,' CLOSING UV FILE')
 1090 FORMAT ('BPSOL: ERROR ',I3,' READING FQ/CH TABLE')
 1400 FORMAT ('Solving polarization ',A2,', IF channel ',I4)
      END
      SUBROUTINE BASOLV (MAXTEL, MAXFRQ, SCOR, IPOL, SIF, FIRST, IERR)
C-----------------------------------------------------------------------
C   BASOLV reads through the calibrated data and performs a
C   least-squares fit to the antenn-based polynomial bandpasses,
C   and writes the coefficient solutions to the BP table.
C   Input:
C     MAXTEL       I    Max. # antennas.
C     MAXFRQ       I    Max. # frequency channels.
C     SCOR         I    Stokes axis to solve (STOKES VALUE)
C     IPOL         I    Position in BP table (1, unless L in 2-pol
C                       table)
C     SIF          I    IF channel number to solve.
C     FIRST        L    True if first call to BASOLV.
C   Output:
C     IERR         I    Return code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'PPDAT.INC'
      LOGICAL FIRST
      INTEGER MAXTEL, MAXFRQ, SCOR, IPOL, SIF, IERR
C
      INTEGER   MXWORK
      PARAMETER (MXWORK = 14 * MXPARM)
      EXTERNAL  DFIT, DFIT2, DFIT3
      REAL      AWORK(NSIZBP), SWORK(NSIZBP), NCNT(MXBASE)
      INTEGER   BINDI, NIN, IITEMP, IRET, IBL, INDEX,
     *   JBL, I1, I2, IROUND, SCNSOU, SCNSUB, SUBA, INTNO,
     *   NUMINT, IFRQ, BO, VO, IDUM1, IDUM2, FREQID, NBLMAX, K, JNDX,
     *   ISOU, I, J, KR, KI, INDX, NPOL, IBCHAN,
     *   IBPSOU, IREFBP
      REAL      DELT, CATR(256), TIME(512), WT, ENDTIM, CURTIM, LASTIM,
     *   CATIN4(128), WTT(512), SIUSE, SCNTIM, ENDAVG, SCNDT, SCNEND,
     *   BLFACT, STTIME, MINTIM, MAXTIM, XXAMP, YYAMP, WTMAX
      LOGICAL   T, F, JUSRED, WEOF
      DOUBLE PRECISION  DTIMEC, DRAEP, DECEP, DFREQ, DFR, DX(MXPARM),
     *   DSHIFT(MAXANT), DCHAN, DG(MXPARM), DF, DWORK(MXWORK), DTOL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'CPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'BPDAT.INC'
      EQUIVALENCE (CATUV, CATIN4),        (CATBLK, CATR)
      EQUIVALENCE (TIME, BUFF2(1)),    (WTT, BUFF2(513))
      DATA T,F /.TRUE.,.FALSE./
      DATA BO /1/, VO /0/
C-----------------------------------------------------------------------
C                                        Set up some local variables
      MINTIM = 1.0E10
      MAXTIM = -1.0E10
C                                        First initialize.
      NIN = 1
      VO = 0
      CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC,NIN, JBUFSZ,
     *   BUFF1, BO, BINDI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Initialize I/O to INDEX file
C                                       first time through
      IF (AVGALL) THEN
         DONDX = .FALSE.
      ELSE
         IF (FIRST) THEN
            MSGSUP = 32000
            CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, 1, CATUV, IXLUN,
     *         INXRNO, NXKOLS, NXNUMV, IRET)
            MSGSUP = 0
            DONDX = IRET.EQ.0
            END IF
         INXRNO = 1
         END IF
C                                       Dummy if no scans
      SCNTIM = -1.0E10
      SCNEND =  1.0E10
      SCNSOU = 0
      SCNSUB = 0
      SCNDT = 0.0
C                                       Read first scan info
      IF (DONDX) THEN
         CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, SCNTIM,
     *      SCNDT, SCNSOU, SCNSUB, IDUM1, IDUM2, FREQID, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Add 2 sec to each end
      SCNDT = SCNDT + 1.0E-4
      IF (DONDX) THEN
         SCNTIM = SCNTIM - 0.5 * SCNDT
         SCNEND = SCNTIM + SCNDT
         END IF
      SCNDT = MAX (SCNDT, SOLINT)
C
C   *******        This is where the routine loops back to after
C   *******        finishing a solution interval
C
C                                       Begin Loop in time.
 10   CONTINUE
C
C                                       Initialize data averaging
C                                       buffers
      CALL RFILL (NSIZBP, 0.0, XREDAT)
      CALL RFILL (NSIZBP, 0.0, XIMDAT)
      CALL RFILL (NSIZBP, 0.0, XWGTBP)
      CALL FILL (MXSPEC, 0, IBASL)
      CALL FILL (MXSPEC, 0, JBASL)
      CALL RFILL (MXSPEC, 0.0, SHIFTA)
      CALL RFILL (MXSPEC, 0.0, SHIFTB)
      NBPANT = MAXTEL
      NBPCHN = MAXFRQ
      NTERMS = NPOLYB
      NBASL = 1
      NBLMAX = 0
C                                       Initialize shift array
      CALL DFILL (MAXANT, -1.0D10, DSHIFT)
C                                    Read first record of
C                                    solution interval
      CALL UVDISK ('READ', LUNI, FINDI, BUFF1, NIN, BINDI, IERR)
      WEOF = (NIN.LE.0)
      IF (WEOF) GO TO 999
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR, 'READ'
         GO TO 990
         END IF
      CURTIM = BUFF1(BINDI+ILOCT)
      MAXTIM = MAX (CURTIM, MAXTIM)
      MINTIM = MIN (CURTIM, MINTIM)
      IF (ILOCB.GE.0) THEN
         IITEMP = BUFF1(BINDI+ILOCB) + 0.1
         SUBA = (BUFF1(BINDI+ILOCB) - IITEMP) * 100.0 + 1.5
      ELSE
         SUBA = BUFF1(BINDI+ILOCSA) + 0.1
         END IF
C
      IF (DONDX) THEN
         IF ((CURTIM.GT.SCNEND) .OR. ((SUBA.NE.SCNSUB) .AND.
     *      (SCNSUB.NE.0)) .OR.
     *      ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0))) THEN
C                                       Find next index record
 70         IF (INXRNO.LE.NXBUFF(5)) THEN
               CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *            SCNTIM, SCNDT, SCNSOU, SCNSUB, IDUM1, IDUM2,
     *            FREQID, IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Add 2 sec to each end
               SCNDT = SCNDT + 1.0E-4
               SCNTIM = SCNTIM - 0.5 * SCNDT
               SCNEND = SCNTIM + SCNDT
               SCNDT = MAX (SCNDT, SOLINT)
               IF ((CURTIM.GT.SCNEND) .OR. ((SUBA.NE.SCNSUB) .AND.
     *            (SCNSUB.NE.0)) .OR.
     *            ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0))) GO TO 70
            ELSE
               SCNEND = CURTIM + 0.5E-4
               END IF
            END IF
         END IF
C                                      Adjust by 1 sec.
      LASTIM = CURTIM + SOLINT - 1.157407E-5
      STTIME = CURTIM
C                                       if INDEXed divide up scan
C                                       into even sections
      IF (DONDX) THEN
         NUMINT = IROUND (SCNDT / SOLINT)
         NUMINT = MAX (NUMINT, 1)
         SIUSE = SCNDT / NUMINT
         INTNO = (CURTIM-SCNTIM) / SIUSE
         IF (INTNO.LT.0) INTNO = 0
         INTNO = INTNO + 1
         LASTIM = SCNTIM + INTNO * SIUSE
         IF (LASTIM.GT.SCNEND) LASTIM = SCNEND
         END IF
C                                       Set end of pre-average
C                                       interval
 90   ENDAVG = MIN (CURTIM + PREAVG, LASTIM)
      CALL RFILL (NSIZBP, 0.0, AWORK)
      CALL RFILL (NSIZBP, 0.0, SWORK)
      CALL FILL (MXBASE, 0, NCNT)
C                                       Skip first read
      JUSRED = T
C                                       Load data into array.
C                                       Begin Loop.
C                                       If next point already read,
C                                       skip read.
 100  IF (.NOT.JUSRED) THEN
         CALL UVDISK ('READ', LUNI, FINDI, BUFF1, NIN, BINDI, IERR)
         WEOF = (NIN.LE.0)
         IF (WEOF) GO TO 160
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'READ'
            GO TO 990
            END IF
         END IF
C
      JUSRED = F
      ISOU = -1
      IF (ILOCSU.GE.0) ISOU = BUFF1(BINDI+ILOCSU)
      CURTIM = BUFF1(BINDI+ILOCT)
      MAXTIM = MAX (CURTIM, MAXTIM)
      MINTIM = MIN (CURTIM, MINTIM)
C                                       Check for end of pre-average
C                                       or solution interval
      IF (CURTIM.GT.ENDAVG) GO TO 160
C                                        Determine antenna numbers
      IF (ILOCB.GE.0) THEN
         JBL = BUFF1(BINDI+ILOCB) + 0.1
         I1 = JBL / 256
         I2 = JBL - I1 * 256
      ELSE
         I1 = BUFF1(BINDI+ILOCA1) + 0.1
         I2 = BUFF1(BINDI+ILOCA2) + 0.1
         END IF
C                                        No message on AC data
      IF (I2.LT.I1) THEN
          WRITE (MSGTXT,1130) I1, I2, MAXTEL
          CALL MSGWRT (6)
          GO TO 100
          END IF
C                                       Process data
C                                       Baseline factors
      BLFACT = ANTWT(I1) * ANTWT(I2)
C                                       Pre-average the data
C                                       Find entry number
      IBL = NBASL
 140  IF (IBL.LE.NBLMAX) THEN
         IF ((IBASL(IBL).EQ.I1) .AND. (JBASL(IBL).EQ.I2)) GO TO 145
         IBL = IBL + 1
         GO TO 140
         END IF
C                                       New entry
      NBLMAX = NBLMAX + 1
      IF (NBLMAX.GT.MXSPEC) THEN
         MSGTXT = 'BASOLV: Parameter MXSPEC too small; contact AIPS'
     *      // ' admin'
         IERR = 2
         GO TO 990
         END IF
      IBASL(IBL) = I1
      JBASL(IBL) = I2
C                                       Accumulate lobe-rotator shifts
C                                       for VLBA data
 145  IF (VLBA) THEN
         K = IBL - NBASL + 1
         IF (SINGLE.OR. (ISOU.LT.0)) THEN
            DRAEP = RA * DG2RAD
            DECEP = DEC * DG2RAD
         ELSE
            IF (IDSOUR.NE.ISOU) THEN
               CALL GETSOU (ISOU, DISKIN, CNOIN, CATUV, IPLUN, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1150) IERR, ISOU
                  GO TO 990
                  END IF
               DRAEP = RAEPO
               DECEP = DECEPO
               END IF
            END IF
C                                       Compute shift
         DFREQ = FREQ + FOFF(SIF)
         CALL DETRAT (CURTIM, DRAEP, DECEP, STNX(I1), STNY(I1),
     *      STNZ(I1), DFREQ, DFR)
         DCHAN = DFR / FINC(SIF)
         DSHIFT(I1) = MAX (DSHIFT(I1), ABS (DCHAN))
         SHIFTA(IBL) = SHIFTA(IBL) + DCHAN
         CALL DETRAT (CURTIM, DRAEP, DECEP, STNX(I2), STNY(I2),
     *      STNZ(I2), DFREQ, DFR)
         DCHAN = DFR / FINC(SIF)
         DSHIFT(I2) = MAX (DSHIFT(I2), ABS (DCHAN))
         SHIFTB(IBL) = SHIFTB(IBL) + DCHAN
         NCNT(K) = NCNT(K) + 1
         END IF
C                                       Accumulate data
      DO 150 IFRQ = 1,MAXFRQ
         INDEX = BINDI + NRPARM + (IFRQ - 1) * INCF +
     *      (SIF - BIF) * INCIF + (ICOR0 - SCOR) * INCS
         WT = BLFACT * BUFF1(INDEX+2)
         IF (BUFF1(INDEX+2).LE.0.0) WT = 0.0
         JNDX = (IBL - 1) * MAXFRQ + IFRQ
         XREDAT(JNDX) = XREDAT(JNDX) + BUFF1(INDEX) * WT
         XIMDAT(JNDX) = XIMDAT(JNDX) + BUFF1(INDEX+1) * WT
         XWGTBP(JNDX) = XWGTBP(JNDX) + WT
         K = (IBL - NBASL) * MAXFRQ + IFRQ
C         SWORK(K) = SWORK(K) + WT * (BUFF1(INDEX)**2 +
C     *      BUFF1(INDEX+1)**2)
         SWORK(K) = SWORK(K) + WT * (BUFF1(INDEX)**2 +
     *      BUFF1(INDEX+1)**2)
         IF (SCALAR) AWORK(K) = AWORK(K) + WT * SQRT (BUFF1(INDEX) ** 2
     *      + BUFF1(INDEX+1) ** 2)
150      CONTINUE
C                                       Loop back for next record
      GO TO 100
C                                       End of pre-averaging:
C                                       Normalize
160   DO 170 IBL = NBASL,NBLMAX
C                                       Average the lobe-rotator
C                                       shifts
         K = IBL - NBASL + 1
         IF (NCNT(K).GT.0) THEN
            SHIFTA(IBL) = SHIFTA(IBL) / NCNT(K)
            SHIFTB(IBL) = SHIFTB(IBL) / NCNT(K)
            END IF
C                                       Average the data
         DO 165 IFRQ = 1,MAXFRQ
            JNDX = (IBL - 1) * MAXFRQ + IFRQ
            WT = XWGTBP(JNDX)
            IF (WT.GT.0.0) THEN
               XREDAT(JNDX) = XREDAT(JNDX) / WT
               XIMDAT(JNDX) = XIMDAT(JNDX) / WT
               END IF
 165        CONTINUE
 170     CONTINUE
C                                       Adjust weights & drop edges
      WTMAX = 0.0
      DO 180 IBL = NBASL,NBLMAX
         DO 175 IFRQ = 1,MAXFRQ
            JNDX = (IBL - 1) * MAXFRQ + IFRQ
            WT = XWGTBP(JNDX)
            IF ((IFRQ.LT.LBCHAN) .OR. (IFRQ.GT.LECHAN)) WT = 0.0
            IF (WT.GT.0.0) THEN
               K = (IBL - NBASL) * MAXFRQ + IFRQ
               IF (CPARM(9).LE.0.0) THEN
                  XXAMP = SWORK(K) / WT - XREDAT(JNDX)**2 -
     *               XIMDAT(JNDX)**2
                  IF (XXAMP.GT.0.0) WT = 0.9 * MIN (10.*WT, 1.0/XXAMP) +
     *               WT / 10.0
               ELSE IF (CPARM(9).GT.1.5) THEN
                  WT = 1.0
                  END IF
               WTMAX = MAX (WTMAX, WT)
C                                       use scalar average
               IF (SCALAR) THEN
                  XXAMP = SQRT (XREDAT(JNDX) ** 2 +
     *               XIMDAT(JNDX) ** 2) * XWGTBP(JNDX)
                  YYAMP = 1.0
                  IF (XXAMP.GT.0.0) YYAMP = AWORK(K) / XXAMP
                  XREDAT(JNDX) = XREDAT(JNDX) * YYAMP
                  XIMDAT(JNDX) = XIMDAT(JNDX) * YYAMP
                  END IF
               END IF
            XWGTBP(JNDX) = WT
 175        CONTINUE
 180     CONTINUE
C                                       Auto-scale XP data ?
      DO 185 IBL = NBASL, NBLMAX
         JNDX = (IBL - 1) * MAXFRQ + 1
         IF (WSCALE) CALL XPSCAL (XREDAT(JNDX), XIMDAT(JNDX), MAXFRQ)
 185     CONTINUE
C                                       scale weights
      IF ((WTMAX.GT.0.0) .AND. (WTMAX.NE.1.0)) THEN
         DO 195 IBL = NBASL,NBLMAX
            DO 190 IFRQ = 1,MAXFRQ
               JNDX = (IBL - 1) * MAXFRQ + IFRQ
               IF (XWGTBP(JNDX).GT.0.) XWGTBP(JNDX) = XWGTBP(JNDX)/WTMAX
 190           CONTINUE
 195        CONTINUE
         END IF
C                                       Increment the data pointer
      NBASL = NBLMAX + 1
C                                       More data to pre-average ?
      IF ((ENDAVG.LT.LASTIM) .AND. (.NOT.WEOF)) GO TO 90
C                                       End of solution interval
      NBASL = NBASL - 1
C                                       Do solution.
C                                       Set reference antenna
      IF (ACONLY) THEN
C                                       AC data
         CALL FILL (MAXANT, 1, IFITAN)
      ELSE
         IF (IBSOLV.GT.0) THEN
C                                       XC data; phase only
            CALL FILL (MAXANT, 2, IFITAN)
            IFITAN(REFANT) = -1
         ELSE
C                                       XC data; amp. and phase
            CALL FILL (MAXANT, 0, IFITAN)
            IFITAN(REFANT) = 1
            END IF
         END IF
C
      CALL SETNDX
C                                       Set initial values
      CALL RFILL (MXPARM, 0.0, BPZERO)
      IF (INITVR.GT.0) THEN
         CALL BPVAL (DISKIN, CNOIN, INITVR, CATUV, BPBUFF, LUNSBP, IPOL,
     *      SIF, SUBA, FRQSEL, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         DO 215 I = 1, NBPANT
            J = 2 * NTERMS * (I - 1) + 1
            BPZERO(J) = 2.0
 215        CONTINUE
         END IF
C                                       Transfer to DX array
      CALL DFILL (NFIT, 0.0D0, DX)
      DO 275 I = 1, NBPANT
         INDX = ISTRE(I)
         JNDX = ISTIM(I)
         KR = 2 * NTERMS * (I - 1) + 1
         KI = KR + NTERMS
         DO 250 J = 1, NTERMS
            IF (INDX.GT.0) DX(INDX+J-1) = BPZERO(KR+J-1)
            IF (JNDX.GT.0) DX(JNDX+J-1) = BPZERO(KI+J-1)
 250        CONTINUE
 275     CONTINUE
C                                       Do fit
C                                       (Re, Im)
      IF (IFITYP.EQ.1) THEN
         CALL DFIT (NFIT, DX, DF, DG)
         DTOL = DTOLB * DF
         IF (DTOL.GE.9.9D0) DTOL = 2.0D0
         DF = 0.0D0
         CALL TNAK (IERR, NFIT, DX, DF, DG, DWORK, MXWORK, DFIT,
     *      NITERB, DTOL, PRTLV)
         END IF
C                                       (Amp, Phase)
      IF ((IFITYP.EQ.2) .AND. (IBSOLV.NE.2)) THEN
         CALL DFIT2 (NFIT, DX, DF, DG)
         DTOL = DF * DTOLB
         IF (DTOL.GE.9.9D0) DTOL = 2.0D0
         DF = 0.0D0
         CALL TNAK (IERR, NFIT, DX, DF, DG, DWORK, MXWORK, DFIT2,
     *      NITERB, DTOL, PRTLV)
         END IF
C                                       (phase only - nothing else)
      IF (IBSOLV.EQ.2) THEN
         CALL DFIT3 (NFIT, DX, DF, DG)
         DTOL = DF * DTOLB
         IF (DTOL.GE.9.9D0) DTOL = 2.0D0
         DF = 0.0D0
         CALL TNAK (IERR, NFIT, DX, DF, DG, DWORK, MXWORK, DFIT3,
     *      NITERB, DTOL, PRTLV)
         END IF
      IF (IERR.EQ.2) THEN
         MSGTXT = 'WARNING: SOLVER DID NOT CONVERGE, SAVE ANSWER ANYWAY'
         CALL MSGWRT (7)
         END IF
      IF (IERR.EQ.-1) THEN
         MSGTXT = 'PARAMETER ERROR TO SOLVER - QUIT'
         GO TO 990
         END IF
C                                       Normalise coefficients ?
      IF (BPNORM) CALL CNORM (DX, NFIT)
C                                       Adjust time to center.
      ENDTIM = MIN (LASTIM, CURTIM)
      DTIMEC = (STTIME + ENDTIM) * 0.5
C                                       End of solution loop
C                                       Write solution record.
      DELT = ENDTIM - STTIME
      IF (DELT.LE.0.0) DELT = 0.01
C                                       Update BP table
      NUMSHF = 1
      IF (ACONLY) NUMSHF = 2
C                                       Reference antenna.
      IREFBP = REFANT
      IF (ACONLY) IREFBP = 0
C                                       Use IF/chan. ranges from
C                                       the original uv-file
      NPOL = 1
      IF (NCOR.GT.1) NPOL = 2
      IF ((NCOR.GE.1) .AND. (ICOR0.GT.0)) NPOL = 1
      IBCHAN = 1
C                                       Check for SU_ID < 0
      IBPSOU = ISOU
      IF (ISOU.LT.0) THEN
         IF (SINGLE) THEN
            IBPSOU = 1
         ELSE
            IBPSOU = SOUWAN(1)
            END IF
         END IF
C
      CALL BPUPD (DISKIN, CNOIN, BPOVER, CATUV, BPBUFF, LUNSBP, IPOL,
     *   SIF, MAXTEL, NPOL, NIFMAX, NCHMAX, IBCHAN, NUMSHF, DTIMEC,
     *   DELT, IBPSOU, SUBA, FINC(SIF), FRQSEL, DSHIFT, DX, NFIT,
     *   IREFBP, IERR)
C                                        Loop back for next time range
      IF ((.NOT.WEOF) .AND. (IERR.EQ.0)) GO TO 10
      GO TO 999
C                                        Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('BASOLV: ERROR',I3,' INITING UV FILE')
 1100 FORMAT ('BASOLV: ERROR',I3,1X,A4,'ING UV FILE')
 1130 FORMAT ('BASOLV: Bad baseline code=',I4,'-',I4,' no. ant.=',I4)
 1150 FORMAT ('BASOLV: ERROR',I4,' FROM GETSOU FOR SUID:',I4)
      END
      SUBROUTINE INITEX (OPCODE, IERR)
C-----------------------------------------------------------------------
C Iintializes and closes down the external channel 0 file.
C Inputs:
C     OPCODE      C*4     "INIT', 'CLOS'
C Outputs:
C     IERR        I        Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER OPCODE*4
C
      CHARACTER STAT*4, TYPTMP*2, FILE3*48
      INTEGER   JERR
      LOGICAL   T, F
      INCLUDE 'CPASS.INC'
      INCLUDE 'EZERO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IERR = 1
C                                       Initing or closing
      IF (OPCODE.EQ.'INIT') THEN
C                                       Open the UV file
         CALL ZPHFIL ('UV', DISK3, CNOIN3, 1, FILE3, IERR)
         CALL ZOPEN (LUN3, FIND3, DISK3, FILE3, T, F, T, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
C                                       Compressed data?
         ISCMP3 = CATI3(KINAX).EQ.1
C                                       Find pointers for compressed
C                                       data
         IF (ISCMP3) THEN
            CALL AXEFND (8, 'WEIGHT  ', CATI3(KIPCN), CATH3(KHPTP),
     *         KLOCW3, JERR)
C                                       Must have this one
            IF ((JERR.NE.0) .OR. (KLOCW3.LT.0)) THEN
               IERR = 5
               MSGTXT = 'INITEX: CANNOT FIND WEIGHT & SCALE FOR ' //
     *            'COMPRESSED DATA'
               GO TO 990
               END IF
            CALL AXEFND (8, 'SCALE   ', CATI3(KIPCN), CATH3(KHPTP),
     *            KLOCS3, JERR)
C                                       Get data decompression pointers
            CALL CMPRM3 (BIF, EIF, 1, 1, 1, 1)
         ELSE
            KLOCW3 = -1
            KLOCS3 = -1
            END IF
         GO TO 999
         END IF
C                                       Close files
C                                       UV data file
      IF (OPCODE.EQ.'CLOS') THEN
         MSGSUP = 32000
         CALL ZCLOSE (LUN3, FIND3, IERR)
         MSGSUP = 0
C                                       Clear status
         STAT = 'CLRD'
         TYPTMP = 'UV'
         CALL CATDIR ('CSTA', DISK3, CNOIN3, NAME3, CLAS3, SEQ3, TYPTMP,
     *      NLUSER, STAT, BUFF3, IERR)
         IF (IERR.EQ.10) IERR = 0
         IF (IERR.NE.0) GO TO 999
         IERR = 0
         GO TO 999
         END IF
C                                       If here wrong opcode
      WRITE (MSGTXT,1040) OPCODE
      IERR = 1
      GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('INITEX: ERROR ',I3,' OPENING UV DATA FILE')
 1040 FORMAT ('INITEX: WRONG OPCODE = ',A4)
      END
      SUBROUTINE EXTDIV (LRPARM, VIS, IERR)
C-----------------------------------------------------------------------
C  Routine to divide a spectrum by 'channel 0'; where 'channel 0'
C  is a pseudo-continuum uv database in an external file. In order
C  for this to work it is necessary that the external file and the
C  spectral line file be as similar as possible, i.e. same number
C  of visibilities, same order of data, same polzns, same IF's
C  etc. In order to determine which vis. record to use from the
C  'channel 0' file the vis. record number currently being used in
C  the line file is recorded in a common. Also when scans are
C  skipped this is passed to EXTDIV, therefore it is convenient
C  to structure EXTDIV somewhat like subroutine DATGET but without
C  all the multi-source stuff.
C  Inputs:
C     LRPARM      I(*)       random parms from line data set
C     VIS         R(*)       Spectral line visibility array
C  Outputs:
C     IERR        I          Error code, 0 => OK
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER IERR
      REAL    VIS(*), LRPARM(*)
C
      INTEGER BO, TIT(3), I, CMPNT3, IPOLPT, NPOLDO, CMPNT4, LOOPIF,
     *   INDEX, LOOPS, JNDEX, INP, RECOFF, BOFF, BPFVIS, BPLVIS, A1, A2,
     *   A1O, A2O
      REAL    TIMEL, TIME0, IBLL, IBL0, TITSEC, CHZ(MAXIF*12), DENOM,
     *   TEMP
      LOGICAL FLAGD, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'EZERO.INC'
      INCLUDE 'CPASS.INC'
      SAVE BPFVIS, BPLVIS
      DATA BO /1/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Need to reinit uv data?
      IF (DOUVIN) THEN
C                                       FSTVS3 now means current record
C                                       NREAD3 and FSTRD3 should be ok
C        NREAD3 = LSTVS3 - FSTVS3 + 1
C        FSTRD3 = FSTVS3 - 1
         BUFSZ3 = UVBFSL * 2
C                                       No data
         IF (NREAD3.LT.1) THEN
            IERR = 1
            MSGTXT = 'EXTDIV: NO DATA SELECTED'
            GO TO 990
            END IF
         CALL UVINIT ('READ', LUN3, FIND3, NREAD3, FSTRD3, LREC3,
     *      LENBU3, BUFSZ3, BUFF3, BO, BIND3, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
         CALL UVDISK ('READ', LUN3, FIND3, BUFF3, NIO3, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         BPFVIS = FSTRD3 + 1
         BPLVIS = BPFVIS + NIO3 - 1
         DOUVIN = F
         END IF
C                                       Is our record in this buffer
 10   IF (RECNO3.LT.BPFVIS) THEN
         MSGTXT = 'EXTDIV: LINE & CH. 0 DATA OUT OF SEQUENCE'
         IERR = 1
         GO TO 990
         END IF
      IF (RECNO3.LE.BPLVIS) THEN
C                                       We've got it
         RECOFF = RECNO3 - BPFVIS
         BOFF = BIND3 + LREC3*RECOFF
         GO TO 50
         END IF
C                                       Pull next buffer
      CALL UVDISK ('READ', LUN3, FIND3, BUFF3, NIO3, BIND3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      BPFVIS = BPLVIS + 1
      BPLVIS = BPFVIS + NIO3 - 1
C                                       Should never reach end
C                                       of scan, that's taken care
C                                       of by line data
      IF (NIO3.LE.0) THEN
         MSGTXT = 'EXTDIV: UNEXPECTED END OF SCAN IN CH. 0 DATA'
         IERR = 4
         GO TO 990
         END IF
      GO TO 10
C                                       Check if matches
 50   TIMEL = LRPARM(ILOCT+1)
      TIME0 = BUFF3(BOFF+ILOCT3)
      IF (ILOCB.GE.0) THEN
         IBLL  = LRPARM(ILOCB+1)
         IBL0  = BUFF3(BOFF+ILOCB3)
         IF ((TIMEL.NE.TIME0) .OR. (IBLL.NE.IBL0)) THEN
            IERR = 2
            CALL PTIME (TIMEL, .FALSE., TIT, TITSEC)
            WRITE (MSGTXT,1020) TIT, TITSEC
            GO TO 990
            END IF
      ELSE
         A1 = LRPARM(1+ILOCA1) + 0.01
         A2 = LRPARM(1+ILOCA2) + 0.01
         A1O  = BUFF3(BOFF+ILCA13) + 0.01
         A2O  = BUFF3(BOFF+ILCA23) + 0.01
         IF ((TIMEL.NE.TIME0) .OR. (A1.NE.A1O) .OR. (A2.NE.A2O)) THEN
            IERR = 2
            CALL PTIME (TIMEL, .FALSE., TIT, TITSEC)
            WRITE (MSGTXT,1020) TIT, TITSEC
            GO TO 990
            END IF
         END IF
C                                       Compressed data - decompress
      IF (ISCMP3) THEN
         DO 120 I = 1,NDECM3
            CMPNT3 = BOFF + NPARM3 + DECM3(2,I)/3
            CMPNT4 = 1 + DECM3(2,I)
            CALL ZUVXPN (DECM3(1,I), BUFF3(CMPNT3),
     *         BUFF3(BOFF+KLOCW3), CHZ(CMPNT4))
 120         CONTINUE
      ELSE
C                                       Uncompressed data
         CMPNT4 = 1
         CALL RCOPY ((LREC3-NPARM3), BUFF3(BOFF+NPARM3), CHZ(1))
         END IF
C                                       Since the channel zero
C                                       data is not selected via
C                                       UVGET as is the line data,
C                                       we have to make sure that
C                                       we chose the correct
C                                       STOKES and IF data, so I key
C                                       on the LINE ICOR0 value,
C                                       and the BIF.
      IF (INCOR.EQ.NCOR) IPOLPT = 0
      IF ((INCOR.GT.1) .AND. (NCOR.EQ.1)) IPOLPT = ABS(ICOR0) - 1
      NPOLDO = NCOR
      IF (NCOR.GT.1) NPOLDO = 2
C                                       Do the division
      DO 300 LOOPS = 1, NPOLDO
         DO 200 LOOPIF = 1, KNIF
            INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-1) * INCIF
            JNDEX = CMPNT4 + (LOOPS-1+IPOLPT) * INCS3 +
     *         (LOOPIF-1) * INCIF3
            DENOM = CHZ(JNDEX) * CHZ(JNDEX) +
     *              CHZ(JNDEX+1) * CHZ(JNDEX+1)
            FLAGD = .FALSE.
            IF (DENOM.LE.0) THEN
               DENOM = 1.0
               FLAGD = .TRUE.
               END IF
            DO 100 I = 1, NUMFRQ
               INP = INDEX + (I-1) * INCF
               TEMP = VIS(INP)
               VIS(INP)   = (CHZ(JNDEX)*TEMP +
     *                       CHZ(JNDEX+1)*VIS(INP+1)) / DENOM
               VIS(INP+1) = (CHZ(JNDEX)*VIS(INP+1) -
     *                       CHZ(JNDEX+1)*TEMP) / DENOM
               IF (FLAGD) VIS(INP+2) = -1
 100           CONTINUE
 200        CONTINUE
 300     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXTDIV: ERROR ',I3,' INITING EXT. CH. 0 UV FILE')
 1010 FORMAT ('EXTDIV: ERROR ',I3,' READING EXT. CH. 0 UV FILE')
 1020 FORMAT ('EXTDIV: LINE & CH. 0 MISMATCH AT ',I4,'/',2I3,F4.0)
      END
      SUBROUTINE CMPRM3 (BIFNO, EIFNO, FCHAN, LCHAN, FCHANS, LCHANS)
C-----------------------------------------------------------------------
C   Determines the number of blocks of data in a compressed visibility
C   record to decompress and the length and offsets of these blocks.
C   It is assumed that UVPGET has been called with the relevant CATBLK
C   and the values in DUVH.INC are correct.
C   Inputs:
C      BIFNO    I       First IF number
C      EIFNO    I       Highest IF number
C      FCHAN    I       First channel number
C      LCHAN    I       Highest channel number
C      FCHANS   I       First channel number if smoothing
C      LCHANS   I       Last channel number if smoothing
C   Inputs from common:
C      CATI3    I(256)  Catalogue header record of compressed file
C   Output IN COMMON:
C      NDECM3   I        Number of entries in DECM3
C      DECM3    I(2,*)   (1,*) = number of packed correlator values
C                        (2,*) = 0-rel offset in vis data.
C                        (from beginning of vis data NOT ran. parms.)
C-----------------------------------------------------------------------
      INTEGER   BIFNO, EIFNO, FCHAN, LCHAN, FCHANS, LCHANS
C
      INTEGER   I, J, LIMS, NIF, NCHAN, IFCHAN, ILCHAN
      INCLUDE 'CPASS.INC'
      INCLUDE 'EZERO.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Packed data?
      IF (CATI3(KINAX).GT.1) GO TO 999
C                                       Smoothed = use wider range
C                                       of channels
      IF ((FCHANS.GT.0) .AND. (LCHANS.GT.0)) THEN
         IFCHAN = FCHANS
         ILCHAN = LCHANS
      ELSE
         IFCHAN = FCHAN
         ILCHAN = LCHAN
         END IF
C                                       Check if all requested.
      NIF = 1
      IF (JLOCI3.GT.0) NIF = CATI3(KINAX+JLOCI3)
      NCHAN = CATI3(KINAX+JLOCF3)
      IF (((BIFNO.EQ.1) .AND. (EIFNO.EQ.NIF)) .AND.
     *   ((IFCHAN.EQ.1) .AND. (ILCHAN.EQ.NCHAN))) THEN
         NDECM3 = 1
         DECM3(1,1) = LREC3 - NPARM3
         DECM3(2,1) = 0
         GO TO 999
         END IF
C                                       Continuum
      IF (NCHAN.LE.1) THEN
         NDECM3 = 1
         DECM3(1,NDECM3) = (EIFNO-BIFNO+1) * CATI3(KINAX+JLOCS3)
         DECM3(2,NDECM3) = (BIFNO-1) * INCIF3
         GO TO 999
         END IF
C                                       Line
      IF ((JLOCS3.LT.JLOCF3) .AND. ((JLOCI3.GE.3) .OR. (JLOCI3.LE.0)))
     *   THEN
C                                       Stokes most rapid var.
         NDECM3 = 0
         DO 200 I = BIFNO,EIFNO
            NDECM3 = NDECM3 + 1
            DECM3(1,NDECM3) = (ILCHAN-IFCHAN+1) * CATI3(KINAX+JLOCS3)
            DECM3(2,NDECM3) = (IFCHAN-1) * INCF + (I-1) * INCIF3
 200        CONTINUE
         GO TO 999
         END IF
      IF ((JLOCF3.LT.JLOCS3) .AND. ((JLOCI3.GE.3) .OR. (JLOCI3.LE.0)))
     *   THEN
C                                       Frequency most rapid var.
         LIMS = CATI3(KINAX+JLOCS3)
         NDECM3 = 0
         DO 300 I = BIFNO,EIFNO
            DO 300 J = 1,LIMS
               NDECM3 = NDECM3 + 1
               DECM3(1,NDECM3) = ILCHAN - IFCHAN + 1
               DECM3(2,NDECM3) = (IFCHAN-1) * INCF3 + (I-1) * INCIF3 +
     *            (J-1) * INCS3
 300           CONTINUE
         GO TO 999
         END IF
C                                       Anything else - do all

      NDECM3 = 1
      DECM3(1,1) = LREC3 - NPARM3
      DECM3(2,1) = 0
C
 999  RETURN
      END
      SUBROUTINE BPUPD (IDISK, ICNO, IBPVER, JCATBL, JBUFF, ILUN, IPOL,
     *   IIF, NUMANT, NUMPOL, NUMIF, NUMFRQ, IBCHAN, NUMSHF, DTIME,
     *   TINT, ISOU, ISUBA, BANDW, IFQID, DSHIFT, DX, NPARM, IREFA,
     *   IRET)
C----------------------------------------------------------------------
C   Update BP table to include solutions just determined.
C   Inputs:
C      IDISK    I     Disk volume number.
C      ICNO     I     Catalog number.
C      IBPVER   I     BP table to update.
C      JCATBL   I(*)  Catalog header block.
C      JBUFF    I(*)  Buffer for table I/O.
C      ILUN     I     LU number for table I/O.
C      IPOL     I     Polarization of new solution (1=R; 2=L).
C      IIF      I     IF number of new solution.
C      NUMANT   I     Number of antennas.
C      NUMPOL   I     Number of polarizations in table.
C      NUMIF    I     Number of IF channels in table.
C      NUMFRQ   I     Dimension of frequency axix in data.
C      IBCHAN   I     Start channel of BP solution.
C      NUMSHF   I     1=> XC bandpass; 2=> AC bandpass solution.
C      DTIME    D     Time of bandpass solution.
C      TINT     R     Solution interval of BP solution.
C      ISOU     I     Source ID of BP solution.
C      ISUBA    I     Subarray number.
C      BANDW    R     Channel spacing in bandpass solution.
C      IFQID    I     FQ_ID of BP solution.
C      DSHIFT   D(*)  Max. channel shift for each antenna.
C      DX       D(*)  Solution vector for bandpass coefficients.
C      NPARM    I     Number of elements in DX.
C      IREFA    I     Reference antenna number.
C   Output:
C      IRET     I     Return code (0 => ok; else error).
C----------------------------------------------------------------------
      INTEGER   IDISK, ICNO, IBPVER, ILUN, IPOL, IIF, NUMANT,
     *   NUMPOL, NUMIF, NUMFRQ, IBCHAN, NUMSHF, ISOU, ISUBA,
     *   IFQID, NPARM, IREFA, IRET
      DOUBLE PRECISION DTIME, DSHIFT(NUMANT), DX(NPARM)
      REAL      TINT, BANDW
      INTEGER   JBUFF(*), JCATBL(256)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PPDAT.INC'
      LOGICAL   WMATCH(MAXANT)
      CHARACTER LBPTYP*8, WBPTYP*8
      DOUBLE PRECISION DTIM2, DSHFT2(MAXIF), DIFFT, DPOLYN(MAXCHA),
     *   DTEMP
      REAL      XLOWSH, XDELSH, TINT2, BANDW2, BNDPAS(2,MAXCIF),
     *   WEIGHT(2*MAXIF), TBP(2,MAXCIF)
      INTEGER   IBPRNO, BPKOLS(MAXBPC), BPNUMV(MAXBPC), IROW, INX,
     *   NROW, ISOU2, ISUBA2, IANT2, IFQID2, IREF2(2), JNDX, INDX,
     *   I, J, NFILL, KR, NMAX
      INCLUDE 'BPDAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      DO 20 J = 1,MAXANT
         WMATCH(J) = .FALSE.
 20      CONTINUE
C                                       Open BP table
      XLOWSH = 0.0
      XDELSH = 0.0
      IF (IFITYP.EQ.1) LBPTYP = 'CHEB_RI'
      IF (IFITYP.EQ.2) LBPTYP = 'CHEB_AP'
      WBPTYP = LBPTYP
      IF (OFORM.GT.0) WBPTYP = ' '
C
      CALL BPINI ('WRIT', JBUFF, IDISK, ICNO, IBPVER, JCATBL, ILUN,
     *   IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, IBCHAN,
     *   NUMSHF, XLOWSH, XDELSH, WBPTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, IBPVER
         GO TO 990
         END IF
C
      NROW = JBUFF(5)
      DO 500 IROW = 1,NROW
C                                       Read record
         IBPRNO = IROW
         CALL TABBP ('READ', JBUFF, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, DTIM2, TINT2, ISOU2, ISUBA2, IANT2, BANDW2,
     *      DSHFT2, IFQID2, IREF2, WEIGHT, BNDPAS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET, IBPVER
            GO TO 990
            END IF
C                                       Check subarray, fq_id and
C                                       ant_no for match
         IF ((ISUBA2.NE.ISUBA) .OR. (IFQID2.NE.IFQID) .OR.
     *      (IANDX(IANT2).LE.0)) GO TO 500
C                                       Check time stamp for match
         DIFFT = ABS (DTIM2 - DTIME)
C                                       Entries match
         IF (DIFFT.GT.(0.1*TINT2)) GO TO 500
            INDX = ISTRE(IANT2)
            JNDX = ISTIM(IANT2)
C                                       Copy solution for this antenna
C                                       to BP table arrays
            KR = (IANT2 - 1) * 2 * NTERMS
            INX = (IIF - 1) * NUMFRQ + (IPOL-1) * NUMFRQ * NUMIF
            DO 200 J = 1,NUMFRQ
               INX = INX + 1
               IF (J.LE.NTERMS) THEN
                  BNDPAS(1,INX) = BPZERO(KR+J)
                  BNDPAS(2,INX) = BPZERO(KR+NTERMS+J)
                  IF (INDX.GT.0) BNDPAS(1,INX) = DX(INDX+J-1)
                  IF (JNDX.GT.0) BNDPAS(2,INX) = DX(JNDX+J-1)
               ELSE
                  BNDPAS(1,INX) = FBLANK
                  BNDPAS(2,INX) = FBLANK
                  END IF
 200           CONTINUE
C                                       Average shift
            DSHFT2(IIF) = DSHIFT(IANT2)
C                                       Reference antenna
            IF (IREFA.GT.0) THEN
               IREF2(IPOL) = IREFA
            ELSE
               IREF2(IPOL) = IANT2
               END IF
C                                       switch to real BP form?
            IF (OFORM.GT.0) THEN
               INX = (IIF - 1) * NUMFRQ + (IPOL-1) * NUMFRQ * NUMIF + 1
               DTEMP = 0.0D0
               CALL BPCOEF (LBPTYP, BNDPAS(1,INX), BNDPAS(2,INX), 2, 2,
     *            NTERMS, FBLANK, DTEMP, TBP(1,INX), TBP(2,INX), 2, 2,
     *            1, NUMFRQ, 1.0, FLOAT(NUMFRQ), 0, .FALSE., DPOLYN,
     *            MAXCHA, NMAX, .FALSE., IRET)
               IF (IRET.NE.0) GO TO 999
               J = 2 * NUMFRQ
               CALL SPFIX (IIF, NUMFRQ, TBP(1,INX), BNDPAS(1,INX))
               END IF
C                                       Re-write BP table record
C                                       ???? temporary ???
            WEIGHT(IIF+(IPOL-1)*NUMIF) = TINT * 24. * 360.
C
            IBPRNO = IROW
            CALL TABBP ('WRIT', JBUFF, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *         NUMFRQ, NUMPOL, DTIME, TINT, ISOU, ISUBA, IANT2, BANDW,
     *         DSHFT2, IFQID, IREF2, WEIGHT, BNDPAS, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1030) IRET, IBPVER
               GO TO 990
               END IF
C
            WMATCH(IANT2) = .TRUE.
 500     CONTINUE
C                                       Do new records need to be
C                                       written ?
      IBPRNO = NROW + 1
      DO 700 I = 1,NBPANT
C                                       Already written ?
         IF (WMATCH(I) .OR. (IANDX(I).LE.0)) GO TO 700
C                                       Average shift
         CALL DFILL (MAXIF, 0.0D0, DSHFT2)
         DSHFT2(IIF) = DSHIFT(I)
C                                       Reference antenna
         CALL FILL (2, 0, IREF2)
         IF (IREFA.GT.0) THEN
            IREF2(IPOL) = IREFA
         ELSE
            IREF2(IPOL) = I
            END IF
C                                       Bandpass arrays
         NFILL = NUMPOL * NUMFRQ * NUMIF * 2
         CALL RFILL (NFILL, FBLANK, BNDPAS)
         INDX = ISTRE(I)
         JNDX = ISTIM(I)
C                                       ???? temporary ???
         WEIGHT(IIF+(IPOL-1)*NUMIF) = TINT * 24. * 360.
C
         KR = (I - 1) * 2 * NTERMS
         INX = (IIF - 1) * NUMFRQ + (IPOL-1) * NUMFRQ * NUMIF
         DO 600 J = 1,NUMFRQ
            INX = INX + 1
            IF (J.LE.NTERMS) THEN
               BNDPAS(1,INX) = BPZERO(KR+J)
               BNDPAS(2,INX) = BPZERO(KR+NTERMS+J)
               IF (INDX.GT.0) BNDPAS(1,INX) = DX(INDX+J-1)
               IF (JNDX.GT.0) BNDPAS(2,INX) = DX(JNDX+J-1)
            ELSE
               BNDPAS(1,INX) = FBLANK
               BNDPAS(2,INX) = FBLANK
               END IF
 600        CONTINUE
C                                       switch to real BP form?
         IF (OFORM.GT.0) THEN
            INX = (IIF - 1) * NUMFRQ + (IPOL-1) * NUMFRQ * NUMIF + 1
            DTEMP = 0.0D0
            CALL BPCOEF (LBPTYP, BNDPAS(1,INX), BNDPAS(2,INX), 2, 2,
     *         NTERMS, FBLANK, DTEMP, TBP(1,INX), TBP(2,INX), 2, 2,
     *         1, NUMFRQ, 1.0, FLOAT(NUMFRQ), 0, .FALSE., DPOLYN,
     *         MAXCHA, NMAX, .FALSE., IRET)
            IF (IRET.NE.0) GO TO 999
            J = 2 * NUMFRQ
            CALL SPFIX (IIF, NUMFRQ, TBP(1,INX), BNDPAS(1,INX))
            END IF
C                                       Write record
         CALL TABBP ('WRIT', JBUFF, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, DTIME, TINT, ISOU, ISUBA, I, BANDW, DSHFT2,
     *      IFQID, IREF2, WEIGHT, BNDPAS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET, IBPVER
            GO TO 990
            END IF
 700     CONTINUE
C                                       Close BP table
      CALL TABIO ('CLOS', 0, 0, JBUFF, JBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET, IBPVER
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      GO TO 999
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1010 FORMAT ('BPUPD: ERROR',I4,' OPENING BP TABLE:',I4)
 1020 FORMAT ('BPUPD: ERROR',I4,' READING BP TABLE:',I4)
 1030 FORMAT ('BPUPD: ERROR',I4,' WRITING BP TABLE:',I4)
 1040 FORMAT ('BPUPD: ERROR',I4,' CLOSING BP TABLE:',I4)
      END
      SUBROUTINE SPFIX (IIF, NUMF, BPIN, BPOUT)
C-----------------------------------------------------------------------
C   applies spectral index correction
C   Inputs:
C      IIF      I        IF in question
C      NUMFRQ   I        Number spectral channels
C      BPIN     r(2,*)   uncorrected BP (normal type)
C   Outputs
C      BPOUT    R(2,*)   corrected BP
C-----------------------------------------------------------------------
      INTEGER   IIF, NUMF
      REAL      BPIN(2,*), BPOUT(2,*)
C
      INTEGER   IFRQ
      REAL      RMUL, FMUL, CATUVR(256)
      INCLUDE 'CPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATUV, CATUVR)
C-----------------------------------------------------------------------
C                                       ref chan multiplier
      IF (XSPEC.NE.0.0) THEN
         FMUL = (1.0 + NUMF) / 2.0
         RMUL = LOG10 (((FREQ + FOFF(IIF) +
     *         (FMUL-CATUVR(KRCRP+KLOCFY)) * FINC(IIF)) / 1.D9))
         RMUL =  RMUL * (XSPEC + RMUL * (ACURVE(1) + RMUL *
     *      (ACURVE(2) + RMUL * (ACURVE(3) + RMUL *ACURVE(4)))))
         RMUL = 10.0 ** (-RMUL/2.0D0)
         END IF
      DO 20 IFRQ = 1,NUMF
C                                       channel multiplier
         IF (XSPEC.NE.0.0) THEN
            FMUL = LOG10 (((FREQ + FOFF(IIF) +
     *         (IFRQ-CATUVR(KRCRP+KLOCFY)) * FINC(IIF)) / 1.D9))
            FMUL =  FMUL * (XSPEC + FMUL * (ACURVE(1) + FMUL *
     *         (ACURVE(2) + FMUL * (ACURVE(3) + FMUL*ACURVE(4)))))
C                                       scale so ref channel value
C                                       unchanged (was set in SETJY)
            FMUL = (10.0 ** (-FMUL/2.0D0)) / RMUL
         ELSE
            FMUL = 1.0
            END IF
         BPOUT(1,IFRQ) = BPIN(1,IFRQ) * FMUL
         BPOUT(2,IFRQ) = BPIN(2,IFRQ) * FMUL
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SETNDX
C-----------------------------------------------------------------------
C   Compute indexing table for solution parameters.
C-----------------------------------------------------------------------
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PPDAT.INC'
      LOGICAL WEMPTY
      INTEGER I, K, ISPEC
      INCLUDE 'BPDAT.INC'
C-----------------------------------------------------------------------
C                                       Fill index table of
C                                       active antennas
      CALL FILL (NBPANT, -1, IANDX)
      CALL FILL (NBPANT, -1, ISTRE)
      CALL FILL (NBPANT, -1, ISTIM)
C
      DO 50 I = 1, NBASL
         ISPEC = (I - 1) * NBPCHN + 1
C                                       Any valid data ?
         WEMPTY = .TRUE.
         DO 30 K = 1, NBPCHN
            IF (XWGTBP(ISPEC+K-1).GT.0.0) WEMPTY = .FALSE.
 30         CONTINUE
C
         IF (.NOT.WEMPTY) THEN
            IANDX(IBASL(I)) = 1
            IANDX(JBASL(I)) = 1
            END IF
 50      CONTINUE
C                                          Now compute lookup table
      K = 1
      DO 100 I = 1, NBPANT
         IF ((IANDX(I).LE.0) .OR. (IFITAN(I).LT.0)) GO TO 100
         IF ((IFITAN(I).EQ.0) .OR. (IFITAN(I).EQ.1)) THEN
            ISTRE(I) = K
            K = K + NTERMS
            END IF
         IF ((IFITAN(I).EQ.0) .OR. (IFITAN(I).EQ.2)) THEN
            ISTIM(I) = K
            K = K + NTERMS
            END IF
 100     CONTINUE
C
      NFIT = K - 1
C
      RETURN
      END
      SUBROUTINE XPSCAL (XRE, XIM, N)
C-------------------------------------------------------------------
C   Auto-scale a spectrum to zero mean phase and unit amplitude.
C   Inputs:
C      XRE     R(*)    Real parts of spectrum.
C      XIM     R(*)    Imaginary parts of spectrum.
C      N       I       Dimension of XRE, XIM.
C-------------------------------------------------------------------
      INTEGER N
      REAL XRE(N), XIM(N)
C
      DOUBLE PRECISION DSUMA, DSUMRE, DSUMIM, DPHASE, DCOSP,
     *   DSINP, DFACT
      REAL XRETMP, XIMTMP
      INTEGER I
C-------------------------------------------------------------------
C                                       Initialization
      DSUMA = 0.0D0
      DSUMRE = 0.0D0
      DSUMIM = 0.0D0
C                                       Sum amp. and phase
      DO 100 I = 1, N
         DSUMA = DSUMA + SQRT (XRE(I) * XRE(I) + XIM(I) * XIM(I))
         DPHASE = ATAN2 (XIM(I), XRE(I))
         DSUMRE = DSUMRE + COS (DPHASE)
         DSUMIM = DSUMIM + SIN (DPHASE)
 100     CONTINUE
C                                       Normalize
      IF (DSUMA.GT.0.0) THEN
         DFACT = N / DSUMA
         DPHASE = ATAN2 (DSUMIM, DSUMRE)
         DSINP = SIN (DPHASE)
         DCOSP = COS (DPHASE)
C                                       Correct data
         DO 200 I = 1,N
C                                       Scale amplitude
            XRETMP = XRE(I) * DFACT
            XIMTMP = XIM(I) * DFACT
C                                       Rotate phase
            XRE(I) = XRETMP * DCOSP + XIMTMP * DSINP
            XIM(I) = XIMTMP * DCOSP - XRETMP * DSINP
 200        CONTINUE
         END IF
C                                       Exit
 999  RETURN
      END
      SUBROUTINE CNORM (DX, NPARM)
C---------------------------------------------------------------------
C   Normalize the bandpass coefficients.
C   Inputs:
C      DX       D(*)   Array of bandpass coefficients.
C      NPARM    I      Number of polynomial terms per antenna.
C---------------------------------------------------------------------
      INTEGER NPARM
      DOUBLE PRECISION DX(NPARM)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PPDAT.INC'
      CHARACTER LTYPE*8
      LOGICAL WF
      DOUBLE PRECISION DPOLYN(MXTERM), DSUM, DFACT, DSHIFT
      REAL A, B, CR(MAXCHA), CI(MAXCHA), XRE, XIM
      INTEGER IANT, IFRQ, I, INDX, JNDX, KR, KI, NMAX, IRET
      INCLUDE 'BPDAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA WF /.FALSE./
C---------------------------------------------------------------------
      A = 1.0
      B = NBPCHN
C
      DO 500 IANT = 1,NBPANT
         IF (IANDX(IANT).LT.0) GO TO 500
         DSUM = 0.0D0
         CALL RFILL (NTERMS, 0.0, CR)
         CALL RFILL (NTERMS, 0.0, CI)
C                                       Extract real, imag. coeff.
         INDX = ISTRE(IANT)
         JNDX = ISTIM(IANT)
         KR = 2 * NTERMS * (IANT - 1) + 1
         KI = KR + NTERMS
         DO 300 I = 1, NTERMS
            IF (INDX.GT.0) THEN
               CR(I) = DX(INDX+I-1)
            ELSE
               CR(I) = BPZERO(KR+I-1)
               END IF
            IF (JNDX.GT.0) THEN
               CI(I) = DX(JNDX+I-1)
               END IF
 300        CONTINUE
C                                       Sum amplitude
         IF (IFITYP.EQ.1) LTYPE = 'CHEB_RI '
         IF (IFITYP.EQ.2) LTYPE = 'CHEB_AP '
         DO 400 IFRQ = 1, NBPCHN
C                                       Compute BP value in
C                                       channel IFRQ (zero shift)
            DSHIFT = 0.0D0
            CALL BPCOEF (LTYPE, CR, CI, 1, 1, NTERMS, FBLANK,
     *         DSHIFT, XRE, XIM, 1, 1, IFRQ, 1, A, B, 0, WF,
     *         DPOLYN, MXTERM, NMAX, WF, IRET)
            DSUM = DSUM + SQRT (XRE * XRE + XIM * XIM)
 400        CONTINUE
C                                       Normalize coefficients.
         DFACT = FLOAT (NBPCHN) / DSUM
         DO 450 I = 1,NTERMS
            IF (INDX.GT.0) THEN
               DX(INDX+I-1) = DX(INDX+I-1) * DFACT
            ELSE
               BPZERO(KR+I-1) = BPZERO(KR+I-1) * DFACT
               END IF
            IF ((JNDX.GT.0) .AND. (IFITYP.EQ.1)) DX(JNDX+I-1) =
     *         DX(JNDX+I-1) * DFACT
 450        CONTINUE
 500     CONTINUE
C
      RETURN
      END
      SUBROUTINE BPVAL (IDISK, ICNO, IBPVER, JCATBL, JBUFF, ILUN, IPOL,
     *   IIF, ISUBA, IFQID, IRET)
C----------------------------------------------------------------------
C   Read initial coefficients from an existing BP table.
C   Inputs:
C      IDISK   I        Disk volume number.
C      ICNO    I        Catalog slot number.
C      IBPVER  I        BP table version number.
C      JCATBL  I(*)     Catalog header.
C      JBUFF   I(512)   Buffer for table I/O.
C      ILUN    I        LUN to use for table I/O.
C      IPOL    I        Desired polarization IN BP TABLE
C      IIF     I        Desired IF channel.
C      ISUBA   I        Desired subarray number.
C      IFQID   I        Frequency ID.
C   Output:
C      IRET    I        Return code (0 => ok; else error)
C----------------------------------------------------------------------
      INTEGER IDISK, ICNO, IBPVER, ILUN, IPOL, IIF,
     *   ISUBA, IFQID, IRET
      INTEGER JBUFF(*), JCATBL(256)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PPDAT.INC'
      CHARACTER LBPTYP*8
      DOUBLE PRECISION DTIM2, DSHFT2(MAXIF)
      REAL      XLOWSH, XDELSH, TINT2, BANDW2, BNDPAS(2,MAXCIF),
     *   WEIGHT(2*MAXIF)
      INTEGER   IBPRNO, BPKOLS(MAXBPC), BPNUMV(MAXBPC), IROW, NROW,
     *   ISOU2, ISUBA2, IANT2, IFQID2, IREF2(2), J, NUMFRQ, NUMSHF,
     *   NUMANT, NUMPOL, NUMIF, IBCHAN, KR, KI, INX
      LOGICAL   OPEN
      INCLUDE 'BPDAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      OPEN = .FALSE.
C                                       Open BP table
      CALL BPINI ('READ', JBUFF, IDISK, ICNO, IBPVER, JCATBL, ILUN,
     *   IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, IBCHAN,
     *   NUMSHF, XLOWSH, XDELSH, LBPTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, IBPVER
         GO TO 990
         END IF
      OPEN = .TRUE.
C                                       Check for compatible
C                                       data type
      INITYP = 0
      IF (LBPTYP.EQ.'CHEB_RI') INITYP = 1
      IF (LBPTYP.EQ.'CHEB_AP') INITYP = 2
      IF (INITYP.NE.IFITYP) THEN
         IRET = 8
         WRITE (MSGTXT,1015) IBPVER, INITYP
         GO TO 990
         END IF
C
      NROW = JBUFF(5)
      DO 500 IROW = 1, NROW
C                                       Read record
         IBPRNO = IROW
         CALL TABBP ('READ', JBUFF, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, DTIM2, TINT2, ISOU2, ISUBA2, IANT2, BANDW2,
     *      DSHFT2, IFQID2, IREF2, WEIGHT, BNDPAS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET, IBPVER
            GO TO 990
            END IF
C                                       Check subarray, fq_id
         IF ((ISUBA2.NE.ISUBA) .OR. (IFQID2.NE.IFQID)) GO TO 500
C                                       Copy solution for this antenna
C                                       to parameter vector BPZERO
            KR = 2 * (IANT2 - 1) * NTERMS
            KI = KR + NTERMS
            IF (KI.GT.(MXPARM-NTERMS)) THEN
               MSGTXT = 'BPVAL: OVERFLOW BPZERO'
               IRET = 9
               GO TO 990
               END IF
C                                       Fill initial value array
            INX = (IIF - 1) * NUMFRQ + (IPOL-1) * NUMFRQ * NUMIF
            DO 200 J = 1,NTERMS
               INX = INX + 1
               BPZERO(KR+J) = BNDPAS(1,INX)
               BPZERO(KI+J) = BNDPAS(2,INX)
 200           CONTINUE
 500     CONTINUE
C                                       Close BP table
      OPEN = .FALSE.
      CALL TABIO ('CLOS', 0, 0, JBUFF, JBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET, IBPVER
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990    CALL MSGWRT (8)
      IF (OPEN) THEN
         OPEN = .FALSE.
         CALL TABIO ('CLOS', 0, 0, JBUFF, JBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET, IBPVER
            GO TO 990
            END IF
         END IF
C
 999  RETURN
C----------------------------------------------------------------------
 1010 FORMAT ('BPVAL: ERROR',I4,' OPENING BP TABLE:',I4)
 1015 FORMAT ('BPVAL: INCOMPATIBLE INITIAL DATA IN BP:',I4,' TYPE',I2)
 1020 FORMAT ('BPVAL: ERROR',I4,' READING BP TABLE:',I4)
 1040 FORMAT ('BPVAL: ERROR',I4,' CLOSING BP TABLE:',I4)
      END
      SUBROUTINE BPSPEC
C-----------------------------------------------------------------------
C   BPSPEC checks the spectral index parameters in common and fills
C   in when the calsour is well known
C   Output in common
C      XSPEC    R      Spectral index - 0.0 means do none
C      XCURVE   R(3)   Spectral curvature parameters
C-----------------------------------------------------------------------
      INCLUDE 'CPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
C
      INTEGER   XSOUR, NDATES, LXSOUR
      PARAMETER (XSOUR=5, NDATES=17, LXSOUR=6)
C
      INTEGER   IRET, LUNTMP, LUN, I, J, ISRC, ID(3), IDNUM, LSRC,
     *   ICTYPE, JTRIM, NTERM
      REAL      TCOEFF(4,XSOUR), DATES(NDATES), DCOEFF(4,NDATES,3), DD,
     *   W1, W2, LCOEFF(5,LXSOUR), SCOEFF(4,3), TEMP(3), PBOEFF(6,XSOUR)
      CHARACTER KNOSOU(4,XSOUR)*16, LNOSOU(4,LXSOUR)*16, DATE*8,
     *   SNOSOU(3,3)*16
      HOLLERITH CATH(256)
      DOUBLE PRECISION DT
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATH, CATBLK)
C                                       Perley/Butler 2017
C                                       3C286
      DATA PBOEFF /1.2481, -0.4507, -0.1798, 0.0357, 0.0, 0.0,
C                                       3C48
     *             1.3253, -0.7553, -0.1914, 0.0498, 0.0, 0.0,
C                                       3C147
     *             1.4516, -0.6961, -0.2007, 0.0640, -0.0464, 0.0289,
C                                       3C138
     *             1.0088, -0.4981, -0.1552, -0.0102, 0.0223, 0.0,
C                                       3C295
     *             1.4701, -0.7658, -0.2780, -0.0347, 0.0399, 0.0/
C                                       steady sources Perley 2013
C                                       3C123
      DATA SCOEFF / 1.8077, -0.8018, -0.1157, 0.0,
C                                       3C196
     *              1.2969, -0.8690, -0.1788, 0.0305,
C                                       3C295
     *              1.4866, -0.7871, -0.3440, 0.0749/
C                                       Perley 2013 coefficients
C                                       same units as RCOEFF
C                                       3C286
      DATA TCOEFF /1.2515,  -0.4605,  -0.1715,   0.0336,
C                                       3C48 (2010)
     *             1.3197,  -0.7253,  -0.2023,   0.0540,
C                                       3C147 (2010)
     *             1.4428,  -0.6300,  -0.3142,   0.1032,
C                                       3C138 (2010)
     *             1.0053,  -0.4384,  -0.1855,   0.0511,
C                                       1934-638 (Reynolds, 02/Jul/94)
C    *           -30.7667,  26.4908,  -7.0977,   0.605334,
C                                       3C295
     *             1.4866,  -0.7871,  -0.3440,   0.0749 /
C                                       3C196
C    *             1.2969,  -0.8690,  -0.1788,   0.0305/
C                                       Source lists
      DATA KNOSOU /'3C286','1328+307','1331+305', 'J1331+3030',
     *   '3C48',    '0134+329', '0137+331', 'J0137+3309',
     *   '3C147',   '0538+498', '0542+498', 'J0542+4951',
     *   '3C138',   '0518+165', '0521+166', 'J0521+1638',
C    *   '1934-638','1934-638', '1934-638', 'J1939-6342',
     *   '3C295',   '1409+524', '1411+522', 'J1411+5212'/
C                                       date list
      DATA DATES /1983.4, 1985.9, 1987.3, 1989.9, 1995.2, 1998.1,
     *   1999.3, 2000.8, 2001.9, 2003.1, 2004.7, 2006.0, 2007.4, 2008.7,
     *   2010.0, 2010.9, 2012.0/
C                                       3C48
      DATA DCOEFF /
     *   1.3339,-.7643,-.1946,.055,   1.3350,-.7598,-.1869,.057,
     *   1.3361,-.7577,-.1905,.048,   1.3363,-.7605,-.1965,.057,
     *   1.3359,-.7673,-.2041,.059,   1.3342,-.7732,-.2078,.065,
     *   1.3342,-.7682,-.2097,.056,   1.3323,-.7654,-.2091,.060,
     *   1.3342,-.7708,-.2014,.059,   1.3341,-.7691,-.2006,.057,
     *   1.3341,-.7641,-.2102,.059,   1.3335,-.7705,-.2008,.058,
     *   1.3335,-.7660,-.1982,.051,   1.3361,-.7700,-.2119,.076,
     *   1.3334,-.7662,-.1988,.062,   1.3332,-.7665,-.1980,.064,
     *   1.3324,-.7690,-.1950,.059,
C                                       3C147
     *   1.4620,-.7085,-.2347,.051,   1.4648,-.7177,-.2501,.089,
     *   1.4624,-.7115,-.2336,.071,   1.4646,-.7194,-.2532,.092,
     *   1.4632,-.7121,-.2346,.086,   1.4641,-.7090,-.2313,.088,
     *   1.4642,-.7132,-.2424,.082,   1.4585,-.7086,-.2296,.068,
     *   1.4636,-.7124,-.2426,.084,   1.4639,-.7144,-.2453,.082,
     *   1.4635,-.7112,-.2453,.091,   1.4631,-.7136,-.2338,.094,
     *   1.4645,-.7115,-.2378,.084,   1.4625,-.7112,-.2396,.081,
     *   1.4623,-.7139,-.2405,.081,   1.4607,-.7150,-.2372,.077,
     *   1.4616,-.7187,-.2424,.079,
C                                       3C138
     *   1.0328,-.5523,-.1161,.008,   1.0337,-.5591,-.1605,.032,
     *   1.0354,-.5914,-.1032,-.005,  1.0292,-.5636,-.1857,.052,
     *   1.0145,-.5466,-.1758,.038,   1.0259,-.5679,-.1735,.039,
     *   1.0204,-.5702,-.1636,.030,   1.0081,-.5077,-.2492,.064,
     *   1.0196,-.5627,-.1823,.039,   1.0177,-.5686,-.1591,.029,
     *   1.0094,-.5003,-.2642,.085,   1.0181,-.5543,-.1486,.038,
     *   1.0149,-.5408,-.1174,.012,   1.0132,-.4941,-.1556,.045,
     *   1.0230,-.4983,-.1529,.048,   1.0207,-.5140,-.1626,.058,
     *   1.0332,-.5608,-.1197,.041/
C                                       Source lists: low freq
C                                       3C286
      DATA LCOEFF /27.477, -0.158,  0.032, -0.180,  0.000,
C                                       3C48
     *             64.768, -0.387, -0.420,  0.181,  0.000,
C                                       3C147
     *             66.738, -0.022, -1.012,  0.549,  0.000,
C                                       3C196
     *             83.084, -0.699,  0.110,  0.000,  0.000,
C                                       3c380
     *             77.352, -0.767,  0.000,  0.000,  0.000,
C                                       3C295
     *             97.763, -0.582, -0.298,  0.583, -0.363/
      DATA LNOSOU /'3C286','1328+307','1331+305', 'J1331+3030',
     *   '3C48',    '0134+329', '0137+331', 'J0137+3309',
     *   '3C147',   '0538+498', '0542+498', 'J0542+4951',
     *   '3C196',   '0809+483', '0813+482', 'J0813+4813',
     *   '3C380',   '1828+487', '1829+487', 'J1829+4844',
     *   '3C295',   '1409+524', '1411+522', 'J1411+5212'/
      DATA SNOSOU /'3C123', '0433+295', 'J0437+2946',
     *   '3C196', '0809+483', 'J0813+4813',
     *   '3C295', '1409+524', 'J1411+5212'/
C-----------------------------------------------------------------------
C                                       no spectral fit
      IF (DOSCAL.LT.0.0) THEN
         XSPEC = 0.0
C                                       default - is source known?
      ELSE IF (XSPEC.EQ.0.0) THEN
         IF (NSOUWD.GT.1) THEN
            MSGTXT = 'MORE THAN ONE CALSOUR, NO SPECTRAL INDEX DEFAULT'
            CALL MSGWRT (6)
            DOSCAL = -1.0
         ELSE
            CALL RFILL (4, 0.0, ACURVE)
            LUN = LUNTMP (1)
            CALL GETSOU (SOUWAN(1), DISKIN, CNOIN, CATUV, LUN, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'SU TABLE PROBLEM, NO SPECTRAL INDEX DEFAULT'
               CALL MSGWRT (6)
               DOSCAL = -1.0
            ELSE
               ICTYPE = 1
               IF (FREQ.LT.0.75D9) ICTYPE = -1
               CALL H2CHR (8, 1, CATH(KHDOB), DATE)
               CALL DATEST (DATE, ID)
               CALL DAYNUM (ID(1), ID(3), ID(2), IDNUM)
               DD = ID(1) + IDNUM/365.25
               IF (DD.GT.2014.0) ICTYPE = 0
               ISRC = 0
               IF (ICTYPE.EQ.1) THEN
                  DO 10 I = 1,3
                     DO 5 J = 1,3
                        IF (SNAME(:JTRIM(SNOSOU(J,I))).EQ.
     *                     SNOSOU(J,I)) ISRC = I + XSOUR
 5                      CONTINUE
 10                  CONTINUE
                  DO 20 I = 1,XSOUR
                     DO 15 J = 1,4
                        IF (SNAME(:JTRIM(KNOSOU(J,I))).EQ.KNOSOU(J,I))
     *                     ISRC = I
 15                     CONTINUE
 20                  CONTINUE
               ELSE
                  DO 40 I = 1,LXSOUR
                     DO 30 J = 1,4
                        IF (SNAME(:JTRIM(LNOSOU(J,I))).EQ.LNOSOU(J,I))
     *                     ISRC = I
 30                     CONTINUE
 40                  CONTINUE
                  END IF
C                                       non-standard source
               IF (ISRC.LE.0) THEN
                  NTERM = 1
                  IF (DOSCAL.GT.1.5) NTERM = 2
                  CALL RFILL (3, 0.0, ACURVE)
                  CALL FNDSPX (DISKIN, CNOIN, SOUWAN(1), FRQSEL,
     *               CATUV, NTERM, TEMP, IRET)
                  IF (IRET.EQ.0) THEN
                     XSPEC = TEMP(2)
                     IF (NTERM.EQ.2) ACURVE(1) = TEMP(3)
                     END IF
C                                       low frequency
               ELSE IF (ICTYPE.EQ.-1) THEN
C                                       return wrt 1 GHz, not 150 MHz
                  DT = LOG10 (1.0D3 / 150.0D0)
                  XSPEC = LCOEFF(2,ISRC) + 2.D0*DT*LCOEFF(3,ISRC) +
     *               3.D0*DT*DT*LCOEFF(4,ISRC) +
     *               4.D0*DT*DT*DT*LCOEFF(5,ISRC)
                  ACURVE(1) = LCOEFF(3,ISRC) + 3.D0*DT*LCOEFF(4,ISRC) +
     *               6.D0*DT*DT*LCOEFF(5,ISRC)
                  ACURVE(2) = LCOEFF(4,ISRC) + 4.D0*DT*LCOEFF(5,ISRC)
                  ACURVE(3) = LCOEFF(5,ISRC)
                  WRITE (MSGTXT,1020) LNOSOU(1,ISRC), XSPEC, ACURVE(1),
     *               ACURVE(2), ACURVE(3)
                  CALL MSGWRT (3)
C                                       Perley-Butler 2017
               ELSE IF (ICTYPE.EQ.0) THEN
                  XSPEC = PBOEFF(2,ISRC)
                  ACURVE(1) = PBOEFF(3,ISRC)
                  ACURVE(2) = PBOEFF(3,ISRC)
                  ACURVE(3) = PBOEFF(3,ISRC)
                  ACURVE(4) = PBOEFF(3,ISRC)
                  WRITE (MSGTXT,1020) KNOSOU(1,ISRC), XSPEC, ACURVE
                  CALL MSGWRT (3)
C                                       stable ones
               ELSE IF (ISRC.GT.XSOUR) THEN
                  ISRC = ISRC - XSOUR
                  XSPEC = SCOEFF(2,ISRC)
                  ACURVE(1) = SCOEFF(3,ISRC)
                  ACURVE(2) = SCOEFF(4,ISRC)
                  WRITE (MSGTXT,1020) SNOSOU(1,ISRC), XSPEC, ACURVE(1),
     *               ACURVE(2)
                  CALL MSGWRT (3)
C                                       3C286, 3C295 stable
               ELSE IF ((ISRC.EQ.1) .OR. (ISRC.EQ.5)) THEN
                  XSPEC = TCOEFF(2,ISRC)
                  ACURVE(1) = TCOEFF(3,ISRC)
                  ACURVE(2) = TCOEFF(4,ISRC)
                  WRITE (MSGTXT,1020) KNOSOU(1,ISRC), XSPEC, ACURVE(1),
     *               ACURVE(2)
                  CALL MSGWRT (3)
               ELSE IF (ISRC.GT.0) THEN
                  LSRC = ISRC - 1
                  IF ((DD.LE.DATES(1)) .OR. (DD.GE.DATES(NDATES))) THEN
                     I = NDATES
                     IF (DD.LE.DATES(1)) I = 1
                     XSPEC = DCOEFF(2,I,LSRC)
                     ACURVE(1) = DCOEFF(3,I,LSRC)
                     ACURVE(2) = DCOEFF(4,I,LSRC)
                     WRITE (MSGTXT,1020) KNOSOU(1,ISRC), XSPEC,
     *                  ACURVE(1), ACURVE(2)
                     CALL MSGWRT (3)
C                                       interpolate
                  ELSE
                     DO 50 I = 2,NDATES
                        IF (DD.LT.DATES(I)) THEN
                           W1 = (DATES(I) - DD) / (DATES(I)-DATES(I-1))
                           W2 = 1.0 - W1
                           XSPEC = W2 * DCOEFF(2,I,LSRC) +
     *                        W1 * DCOEFF(2,I-1,LSRC)
                           ACURVE(1) = W2 * DCOEFF(3,I,LSRC) +
     *                        W1 * DCOEFF(3,I-1,LSRC)
                           ACURVE(2) = W2 * DCOEFF(4,I,LSRC) +
     *                        W1 * DCOEFF(4,I-1,LSRC)
                           WRITE (MSGTXT,1020) KNOSOU(1,ISRC), XSPEC,
     *                        ACURVE(1), ACURVE(2)
                           CALL MSGWRT (3)
                           GO TO 999
                           END IF
 50                     CONTINUE
                     END IF
                  END IF
               END IF
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (A5,' default spectral index',5F7.3)
      END
      SUBROUTINE DFIT (N, DX, DF, DG)
C---------------------------------------------------------------------
C   Compute objective function and gradient for (Re,Im) fit.
C   Inputs:
C      N       I      Dimension of parameter array DX.
C      DX      D(*)   Parameter array.
C   Outputs:
C      DF      D      Objective function.
C      DG      D(*)   Gradient of objective function.
C---------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION DX(N), DG(N), DF
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PPDAT.INC'
      CHARACTER LTYPE*8
      LOGICAL WF
      COMPLEX BPA, BPB, BPC
      DOUBLE PRECISION DCHISQ, DJR, DJI, DIR, DII, DA, DB,
     *   DXKI(MXTERM), DXKJ(MXTERM), DRETMP, DIMTMP, DSHIFT
      REAL C(MXPARM)
      REAL XVALI, XVALJ, XREA, XIMA, XREB, XIMB, A, B
      INTEGER I, J, INDX1, INDX2, JNDX1, JNDX2, IANT, JANT, JNDX,
     *   INDX, M, NDAT, INCA, INCB, IRET, KR, NMAX
      INCLUDE 'BPDAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LTYPE /'CHEB_RI '/, WF /.FALSE./
C---------------------------------------------------------------------
C                                       Convert coefficients to real
C                                       and zero gradient array.
      DO 50 I = 1, N
         C(I) = DX(I)
         DG(I) = 0.0D0
 50      CONTINUE
      DF = 0.0D0
C                                       Initialization.
      DCHISQ = 0.0D0
C                                       Polynomial range.
      A = 1.0
      B = FLOAT (NBPCHN)
      DA = 1.0D0
      DB = DBLE (NBPCHN)
      NDAT = 0
C                                       Loop over all baselines.
      DO 500 I = 1, NBASL
         IANT = IBASL(I)
         JANT = JBASL(I)
         INDX1 = ISTRE(IANT)
         INDX2 = ISTIM(IANT)
         JNDX1 = ISTRE(JANT)
         JNDX2 = ISTIM(JANT)
         DO 450 J = 1, NBPCHN
C                                       Pointer to XREDAT, XIMDAT
            JNDX = (I - 1) * NBPCHN + J
            IF (XWGTBP(JNDX).LE.0.0) GO TO 450
C
            XVALI = FLOAT (J) - SHIFTA(I)
            IF ((XVALI.LT.A) .OR. (XVALI.GT.B)) GO TO 450
C                                       Compute BP in channel j for
C                                       first antenna.
            INCA = MIN (1, INDX1)
            INCB = MIN (1, INDX2)
            XIMA = 0.0
            DSHIFT = DBLE (SHIFTA(I))
C                                       Amplitude being fitted.
            IF (INDX1.GT.0) THEN
               CALL BPCOEF (LTYPE, C(INDX1), C(INDX2), INCA, INCB,
     *            NTERMS, FBLANK, DSHIFT, XREA, XIMA, 1, 1, J, 1,
     *            A, B, 1, WF, DXKI, MXTERM, NMAX, WF, IRET)
C                                       Use initial value data for
C                                       for amplitude.
            ELSE
               KR = (IANT - 1) * 2 * NTERMS + 1
               CALL BPCOEF (LTYPE, BPZERO(KR), C(INDX2), 1, INCB,
     *            NTERMS, FBLANK, DSHIFT, XREA, XIMA, 1, 1, J, 1,
     *            A, B, 1, WF, DXKI, MXTERM, NMAX, WF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 450
            BPA = CMPLX (XREA, XIMA)
C
            XVALJ = FLOAT (J) - SHIFTB(I)
            IF ((XVALJ.LT.A) .OR. (XVALJ.GT.B)) GO TO 450
C                                       Compute BP in channel j for
C                                       second antenna.
            INCA = MIN (1, JNDX1)
            INCB = MIN (1, JNDX2)
            XIMB = 0.0
            DSHIFT = DBLE (SHIFTB(I))
C                                       Amplitude being fitted.
            IF (JNDX1.GT.0) THEN
               CALL BPCOEF (LTYPE, C(JNDX1), C(JNDX2), INCA, INCB,
     *            NTERMS, FBLANK, DSHIFT, XREB, XIMB, 1, 1, J, 1,
     *            A, B, 1, WF, DXKJ, MXTERM, NMAX, WF, IRET)
C                                       Use initial value data for
C                                       for amplitude.
            ELSE
               KR = (JANT - 1) * 2 * NTERMS + 1
               CALL BPCOEF (LTYPE, BPZERO(KR), C(JNDX2), 1, INCB,
     *            NTERMS, FBLANK, DSHIFT, XREB, XIMB, 1, 1, J, 1,
     *            A, B, 1, WF, DXKJ, MXTERM, NMAX, WF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 450
            BPB = CMPLX (XREB, XIMB)
C                                       Form complex product
            BPC = BPA * CONJG (BPB)
            DRETMP = (XREDAT(JNDX) - REAL (BPC))
            DIMTMP = (XIMDAT(JNDX) - AIMAG (BPC))
            DCHISQ = DCHISQ + (DRETMP**2 + DIMTMP**2) * XWGTBP(JNDX)
            NDAT = NDAT + 1
C                                          Accumulate gradient
            DO 300 M = 1, NTERMS
               DJR = XREB * DXKI(M)
               DJI = XIMB * DXKI(M)
               DIR = XREA * DXKJ(M)
               DII = XIMA * DXKJ(M)
C                                          Antenna 1
C                                          Real part
               INDX = ISTRE(IANT) + M - 1
               DG(INDX) = DG(INDX) - 2.0D0 * (DRETMP * DJR -
     *            DIMTMP * DJI) * XWGTBP(JNDX)
C                                          Imaginary part
               INDX = ISTIM(IANT) + M - 1
               IF (ISTIM(IANT).GT.0) THEN
                  DG(INDX) = DG(INDX) - 2.0D0 * (DRETMP * DJI +
     *               DIMTMP * DJR) * XWGTBP(JNDX)
                  END IF
C                                          Antenna 2
C                                          Real part
               INDX = ISTRE(JANT) + M - 1
               DG(INDX) = DG(INDX) - 2.0D0 * (DRETMP * DIR +
     *            DIMTMP * DII) * XWGTBP(JNDX)
C                                          Imaginary part
               INDX = ISTIM(JANT) + M - 1
               IF (ISTIM(JANT).GT.0) THEN
                  DG(INDX) = DG(INDX) - 2.0D0 * (DRETMP * DII -
     *               DIMTMP * DIR) * XWGTBP(JNDX)
                  END IF
 300           CONTINUE
 450        CONTINUE
 500     CONTINUE
C
      DF = DCHISQ
      RETURN
      END
      SUBROUTINE DFIT2 (N, DX, DF, DG)
C---------------------------------------------------------------------
C   Compute objective function and gradient for (Amp,Phase) fit.
C   Inputs:
C      N       I      Dimension of parameter array DX.
C      DX      D(*)   Parameter array.
C   Outputs:
C      DF      D      Objective function.
C      DG      D(*)   Gradient of objective function.
C---------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION DX(N), DG(N), DF
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PPDAT.INC'
      CHARACTER LTYPE*8
      LOGICAL WF
      DOUBLE PRECISION DCHISQ, DA, DB, DXKI(MXTERM),
     *   DXKJ(MXTERM), DRETMP, DIMTMP, DXA, DXB, DCOSB,
     *   DSINB, DPLUS, DMINUS, DSHIFT
      REAL C(MXPARM)
      REAL XVALI, XVALJ, A, B,
     *   XAMPA, XPHSA, XAMPB, XPHSB
      INTEGER I, J, INDX1, INDX2, JNDX1, JNDX2, IANT, JANT, JNDX,
     *   INDX, M, KR, INCA, INCB, NMAX, IRET
      INCLUDE 'BPDAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LTYPE /'CHEB_AP '/, WF /.FALSE./
C---------------------------------------------------------------------
C                                       Convert coefficients to real.
C                                       Zero gradient array.
      DO 50 I = 1, N
         C(I) = DX(I)
         DG(I) = 0.0D0
 50      CONTINUE
      DF = 0.0D0
C                                       Initialization.
      DCHISQ = 0.0D0
C                                       Polynomial range.
      A = 1.0
      B = FLOAT (NBPCHN)
      DA = 1.0D0
      DB = DBLE (NBPCHN)
C                                       Loop over all baselines.
      DO 500 I = 1, NBASL
         IANT = IBASL(I)
         JANT = JBASL(I)
         INDX1 = ISTRE(IANT)
         INDX2 = ISTIM(IANT)
         JNDX1 = ISTRE(JANT)
         JNDX2 = ISTIM(JANT)
         DO 450 J = 1, NBPCHN
C                                       Position in XREDAT,XIMDAT
            JNDX = (I - 1) * NBPCHN + J
C                                       Valid data ?
            IF (XWGTBP(JNDX).LE.0.0) GO TO 450
C                                       X-ordinate
            XVALI = FLOAT (J) - SHIFTA(I)
            IF ((XVALI.LT.A) .OR. (XVALI.GT.B)) GO TO 450
C                                       Compute BP in channel j for
C                                       first antenna.
            INCA = MIN (1, INDX1)
            INCB = MIN (1, INDX2)
            XPHSA = 0.0
            DSHIFT = DBLE (SHIFTA(I))
            IF (INDX1.GT.0) THEN
C                                       Amplitude being fitted.
               CALL BPCOEF (LTYPE, C(INDX1), C(INDX2), INCA, INCB,
     *            NTERMS, FBLANK, DSHIFT, XAMPA, XPHSA, 1, 1, J, 1,
     *            A, B, 1, WF, DXKI, MXTERM, NMAX, WF, IRET)
            ELSE
C                                       Use initial value data for
C                                       for amplitude.
               KR = (IANT - 1) * 2 * NTERMS + 1
               CALL BPCOEF (LTYPE, BPZERO(KR), C(INDX2), 1, INCB,
     *            NTERMS, FBLANK, DSHIFT, XAMPA, XPHSA, 1, 1, J, 1,
     *            A, B, 1, WF, DXKI, MXTERM, NMAX, WF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 450
C
C                                       Compute BP in channel j for
C                                       second antenna.
            XVALJ = FLOAT (J) - SHIFTB(I)
            IF ((XVALJ.LT.A) .OR. (XVALJ.GT.B)) GO TO 450
            INCA = MIN (1, JNDX1)
            INCB = MIN (1, JNDX2)
            XPHSB = 0.0
            DSHIFT = DBLE (SHIFTB(I))
            IF (INDX1.GT.0) THEN
C                                       Amplitude being fitted.
               CALL BPCOEF (LTYPE, C(JNDX1), C(JNDX2), INCA, INCB,
     *            NTERMS, FBLANK, DSHIFT, XAMPB, XPHSB, 1, 1, J, 1,
     *            A, B, 1, WF, DXKJ, MXTERM, NMAX, WF, IRET)
            ELSE
C                                       Use initial value data for
C                                       for amplitude.
               KR = (JANT - 1) * 2 * NTERMS + 1
               CALL BPCOEF (LTYPE, BPZERO(KR), C(JNDX2), 1, INCB,
     *            NTERMS, FBLANK, DSHIFT, XAMPB, XPHSB, 1, 1, J, 1,
     *            A, B, 1, WF, DXKJ, MXTERM, NMAX, WF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 450
C                                       Form product
            DXA = XAMPA * XAMPB
            DXB = XPHSA - XPHSB
            DCOSB = COS (DXB)
            DSINB = SIN (DXB)
C
            DRETMP = (XREDAT(JNDX) - DXA * DCOSB)
            DIMTMP = (XIMDAT(JNDX) - DXA * DSINB)
            DCHISQ = DCHISQ + (DRETMP**2 + DIMTMP**2) * XWGTBP(JNDX)
C                                          Accumulate gradient
            DO 300 M = 1, NTERMS
C                                          Intermediate values
               DPLUS = -2.0D0 * (DCOSB * DRETMP + DSINB * DIMTMP)
               DMINUS = -2.0D0 * (-DSINB * DRETMP + DCOSB * DIMTMP)
C                                          Antenna 1
C                                          Real part
               INDX = ISTRE(IANT) + M - 1
               IF (ISTRE(IANT).GT.0) DG(INDX) = DG(INDX) +
     *            XAMPB * DXKI(M) * DPLUS * XWGTBP(JNDX)
C                                          Imaginary part
               INDX = ISTIM(IANT) + M - 1
               IF (ISTIM(IANT).GT.0) DG(INDX) = DG(INDX) + DXA * DXKI(M)
     *            * DMINUS  * XWGTBP(JNDX)
C                                          Antenna 2
C                                          Real part
               INDX = ISTRE(JANT) + M - 1
               IF (ISTRE(JANT).GT.0) DG(INDX) = DG(INDX) +
     *            XAMPA * DXKJ(M) * DPLUS * XWGTBP(JNDX)
C                                          Imaginary part
               INDX = ISTIM(JANT) + M - 1
               IF (ISTIM(JANT).GT.0) DG(INDX) = DG(INDX) - DXA * DXKJ(M)
     *            * DMINUS * XWGTBP(JNDX)
 300           CONTINUE
 450        CONTINUE
 500     CONTINUE
C
      DF = DCHISQ
      RETURN
      END
      SUBROUTINE DFIT3 (N, DX, DF, DG)
C---------------------------------------------------------------------
C   Compute objective function and gradient for phase-only fit.
C   Inputs:
C      N       I      Dimension of parameter array DX.
C      DX      D(*)   Parameter array.
C   Outputs:
C      DF      D      Objective function.
C      DG      D(*)   Gradient of objective function.
C---------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION DX(N), DG(N), DF
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PPDAT.INC'
      CHARACTER LTYPE*8
      LOGICAL WF
      DOUBLE PRECISION DCHISQ, DA, DB, DXKI(MXTERM),
     *   DXKJ(MXTERM), DRETMP, DXA, DXB, DPLUS, DSHIFT
      REAL C(MXPARM)
      REAL XVALI, XVALJ, A, B, XPHSA, XAMPB, XPHSB
      INTEGER I, J, INDX1, INDX2, JNDX1, JNDX2, IANT, JANT, JNDX,
     *   INDX, M, INCB, NMAX, IRET
      INCLUDE 'BPDAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LTYPE /'CHEB_AP '/, WF /.FALSE./
C---------------------------------------------------------------------
C                                       Convert coefficients to real and
C                                       zero gradient array.
      DO 50 I = 1, N
         C(I) = DX(I)
         DG(I) = 0.0D0
 50      CONTINUE
      DF = 0.0D0
C                                       Initialization.
      DCHISQ = 0.0D0
C                                       Polynomial range.
      A = 1.0
      B = FLOAT (NBPCHN)
      DA = 1.0D0
      DB = DBLE (NBPCHN)
C                                       Loop over all baselines.
      DO 500 I = 1, NBASL
         IANT = IBASL(I)
         JANT = JBASL(I)
         INDX1 = ISTRE(IANT)
         INDX2 = ISTIM(IANT)
         JNDX1 = ISTRE(JANT)
         JNDX2 = ISTIM(JANT)
         DO 450 J = 1, NBPCHN
C                                       Pointer to XREDAT, XIMDAT
            JNDX = (I - 1) * NBPCHN + J
            IF (XWGTBP(JNDX).LE.0.0) GO TO 450
C                                       Ignore amplitude
            XVALI = FLOAT (J) - SHIFTA(I)
            IF ((XVALI.LT.A) .OR. (XVALI.GT.B)) GO TO 450
C                                       Fit phase
            XPHSA = 0.0
            INCB = MIN (1, INDX2)
            DSHIFT = DBLE (SHIFTA(I))
            IF (INDX2.GT.0) THEN
C                                       Expand phase only.
               CALL BPCOEF (LTYPE, C(INDX1), C(INDX2), -1, INCB,
     *            NTERMS, FBLANK, DSHIFT, XAMPB, XPHSA, 1, 1, J, 1,
     *            A, B, 1, WF, DXKI, MXTERM, NMAX, WF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 450
C
            XVALJ = FLOAT (J) - SHIFTB(I)
            IF ((XVALJ.LT.A) .OR. (XVALJ.GT.B)) GO TO 450
            XPHSB = 0.0
            INCB = MIN (1, JNDX2)
            DSHIFT = DBLE (SHIFTB(I))
            IF (JNDX2.GT.0) THEN
C                                       Expand phase only.
               CALL BPCOEF (LTYPE, C(JNDX1), C(JNDX2), -1, INCB,
     *            NTERMS, FBLANK, DSHIFT, XAMPB, XPHSB, 1, 1, J, 1,
     *            A, B, 1, WF, DXKJ, MXTERM, NMAX, WF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 450
C                                       Form product
            DXB = XPHSA - XPHSB
C
            DXA = ATAN2 (XIMDAT(JNDX), XREDAT(JNDX))
            DRETMP = DXA - DXB
            DCHISQ = DCHISQ + (DRETMP**2) * XWGTBP(JNDX)
C                                          Accumulate gradient
            DPLUS = 2.0D0 * DRETMP
C
            DO 300 M = 1, NTERMS
C                                          Antenna 1
C                                          Real part - ignore
C                                          Imaginary part
               INDX = ISTIM(IANT) + M - 1
               IF (ISTIM(IANT).GT.0) DG(INDX) = DG(INDX) - DPLUS *
     *            DXKI(M) * XWGTBP(JNDX)
C                                          Antenna 2
C                                          Real part - ignore
C                                          Imaginary part
               INDX = ISTIM(JANT) + M - 1
               IF (ISTIM(JANT).GT.0) DG(INDX) = DG(INDX) + DPLUS *
     *            DXKJ(M) * XWGTBP(JNDX)
 300           CONTINUE
 450        CONTINUE
 500     CONTINUE
C
      DF = DCHISQ
      RETURN
      END
C
C---------------------------------------------------------------------
C All software after this point taken from netlib libraries opt/tn.f
C and blas. Some write statements converted to AIPS-compatible form,
C others deleted. Routine TNAK is a minor variant of the original TN
C with added input parameters. Missing variable declarations added,
C implicit typing removed. ZTIME rename to ZZTIME; and DCOPY renamed
C to BLASCP.
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C%% TRUNCATED-NEWTON METHOD:  SUBROUTINES
C   FOR OTHER MACHINES, MODIFY ROUTINE MCHPR1 (MACHINE EPSILON)
C   WRITTEN BY:  STEPHEN G. NASH
C                OPERATIONS RESEARCH AND APPLIED STATISTICS DEPT.
C                GEORGE MASON UNIVERSITY
C                FAIRFAX, VA 22030
C******************************************************************
      SUBROUTINE TNAK (IERROR, N, X, F, G, W, LW, SFUN, NITER,
     *   DTOL, IPRTLV)
      INTEGER           IERROR, N, LW
      DOUBLE PRECISION  X(N), G(N), F, W(LW)
C
      DOUBLE PRECISION DTOL
      INTEGER NITER, IPRTLV
C
C THIS ROUTINE SOLVES THE OPTIMIZATION PROBLEM
C
C            MINIMIZE F(X)
C               X
C
C WHERE X IS A VECTOR OF N REAL VARIABLES.  THE METHOD USED IS
C A TRUNCATED-NEWTON ALGORITHM (SEE "NEWTON-TYPE MINIMIZATION VIA
C THE LANCZOS METHOD" BY S.G. NASH (SIAM J. NUMER. ANAL. 21 (1984),
C PP. 770-778).  THIS ALGORITHM FINDS A LOCAL MINIMUM OF F(X).  IT DOES
C NOT ASSUME THAT THE FUNCTION F IS CONVEX (AND SO CANNOT GUARANTEE A
C GLOBAL SOLUTION), BUT DOES ASSUME THAT THE FUNCTION IS BOUNDED BELOW.
C IT CAN SOLVE PROBLEMS HAVING ANY NUMBER OF VARIABLES, BUT IT IS
C ESPECIALLY USEFUL WHEN THE NUMBER OF VARIABLES (N) IS LARGE.
C
C SUBROUTINE PARAMETERS:
C
C IERROR - (INTEGER) ERROR CODE
C          (0 => NORMAL RETURN)
C          (2 => MORE THAN MAXFUN EVALUATIONS)
C          (3 => LINE SEARCH FAILED TO FIND
C          (         LOWER POINT (MAY NOT BE SERIOUS)C
C          (-1 => ERROR IN INPUT PARAMETERS)
C N      - (INTEGER) NUMBER OF VARIABLES
C X      - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON INPUT, AN INITIAL
C          ESTIMATE OF THE SOLUTION; ON OUTPUT, THE COMPUTED SOLUTION.
C G      - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON OUTPUT, THE FINAL
C          VALUE OF THE GRADIENT
C F      - (REAL*8) ON INPUT, A ROUGH ESTIMATE OF THE VALUE OF THE
C          OBJECTIVE FUNCTION AT THE SOLUTION; ON OUTPUT, THE VALUE
C          OF THE OBJECTIVE FUNCTION AT THE SOLUTION
C W      - (REAL*8) WORK VECTOR OF LENGTH AT LEAST 14*N
C LW     - (INTEGER) THE DECLARED DIMENSION OF W
C SFUN   - A USER-SPECIFIED SUBROUTINE THAT COMPUTES THE FUNCTION
C          AND GRADIENT OF THE OBJECTIVE FUNCTION.  IT MUST HAVE
C          THE CALLING SEQUENCE
C             SUBROUTINE SFUN (N, X, F, G)
C             INTEGER           N
C             DOUBLE PRECISION  X(N), G(N), F
C
C THIS IS AN EASY-TO-USE DRIVER FOR THE MAIN OPTIMIZATION ROUTINE
C LMQN.  MORE EXPERIENCED USERS WHO WISH TO CUSTOMIZE PERFORMANCE
C OF THIS ALGORITHM SHOULD CALL LMQN DIRECTLY.
C
C----------------------------------------------------------------------
C THIS ROUTINE SETS UP ALL THE PARAMETERS FOR THE TRUNCATED-NEWTON
C ALGORITHM.  THE PARAMETERS ARE:
C
C ETA    - SEVERITY OF THE LINESEARCH
C MAXFUN - MAXIMUM ALLOWABLE NUMBER OF FUNCTION EVALUATIONS
C XTOL   - DESIRED ACCURACY FOR THE SOLUTION X*
C STEPMX - MAXIMUM ALLOWABLE STEP IN THE LINESEARCH
C ACCRCY - ACCURACY OF COMPUTED FUNCTION VALUES
C MSGLVL - DETERMINES QUANTITY OF PRINTED OUTPUT
C          0 = NONE, 1 = ONE LINE PER MAJOR ITERATION.
C MAXIT  - MAXIMUM NUMBER OF INNER ITERATIONS PER STEP
C
      DOUBLE PRECISION ETA, ACCRCY, XTOL, STEPMX, MCHPR1
      INTEGER MAXIT, MSGLVL, MAXFUN
      EXTERNAL         SFUN
C
C SET UP PARAMETERS FOR THE OPTIMIZATION ROUTINE
C
      MAXIT = N/2
      IF (MAXIT .GT. 50) MAXIT = 50
      IF (MAXIT .LE. 0) MAXIT = 1
      MSGLVL = IPRTLV
      MAXFUN = NITER
      ETA = .25D0
      STEPMX = 1.D1
      ACCRCY = 1.D2*MCHPR1()
      XTOL = DTOL
C
C MINIMIZE THE FUNCTION
C
      CALL LMQN (IERROR, N, X, F, G, W, LW, SFUN,
     *     MSGLVL, MAXIT, MAXFUN, ETA, STEPMX, ACCRCY, XTOL)
C
      RETURN
      END
C
      SUBROUTINE LMQN (IFAIL, N, X, F, G, W, LW, SFUN,
     *            MSGLVL, MAXIT, MAXFUN, ETA, STEPMX, ACCRCY, XTOL)
      INTEGER           MSGLVL, N, MAXFUN, IFAIL, LW
      DOUBLE PRECISION  X(N), G(N), W(LW), ETA, XTOL, STEPMX, F, ACCRCY
C
C THIS ROUTINE IS A TRUNCATED-NEWTON METHOD.
C THE TRUNCATED-NEWTON METHOD IS PRECONDITIONED BY A LIMITED-MEMORY
C QUASI-NEWTON METHOD (THIS PRECONDITIONING STRATEGY IS DEVELOPED
C IN THIS ROUTINE) WITH A FURTHER DIAGONAL SCALING (SEE ROUTINE NDIA3).
C FOR FURTHER DETAILS ON THE PARAMETERS, SEE ROUTINE TN.
C
      INTEGER I, ICYCLE, IOLDG, IPK, IYK, LOLDG, LPK, LSR,
     *     LWTEST, LYK, LYR, NFTOTL, NITER, NM1, NUMF, NWHY,
     *     NMODIF, NLINCG, IRESET, NFEVAL, LHYR, IPIVOT(2500), ISK,
     *     IDIAGB, MODET, LEMAT, LZ1, LZK, MAXIT, LGV, LV, LHG,
     *     LHYK, LSK, LDIAGB
      DOUBLE PRECISION ABSTOL, ALPHA, DIFNEW, DIFOLD, EPSMCH,
     *     EPSRED, FKEEP, FM, FNEW, FOLD, FSTOP, FTEST, GNORM, GSK,
     *     GTG, GTPNEW, OLDF, OLDGTP, ONE, PE, PEPS, PNORM, RELTOL,
     *     RTEPS, RTLEPS, RTOL, RTOLSQ, SMALL, SPE, TINY,
     *     TNYTOL, TOLEPS, XNORM, YKSK, YRSR, ZERO
      LOGICAL LRESET, UPD1
      INCLUDE 'INCS:DMSG.INC'
C
C THE FOLLOWING IMSL AND STANDARD FUNCTIONS ARE USED
C
      DOUBLE PRECISION DABS, DDOT, DSQRT, STEP1, DNRM2
      EXTERNAL SFUN
      COMMON /SUBSCR/ LGV,LZ1,LZK,LV,LSK,LYK,LDIAGB,LSR,LYR,
     *     LOLDG,LHG,LHYK,LPK,LEMAT,LWTEST
C
C INITIALIZE PARAMETERS AND CONSTANTS
C
      IF (MSGLVL .GE. 1) THEN
         WRITE(MSGTXT,800)
         CALL MSGWRT (3)
         END IF
C
      CALL SETPAR(N)
      UPD1 = .TRUE.
      IRESET = 0
      NFEVAL = 0
      NMODIF = 0
      NLINCG = 0
      FSTOP = F
      ZERO = 0.D0
      ONE = 1.D0
      NM1 = N - 1
C
C WITHIN THIS ROUTINE THE ARRAY W(LOLDG) IS SHARED BY W(LHYR)
C
      LHYR = LOLDG
C
C CHECK PARAMETERS AND SET CONSTANTS
C
      CALL CHKUCP (LWTEST,MAXFUN,NWHY,N,ALPHA,EPSMCH,
     *     ETA,PEPS,RTEPS,RTOL,RTOLSQ,STEPMX,FTEST,
     *     XTOL,XNORM,X,LW,SMALL,TINY,ACCRCY)
      IF (NWHY.LT.0) GO TO 120
      CALL SETUCR (NFTOTL,NITER,N,F,FNEW,
     *     FM,GTG,OLDF,SFUN,G,X)
      FOLD = FNEW
      IF (MSGLVL .GE. 1) THEN
         WRITE(MSGTXT,810) NITER,NFTOTL,FNEW,GTG
         CALL MSGWRT (3)
         END IF
C
C CHECK FOR SMALL GRADIENT AT THE STARTING POINT.
C
      FTEST = ONE + DABS(FNEW)
      IF (GTG .LT. 1.D-4*EPSMCH*FTEST*FTEST) GO TO 90
C
C SET INITIAL VALUES TO OTHER PARAMETERS
C
      ICYCLE = NM1
      TOLEPS = RTOL + RTEPS
      RTLEPS = RTOLSQ + EPSMCH
      GNORM  = DSQRT(GTG)
      DIFNEW = ZERO
      EPSRED = 5.0D-2
      FKEEP  = FNEW
C
C SET THE DIAGONAL OF THE APPROXIMATE HESSIAN TO UNITY.
C
      IDIAGB = LDIAGB
      DO 10 I = 1,N
         W(IDIAGB) = ONE
         IDIAGB = IDIAGB + 1
10    CONTINUE
C
C ..................START OF MAIN ITERATIVE LOOP..........
C
C COMPUTE THE NEW SEARCH DIRECTION
C
      MODET = MSGLVL - 3
      CALL MODLNP(MODET,W(LPK),W(LGV),W(LZ1),W(LV),
     *     W(LDIAGB),W(LEMAT),X,G,W(LZK),
     *     N,W,LW,MAXIT,NFEVAL,
     *     NLINCG,UPD1,YKSK,GSK,YRSR,LRESET,SFUN,.FALSE.,IPIVOT,
     *     ACCRCY,GTPNEW,GNORM,XNORM)
 20   CONTINUE
      CALL BLASCP(N,G,1,W(LOLDG),1)
      PNORM = DNRM2(N,W(LPK),1)
      OLDF = FNEW
      OLDGTP = GTPNEW
C
C PREPARE TO COMPUTE THE STEP LENGTH
C
      PE = PNORM + EPSMCH
C
C COMPUTE THE ABSOLUTE AND RELATIVE TOLERANCES FOR THE LINEAR SEARCH
C
      RELTOL = RTEPS*(XNORM + ONE)/PE
      ABSTOL = - EPSMCH*FTEST/(OLDGTP - EPSMCH)
C
C COMPUTE THE SMALLEST ALLOWABLE SPACING BETWEEN POINTS IN
C THE LINEAR SEARCH
C
      TNYTOL = EPSMCH*(XNORM + ONE)/PE
      SPE = STEPMX/PE
C
C SET THE INITIAL STEP LENGTH.
C
      ALPHA = STEP1(FNEW,FM,OLDGTP,SPE)
C
C PERFORM THE LINEAR SEARCH
C
      CALL LINDER(N,SFUN,SMALL,EPSMCH,RELTOL,ABSTOL,TNYTOL,
     *     ETA,SPE,W(LPK),OLDGTP,X,FNEW,ALPHA,G,NUMF,
     *     NWHY,W,LW)
C
      FOLD = FNEW
      NITER = NITER + 1
      NFTOTL = NFTOTL + NUMF
      GTG = DDOT(N,G,1,G,1)
      IF (MSGLVL .GE. 1) THEN
         WRITE(MSGTXT,810) NITER,NFTOTL,FNEW,GTG
         CALL MSGWRT (3)
         END IF
      IF (NWHY .LT. 0) GO TO 120
      IF (NWHY .EQ. 0 .OR. NWHY .EQ. 2) GO TO 30
C
C THE LINEAR SEARCH HAS FAILED TO FIND A LOWER POINT
C
      NWHY = 3
      GO TO 100
30    IF (NWHY .LE. 1) GO TO 40
      CALL SFUN(N,X,FNEW,G)
      NFTOTL = NFTOTL + 1
C
C TERMINATE IF MORE THAN MAXFUN EVALUTATIONS HAVE BEEN MADE
C
40    NWHY = 2
      IF (NFTOTL .GT. MAXFUN) GO TO 110
      NWHY = 0
C
C SET UP PARAMETERS USED IN CONVERGENCE AND RESETTING TESTS
C
      DIFOLD = DIFNEW
      DIFNEW = OLDF - FNEW
C
C IF THIS IS THE FIRST ITERATION OF A NEW CYCLE, COMPUTE THE
C PERCENTAGE REDUCTION FACTOR FOR THE RESETTING TEST.
C
      IF (ICYCLE .NE. 1) GO TO 50
      IF (DIFNEW .GT. 2.0D0 *DIFOLD) EPSRED = EPSRED + EPSRED
      IF (DIFNEW .LT. 5.0D-1*DIFOLD) EPSRED = 5.0D-1*EPSRED
50    CONTINUE
      GNORM = DSQRT(GTG)
      FTEST = ONE + DABS(FNEW)
      XNORM = DNRM2(N,X,1)
C
C TEST FOR CONVERGENCE
C
      IF ((ALPHA*PNORM .LT. TOLEPS*(ONE + XNORM)
     *     .AND. DABS(DIFNEW) .LT. RTLEPS*FTEST
     *     .AND. GTG .LT. PEPS*FTEST*FTEST)
     *     .OR. GTG .LT. 1.D-4*ACCRCY*FTEST*FTEST) GO TO 90
C
C COMPUTE THE CHANGE IN THE ITERATES AND THE CORRESPONDING CHANGE
C IN THE GRADIENTS
C
      ISK = LSK
      IPK = LPK
      IYK = LYK
      IOLDG = LOLDG
      DO 60 I = 1,N
         W(IYK) = G(I) - W(IOLDG)
         W(ISK) = ALPHA*W(IPK)
         IPK = IPK + 1
         ISK = ISK + 1
         IYK = IYK + 1
         IOLDG = IOLDG + 1
 60      CONTINUE
C
C SET UP PARAMETERS USED IN UPDATING THE DIRECTION OF SEARCH.
C
      YKSK = DDOT(N,W(LYK),1,W(LSK),1)
      LRESET = .FALSE.
      IF (ICYCLE .EQ. NM1 .OR. DIFNEW .LT.
     *     EPSRED*(FKEEP-FNEW)) LRESET = .TRUE.
      IF (LRESET) GO TO 70
      YRSR = DDOT(N,W(LYR),1,W(LSR),1)
      IF (YRSR .LE. ZERO) LRESET = .TRUE.
70    CONTINUE
      UPD1 = .FALSE.
C
C      COMPUTE THE NEW SEARCH DIRECTION
C
      MODET = MSGLVL - 3
      CALL MODLNP(MODET,W(LPK),W(LGV),W(LZ1),W(LV),
     *     W(LDIAGB),W(LEMAT),X,G,W(LZK),
     *     N,W,LW,MAXIT,NFEVAL,
     *     NLINCG,UPD1,YKSK,GSK,YRSR,LRESET,SFUN,.FALSE.,IPIVOT,
     *     ACCRCY,GTPNEW,GNORM,XNORM)
      IF (LRESET) GO TO 80
C
C      STORE THE ACCUMULATED CHANGE IN THE POINT AND GRADIENT AS AN
C      "AVERAGE" DIRECTION FOR PRECONDITIONING.
C
      CALL DXPY(N,W(LSK),1,W(LSR),1)
      CALL DXPY(N,W(LYK),1,W(LYR),1)
      ICYCLE = ICYCLE + 1
      GOTO 20
C
C RESET
C
80    IRESET = IRESET + 1
C
C INITIALIZE THE SUM OF ALL THE CHANGES IN X.
C
      CALL BLASCP(N,W(LSK),1,W(LSR),1)
      CALL BLASCP(N,W(LYK),1,W(LYR),1)
      FKEEP = FNEW
      ICYCLE = 1
      GO TO 20
C
C ...............END OF MAIN ITERATION.......................
C
90    IFAIL = 0
      F = FNEW
      RETURN
100   OLDF = FNEW
C
C LOCAL SEARCH HERE COULD BE INSTALLED HERE
C
110    F = OLDF
C
C SET IFAIL
C
120   IFAIL = NWHY
      RETURN
800   FORMAT(' NIT   NF', 9X, 'F', 21X, 'GTG')
810   FORMAT(' ',I3,1X,I4,1X,1PD22.15,2X,1PD15.8)
      END
C
C
      SUBROUTINE ZZTIME(N,X,IPIVOT)
      INTEGER          N
      DOUBLE PRECISION X(N)
      INTEGER          IPIVOT(N)
C
      INTEGER I
C
C THIS ROUTINE MULTIPLIES THE VECTOR X BY THE CONSTRAINT MATRIX Z
C
      DO 10 I = 1,N
         IF (IPIVOT(I) .NE. 0) X(I) = 0.D0
10    CONTINUE
      RETURN
      END
C
C
C
C
C THE VECTORS SK AND YK, ALTHOUGH NOT IN THE CALL,
C ARE USED (VIA THEIR POSITION IN W) BY THE ROUTINE MSOLVE.
C
      SUBROUTINE MODLNP(MODET,ZSOL,GV,R,V,DIAGB,EMAT,
     *     X,G,ZK,N,W,LW,MAXIT,NFEVAL,NLINCG,
     *     UPD1,YKSK,GSK,YRSR,LRESET,SFUN,BOUNDS,IPIVOT,ACCRCY,
     *     GTP,GNORM,XNORM)
C-----------------------------------------------------------------------
      INTEGER MODET, N, IPIVOT(*), LW, MAXIT, NFEVAL, NLINCG
      DOUBLE PRECISION ZSOL(N),G(N),GV(N),R(N),V(N),DIAGB(N),W(LW)
      DOUBLE PRECISION EMAT(N),ZK(N),X(N),ACCRCY
      DOUBLE PRECISION ALPHA,BETA,DELTA,GSK,GTP,PR,
     *     QOLD,QNEW,QTEST,RHSNRM,RZ,RZOLD,TOL,VGV,YKSK,YRSR
      DOUBLE PRECISION GNORM,XNORM
      DOUBLE PRECISION DDOT
      LOGICAL FIRST,UPD1,LRESET,BOUNDS
      EXTERNAL SFUN
C
C THIS ROUTINE PERFORMS A PRECONDITIONED CONJUGATE-GRADIENT
C ITERATION IN ORDER TO SOLVE THE NEWTON EQUATIONS FOR A SEARCH
C DIRECTION FOR A TRUNCATED-NEWTON ALGORITHM.  WHEN THE VALUE OF THE
C QUADRATIC MODEL IS SUFFICIENTLY REDUCED,
C THE ITERATION IS TERMINATED.
C
C PARAMETERS
C
C MODET       - INTEGER WHICH CONTROLS AMOUNT OF OUTPUT
C ZSOL        - COMPUTED SEARCH DIRECTION
C G           - CURRENT GRADIENT
C GV,GZ1,V    - SCRATCH VECTORS
C R           - RESIDUAL
C DIAGB,EMAT  - DIAGONAL PRECONDITONING MATRIX
C FEVAL       - VALUE OF QUADRATIC FUNCTION
C
      INTEGER I, K
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C *************************************************************
C INITIALIZATION
C *************************************************************
C
C GENERAL INITIALIZATION
C
      IF (MAXIT .EQ. 0) RETURN
      FIRST = .TRUE.
      RHSNRM = GNORM
      TOL = 1.D-12
      QOLD = 0.D0
C
C INITIALIZATION FOR PRECONDITIONED CONJUGATE-GRADIENT ALGORITHM
C
      CALL INITPC (DIAGB, EMAT, N, W, LW, MODET, UPD1, YKSK, YRSR,
     *   LRESET)
      DO 10 I = 1,N
         R(I) = -G(I)
         V(I) = 0.D0
         ZSOL(I) = 0.D0
10       CONTINUE
C
C ************************************************************
C MAIN ITERATION
C ************************************************************
C
      DO 30 K = 1,MAXIT
         NLINCG = NLINCG + 1
C
C CG ITERATION TO SOLVE SYSTEM OF EQUATIONS
C
         IF (BOUNDS) CALL ZZTIME(N,R,IPIVOT)
         CALL MSOLVE(R,ZK,N,W,LW,UPD1,YKSK,GSK,
     *                 YRSR,LRESET,FIRST)
         IF (BOUNDS) CALL ZZTIME(N,ZK,IPIVOT)
         RZ = DDOT(N,R,1,ZK,1)
         IF (RZ/RHSNRM .LT. TOL) GO TO 80
         IF (K .EQ. 1) BETA = 0.D0
         IF (K .GT. 1) BETA = RZ/RZOLD
         DO 20 I = 1,N
            V(I) = ZK(I) + BETA*V(I)
20       CONTINUE
         IF (BOUNDS) CALL ZZTIME(N,V,IPIVOT)
         CALL GTIMS(V,GV,N,X,G,W,LW,SFUN,FIRST,DELTA,ACCRCY,XNORM)
         IF (BOUNDS) CALL ZZTIME(N,GV,IPIVOT)
         NFEVAL = NFEVAL + 1
         VGV = DDOT(N,V,1,GV,1)
         IF (VGV/RHSNRM .LT. TOL) GO TO 50
         CALL NDIA3(N,EMAT,V,GV,R,VGV)
C
C COMPUTE LINEAR STEP LENGTH
C
         ALPHA = RZ / VGV
C
C COMPUTE CURRENT SOLUTION AND RELATED VECTORS
C
         CALL DAXPY(N,ALPHA,V,1,ZSOL,1)
         CALL DAXPY(N,-ALPHA,GV,1,R,1)
C
C TEST FOR CONVERGENCE
C
         GTP = DDOT(N,ZSOL,1,G,1)
         PR = DDOT(N,R,1,ZSOL,1)
         QNEW = 5.D-1 * (GTP + PR)
         QTEST = K * (1.D0 - QOLD/QNEW)
         IF (QTEST .LT. 0.D0) GO TO 70
         QOLD = QNEW
         IF (QTEST .LE. 5.D-1) GO TO 70
C
C PERFORM CAUTIONARY TEST
C
         IF (GTP .GT. 0) GO TO 40
         RZOLD = RZ
30    CONTINUE
C
C TERMINATE ALGORITHM
C
      K = K-1
      GO TO 70
C
C TRUNCATE ALGORITHM IN CASE OF AN EMERGENCY
C
40    CONTINUE
      CALL DAXPY(N,-ALPHA,V,1,ZSOL,1)
      GTP = DDOT(N,ZSOL,1,G,1)
      GO TO 90
50    CONTINUE
      IF (K.LE.1) THEN
         CALL MSOLVE(G,ZSOL,N,W,LW,UPD1,YKSK,GSK,YRSR,LRESET,FIRST)
         CALL NEGVEC(N,ZSOL)
         IF (BOUNDS) CALL ZZTIME(N,ZSOL,IPIVOT)
         GTP = DDOT(N,ZSOL,1,G,1)
         END IF
70    CONTINUE
      GO TO 90
80    CONTINUE
      IF (K.LE.1) THEN
         CALL BLASCP(N,G,1,ZSOL,1)
         CALL NEGVEC(N,ZSOL)
         IF (BOUNDS) CALL ZZTIME(N,ZSOL,IPIVOT)
         GTP = DDOT(N,ZSOL,1,G,1)
         END IF
      GO TO 70
C
C STORE (OR RESTORE) DIAGONAL PRECONDITIONING
C
90    CONTINUE
      CALL BLASCP(N,EMAT,1,DIAGB,1)
      RETURN
      END
      SUBROUTINE NDIA3(N,E,V,GV,R,VGV)
C-----------------------------------------------------------------------
C UPDATE THE PRECONDITIOING MATRIX BASED ON A DIAGONAL VERSION
C OF THE BFGS QUASI-NEWTON UPDATE.
C-----------------------------------------------------------------------
      INTEGER    N
      DOUBLE PRECISION E(N), V(N), GV(N), R(N), VGV, VR, DDOT
C
      INTEGER I
C-----------------------------------------------------------------------
      VR = DDOT(N,V,1,R,1)
      DO 10 I = 1,N
         E(I) = E(I) - R(I)*R(I)/VR + GV(I)*GV(I)/VGV
         IF (E(I) .GT. 1.D-6) GO TO 10
         E(I) = 1.D0
10    CONTINUE
      RETURN
      END
C
C      SERVICE ROUTINES FOR OPTIMIZATION
C
      SUBROUTINE NEGVEC(N,V)
      INTEGER N
      DOUBLE PRECISION V(N)
C
C NEGATIVE OF THE VECTOR V
C
      INTEGER I
C
      DO 10 I = 1,N
         V(I) = -V(I)
10    CONTINUE
      RETURN
      END
      SUBROUTINE LSOUT (XMIN, XW, U, A, B, SCXBD)
C-----------------------------------------------------------------------
C ERROR PRINTOUTS FOR GETPTC
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMIN, XW, U, A, B, SCXBD
C
      DOUBLE PRECISION YA,YB,YBND,YW,YU
C-----------------------------------------------------------------------
      YU = XMIN + U
      YA = A + XMIN
      YB = B + XMIN
      YW = XW + XMIN
      YBND = SCXBD + XMIN
      RETURN
      END
      DOUBLE PRECISION FUNCTION STEP1(FNEW,FM,GTP,SMAX)
C-----------------------------------------------------------------------
C STEP1 RETURNS THE LENGTH OF THE INITIAL STEP TO BE TAKEN ALONG THE
C VECTOR P IN THE NEXT LINEAR SEARCH.
C-----------------------------------------------------------------------
      DOUBLE PRECISION FNEW,FM,GTP,SMAX
C
      DOUBLE PRECISION ALPHA,D,EPSMCH
      DOUBLE PRECISION DABS,MCHPR1
C-----------------------------------------------------------------------
      EPSMCH = MCHPR1()
      D = DABS(FNEW-FM)
      ALPHA = 1.D0
      IF (2.D0*D .LE. (-GTP) .AND. D .GE. EPSMCH)
     *     ALPHA = -2.D0*D/GTP
      IF (ALPHA .GE. SMAX) ALPHA = SMAX
      STEP1 = ALPHA
      RETURN
      END
C
C
      DOUBLE PRECISION FUNCTION MCHPR1()
C
C RETURNS THE VALUE OF EPSMCH, WHERE EPSMCH IS THE SMALLEST POSSIBLE
C REAL NUMBER SUCH THAT 1.0 + EPSMCH .GT. 1.0
C
C FOR VAX
C
      MCHPR1 = 1.D-17
C
C FOR SUN
C
C     MCHPR1 = 1.0842021724855D-19
      RETURN
      END
C
C
      SUBROUTINE CHKUCP(LWTEST,MAXFUN,NWHY,N,ALPHA,EPSMCH,
     *     ETA,PEPS,RTEPS,RTOL,RTOLSQ,STEPMX,TEST,
     *     XTOL,XNORM,X,LW,SMALL,TINY,ACCRCY)
      INTEGER LW,LWTEST,MAXFUN,NWHY,N
      DOUBLE PRECISION ACCRCY,ALPHA,EPSMCH,ETA,PEPS,RTEPS,RTOL,
     *     RTOLSQ,STEPMX,TEST,XTOL,XNORM,SMALL,TINY, DNRM2
      DOUBLE PRECISION X(N)
C
C CHECKS PARAMETERS AND SETS CONSTANTS WHICH ARE COMMON TO BOTH
C DERIVATIVE AND NON-DERIVATIVE ALGORITHMS
C
      DOUBLE PRECISION DABS,DSQRT,MCHPR1
      EPSMCH = MCHPR1()
      SMALL = EPSMCH*EPSMCH
      TINY = SMALL
      NWHY = -1
      RTEPS = DSQRT(EPSMCH)
      RTOL = XTOL
      IF (DABS(RTOL) .LT. ACCRCY) RTOL = 1.D1*RTEPS
C
C CHECK FOR ERRORS IN THE INPUT PARAMETERS
C
      IF (LW .LT. LWTEST
     *      .OR. N .LT. 1 .OR. RTOL .LT. 0.D0 .OR. ETA .GE. 1.D0 .OR.
     *      ETA .LT. 0.D0 .OR. STEPMX .LT. RTOL .OR.
     *      MAXFUN .LT. 1) RETURN
      NWHY = 0
C
C SET CONSTANTS FOR LATER
C
      RTOLSQ = RTOL*RTOL
      PEPS = ACCRCY**0.6666D0
      XNORM = DNRM2(N,X,1)
      ALPHA = 0.D0
      TEST = 0.D0
      RETURN
      END
      SUBROUTINE SETUCR (NFTOTL,NITER,N,F,FNEW,
     *            FM,GTG,OLDF,SFUN,G,X)
C-----------------------------------------------------------------------
      INTEGER          NFTOTL,NITER,N
      DOUBLE PRECISION F, FNEW, FM, GTG, OLDF, DDOT
      DOUBLE PRECISION G(N), X(N)
      EXTERNAL         SFUN
C-----------------------------------------------------------------------
C
C CHECK INPUT PARAMETERS, COMPUTE THE INITIAL FUNCTION VALUE, SET
C CONSTANTS FOR THE SUBSEQUENT MINIMIZATION
C
      FM = F
C
C COMPUTE THE INITIAL FUNCTION VALUE
C
      CALL SFUN(N,X,FNEW,G)
      NFTOTL = 1
C
C SET CONSTANTS FOR LATER
C
      NITER = 0
      OLDF = FNEW
      GTG = DDOT(N,G,1,G,1)
      RETURN
      END
C
C
      SUBROUTINE GTIMS(V,GV,N,X,G,W,LW,SFUN,FIRST,DELTA,ACCRCY,XNORM)
      INTEGER N,LW
      DOUBLE PRECISION V(N),GV(N),DINV,DELTA,G(N)
      DOUBLE PRECISION F,X(N),W(LW),ACCRCY,DSQRT,XNORM
      LOGICAL FIRST
      EXTERNAL SFUN
      INTEGER LGV, LZ1, LZK, LV, LSK, LYK, LDIAGB, LSR, LYR,
     *   LHYR, LHYK, LPK, LEMAT, LWTEST, LHG, IHG, I
      COMMON/SUBSCR/ LGV,LZ1,LZK,LV,LSK,LYK,LDIAGB,LSR,LYR,
     *     LHYR,LHG,LHYK,LPK,LEMAT,LWTEST
C
C THIS ROUTINE COMPUTES THE PRODUCT OF THE MATRIX G TIMES THE VECTOR
C V AND STORES THE RESULT IN THE VECTOR GV (FINITE-DIFFERENCE VERSION)
C
      IF (.NOT. FIRST) GO TO 20
      DELTA = DSQRT(ACCRCY)*(1.D0+XNORM)
      FIRST = .FALSE.
20    CONTINUE
      DINV = 1.D0/DELTA
      IHG = LHG
      DO 30 I = 1,N
         W(IHG) = X(I) + DELTA*V(I)
         IHG = IHG + 1
30    CONTINUE
      CALL SFUN(N,W(LHG),F,GV)
      DO 40 I = 1,N
         GV(I) = (GV(I) - G(I))*DINV
40    CONTINUE
      RETURN
      END
C
C
      SUBROUTINE MSOLVE(G,Y,N,W,LW,UPD1,YKSK,GSK,
     *     YRSR,LRESET,FIRST)
      INTEGER N, LW
      DOUBLE PRECISION G(N),Y(N),W(LW),YKSK,GSK,YRSR
      LOGICAL UPD1,LRESET,FIRST
C
C THIS ROUTINE SETS UPT THE ARRAYS FOR MSLV
C
      INTEGER LGV, LZ1, LZK, LV, LSK, LYK, LDIAGB, LSR, LYR,
     *   LHYR, LHG, LHYK, LPK, LEMAT, LWTEST
      COMMON/SUBSCR/ LGV,LZ1,LZK,LV,LSK,LYK,LDIAGB,LSR,LYR,
     *     LHYR,LHG,LHYK,LPK,LEMAT,LWTEST
      CALL MSLV(G,Y,N,W(LSK),W(LYK),W(LDIAGB),W(LSR),W(LYR),W(LHYR),
     *     W(LHG),W(LHYK),UPD1,YKSK,GSK,YRSR,LRESET,FIRST)
      RETURN
      END
      SUBROUTINE MSLV(G,Y,N,SK,YK,DIAGB,SR,YR,HYR,HG,HYK,
     *     UPD1,YKSK,GSK,YRSR,LRESET,FIRST)
      INTEGER N
      DOUBLE PRECISION G(N),Y(N)
C
C THIS ROUTINE ACTS AS A PRECONDITIONING STEP FOR THE
C LINEAR CONJUGATE-GRADIENT ROUTINE.  IT IS ALSO THE
C METHOD OF COMPUTING THE SEARCH DIRECTION FROM THE
C GRADIENT FOR THE NON-LINEAR CONJUGATE-GRADIENT CODE.
C IT REPRESENTS A TWO-STEP SELF-SCALED BFGS FORMULA.
C
      DOUBLE PRECISION DDOT,YKSK,GSK,YRSR,RDIAGB,YKHYK,GHYK,
     *     YKSR,YKHYR,YRHYR,GSR,GHYR
      DOUBLE PRECISION SK(N),YK(N),DIAGB(N),SR(N),YR(N),HYR(N),HG(N),
     *     HYK(N),ONE
      LOGICAL LRESET,UPD1,FIRST
      INTEGER I
C
      IF (UPD1) GO TO 100
      ONE = 1.D0
      GSK = DDOT(N,G,1,SK,1)
      IF (LRESET) GO TO 60
C
C COMPUTE HG AND HY WHERE H IS THE INVERSE OF THE DIAGONALS
C
      DO 57 I = 1,N
         RDIAGB = 1.0D0/DIAGB(I)
         HG(I) = G(I)*RDIAGB
         IF (FIRST) HYK(I) = YK(I)*RDIAGB
         IF (FIRST) HYR(I) = YR(I)*RDIAGB
57    CONTINUE
      IF (FIRST) YKSR = DDOT(N,YK,1,SR,1)
      IF (FIRST) YKHYR = DDOT(N,YK,1,HYR,1)
      GSR = DDOT(N,G,1,SR,1)
      GHYR = DDOT(N,G,1,HYR,1)
      IF (FIRST) YRHYR = DDOT(N,YR,1,HYR,1)
      CALL SSBFGS (N, ONE, SR, HG, HYR, YRSR, YRHYR, GSR, GHYR, HG)
      IF (FIRST) CALL SSBFGS (N, ONE, SR, HYK, HYR, YRSR, YRHYR, YKSR,
     *   YKHYR, HYK)
      YKHYK = DDOT(N,HYK,1,YK,1)
      GHYK = DDOT(N,HYK,1,G,1)
      CALL SSBFGS (N, ONE, SK, HG, HYK, YKSK, YKHYK, GSK, GHYK, Y)
      RETURN
60    CONTINUE
C
C COMPUTE GH AND HY WHERE H IS THE INVERSE OF THE DIAGONALS
C
      DO 65 I = 1,N
         RDIAGB = 1.D0/DIAGB(I)
         HG(I) = G(I)*RDIAGB
         IF (FIRST) HYK(I) = YK(I)*RDIAGB
65    CONTINUE
      IF (FIRST) YKHYK = DDOT(N,YK,1,HYK,1)
      GHYK = DDOT(N,G,1,HYK,1)
      CALL SSBFGS (N, ONE, SK, HG, HYK, YKSK, YKHYK, GSK, GHYK, Y)
      RETURN
100   CONTINUE
      DO 110 I = 1,N
110      Y(I) = G(I) / DIAGB(I)
      RETURN
      END
      SUBROUTINE SSBFGS (N, GAMMA, SJ, HJV, HJYJ, YJSJ, YJHYJ,
     *   VSJ, VHYJ, HJP1V)
C-----------------------------------------------------------------------
C SELF-SCALED BFGS
C-----------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION GAMMA, YJSJ, YJHYJ, VSJ, VHYJ
      DOUBLE PRECISION SJ(N), HJV(N), HJYJ(N), HJP1V(N)
C
      INTEGER I
      DOUBLE PRECISION BETA,DELTA
C-----------------------------------------------------------------------
      DELTA = (1.D0 + GAMMA*YJHYJ/YJSJ)*VSJ/YJSJ
     *     - GAMMA*VHYJ/YJSJ
      BETA = -GAMMA*VSJ/YJSJ
      DO 10 I = 1,N
         HJP1V(I) = GAMMA*HJV(I) + DELTA*SJ(I) + BETA*HJYJ(I)
10    CONTINUE
      RETURN
      END
C
C ROUTINES TO INITIALIZE PRECONDITIONER
C
      SUBROUTINE INITPC(DIAGB,EMAT,N,W,LW,MODET,
     *     UPD1,YKSK,YRSR,LRESET)
C-----------------------------------------------------------------------
      INTEGER N, LW, MODET
      DOUBLE PRECISION DIAGB(N),EMAT(N),W(LW)
      DOUBLE PRECISION YKSK,YRSR
      LOGICAL LRESET,UPD1
      INTEGER LGV, LZ1, LZK, LV, LSK, LYK, LDIAGB, LSR, LYR,
     *   LHYR, LHG, LHYK, LPK, LEMAT, LWTEST
      COMMON/SUBSCR/ LGV,LZ1,LZK,LV,LSK,LYK,LDIAGB,LSR,LYR,
     *     LHYR,LHG,LHYK,LPK,LEMAT,LWTEST
C-----------------------------------------------------------------------
      CALL INITP3(DIAGB,EMAT,N,LRESET,YKSK,YRSR,W(LHYK),
     *     W(LSK),W(LYK),W(LSR),W(LYR),MODET,UPD1)
      RETURN
      END
      SUBROUTINE INITP3(DIAGB,EMAT,N,LRESET,YKSK,YRSR,BSK,
     *     SK,YK,SR,YR,MODET,UPD1)
      INTEGER N, MODET
      DOUBLE PRECISION DIAGB(N),EMAT(N),YKSK,YRSR,BSK(N),SK(N),
     *     YK(N),COND,SR(N),YR(N),DDOT,SDS,SRDS,YRSK,TD,D1,DN
      LOGICAL LRESET,UPD1
      INTEGER I
C
      IF (UPD1) GO TO 90
      IF (LRESET) GO TO 60
      DO 10 I = 1,N
         BSK(I) = DIAGB(I)*SR(I)
10    CONTINUE
      SDS = DDOT(N,SR,1,BSK,1)
      SRDS = DDOT(N,SK,1,BSK,1)
      YRSK = DDOT(N,YR,1,SK,1)
      DO 20 I = 1,N
         TD = DIAGB(I)
         BSK(I) = TD*SK(I) - BSK(I)*SRDS/SDS+YR(I)*YRSK/YRSR
         EMAT(I) = TD-TD*TD*SR(I)*SR(I)/SDS+YR(I)*YR(I)/YRSR
20    CONTINUE
      SDS = DDOT(N,SK,1,BSK,1)
      DO 30 I = 1,N
         EMAT(I) = EMAT(I) - BSK(I)*BSK(I)/SDS+YK(I)*YK(I)/YKSK
30    CONTINUE
      GO TO 110
60    CONTINUE
      DO 70 I = 1,N
         BSK(I) = DIAGB(I)*SK(I)
70    CONTINUE
      SDS = DDOT(N,SK,1,BSK,1)
      DO 80 I = 1,N
         TD = DIAGB(I)
         EMAT(I) = TD - TD*TD*SK(I)*SK(I)/SDS + YK(I)*YK(I)/YKSK
80    CONTINUE
      GO TO 110
90    CONTINUE
      CALL BLASCP(N,DIAGB,1,EMAT,1)
110   CONTINUE
      IF (MODET .LT. 1) RETURN
      D1 = EMAT(1)
      DN = EMAT(1)
      DO 120 I = 1,N
         IF (EMAT(I) .LT. D1) D1 = EMAT(I)
         IF (EMAT(I) .GT. DN) DN = EMAT(I)
120   CONTINUE
      COND = DN/D1
      RETURN
      END
C
C
      SUBROUTINE SETPAR(N)
      INTEGER N
      INTEGER LSUB(14), LWTEST, I
      COMMON/SUBSCR/ LSUB,LWTEST
C
C SET UP PARAMETERS FOR THE OPTIMIZATION ROUTINE
C
      DO 10 I = 1,14
          LSUB(I) = (I-1)*N + 1
10    CONTINUE
      LWTEST = LSUB(14) + N - 1
      RETURN
      END
C
C      LINE SEARCH ALGORITHMS OF GILL AND MURRAY
C
      SUBROUTINE LINDER(N,SFUN,SMALL,EPSMCH,RELTOL,ABSTOL,
     *     TNYTOL,ETA,XBND,P,GTP,X,F,ALPHA,G,NFTOTL,
     *     IFLAG,W,LW)
C-----------------------------------------------------------------------
      INTEGER N,NFTOTL,IFLAG,LW
      DOUBLE PRECISION SMALL,EPSMCH,RELTOL,ABSTOL,TNYTOL,ETA,
     *     XBND,GTP,F,ALPHA
      DOUBLE PRECISION P(N),X(N),G(N),W(LW)
C
      INTEGER I,IENTRY,ITEST,L,LG,LX,NUMF,ITCNT, LSPRNT, NPRNT
      DOUBLE PRECISION A,B,B1,BIG,E,FACTOR,FMIN,FPRESN,FU,
     *     FW,GMIN,GTEST1,GTEST2,GU,GW,OLDF,SCXBND,STEP,
     *     TOL,U,XMIN,XW,RMU,RTSMLL,UALPHA
      LOGICAL BRAKTD
C
C      THE FOLLOWING STANDARD FUNCTIONS AND SYSTEM FUNCTIONS ARE
C      CALLED WITHIN LINDER
C
      DOUBLE PRECISION DDOT,DSQRT
      EXTERNAL SFUN
C-----------------------------------------------------------------------
C
C      ALLOCATE THE ADDRESSES FOR LOCAL WORKSPACE
C
      LX = 1
      LG = LX + N
      LSPRNT = 0
      NPRNT  = 10000
      RTSMLL = DSQRT(SMALL)
      BIG = 1.D0/SMALL
      ITCNT = 0
C
C      SET THE ESTIMATED RELATIVE PRECISION IN F(X).
C
      FPRESN = 10.D0*EPSMCH
      NUMF = 0
      U = ALPHA
      FU = F
      FMIN = F
      GU = GTP
      RMU = 1.0D-4
C
C      FIRST ENTRY SETS UP THE INITIAL INTERVAL OF UNCERTAINTY.
C
      IENTRY = 1
10    CONTINUE
C
C TEST FOR TOO MANY ITERATIONS
C
      ITCNT = ITCNT + 1
      IFLAG = 1
      IF (ITCNT .GT. 20) GO TO 50
      IFLAG = 0
      CALL GETPTC (BIG, RTSMLL, RELTOL, ABSTOL, TNYTOL, FPRESN, ETA,
     *   RMU, XBND, U, FU, GU, XMIN, FMIN, GMIN, XW, FW, GW, A, B, OLDF,
     *   B1, SCXBND, E, STEP, FACTOR, BRAKTD, GTEST1, GTEST2, TOL,
     *   IENTRY, ITEST)
C                                       LSOUT
      IF (LSPRNT.GE.NPRNT) CALL LSOUT (XMIN, XW, U, A, B, SCXBND)
C
C      IF ITEST=1, THE ALGORITHM REQUIRES THE FUNCTION VALUE TO BE
C      CALCULATED.
C
      IF (ITEST .NE. 1) GO TO 30
      UALPHA = XMIN + U
      L = LX
      DO 20 I = 1,N
         W(L) = X(I) + UALPHA*P(I)
         L = L + 1
20    CONTINUE
      CALL SFUN(N,W(LX),FU,W(LG))
      NUMF = NUMF + 1
      GU = DDOT(N,W(LG),1,P,1)
C
C      THE GRADIENT VECTOR CORRESPONDING TO THE BEST POINT IS
C      OVERWRITTEN IF FU IS LESS THAN FMIN AND FU IS SUFFICIENTLY
C      LOWER THAN F AT THE ORIGIN.
C
      IF (FU .LE. FMIN .AND. FU .LE. OLDF-UALPHA*GTEST1)
     *     CALL BLASCP(N,W(LG),1,G,1)
      GOTO 10
C
C      IF ITEST=2 OR 3 A LOWER POINT COULD NOT BE FOUND
C
30    CONTINUE
      NFTOTL = NUMF
      IFLAG = 1
      IF (ITEST .NE. 0) GO TO 50
C
C      IF ITEST=0 A SUCCESSFUL SEARCH HAS BEEN MADE
C
      IFLAG = 0
      F = FMIN
      ALPHA = XMIN
      DO 40 I = 1,N
         X(I) = X(I) + ALPHA*P(I)
40    CONTINUE
50    RETURN
      END
C
C
      SUBROUTINE GETPTC(BIG,RTSMLL,RELTOL,ABSTOL,TNYTOL,
     *     FPRESN,ETA,RMU,XBND,U,FU,GU,XMIN,FMIN,GMIN,
     *     XW,FW,GW,A,B,OLDF,B1,SCXBND,E,STEP,FACTOR,
     *     BRAKTD,GTEST1,GTEST2,TOL,IENTRY,ITEST)
      LOGICAL BRAKTD
      INTEGER IENTRY,ITEST
      DOUBLE PRECISION BIG,RTSMLL,RELTOL,ABSTOL,TNYTOL,
     *     FPRESN,ETA,RMU,XBND,U,FU,GU,XMIN,FMIN,GMIN,
     *     XW,FW,GW,A,B,OLDF,B1,SCXBND,E,STEP,FACTOR,
     *     GTEST1,GTEST2,TOL,DENOM
C
C ************************************************************
C GETPTC, AN ALGORITHM FOR FINDING A STEPLENGTH, CALLED REPEATEDLY BY
C ROUTINES WHICH REQUIRE A STEP LENGTH TO BE COMPUTED USING CUBIC
C INTERPOLATION. THE PARAMETERS CONTAIN INFORMATION ABOUT THE INTERVAL
C IN WHICH A LOWER POINT IS TO BE FOUND AND FROM THIS GETPTC COMPUTES A
C POINT AT WHICH THE FUNCTION CAN BE EVALUATED BY THE CALLING PROGRAM.
C THE VALUE OF THE INTEGER PARAMETERS IENTRY DETERMINES THE PATH TAKEN
C THROUGH THE CODE.
C ************************************************************
C
      LOGICAL CONVRG
      DOUBLE PRECISION ABGMIN,ABGW,ABSR,A1,CHORDM,CHORDU,
     *     D1,D2,P,Q,R,S,SCALE,SUMSQ,TWOTOL,XMIDPT
      DOUBLE PRECISION ZERO, POINT1,HALF,ONE,THREE,FIVE,ELEVEN
C
C THE FOLLOWING STANDARD FUNCTIONS AND SYSTEM FUNCTIONS ARE CALLED
C WITHIN GETPTC
C
      DOUBLE PRECISION DABS, DSQRT
C
      ZERO = 0.D0
      POINT1 = 1.D-1
      HALF = 5.D-1
      ONE = 1.D0
      THREE = 3.D0
      FIVE = 5.D0
      ELEVEN = 11.D0
C
C      BRANCH TO APPROPRIATE SECTION OF CODE DEPENDING ON THE
C      VALUE OF IENTRY.
C
      GOTO (10,20), IENTRY
C
C      IENTRY=1
C      CHECK INPUT PARAMETERS
C
10      ITEST = 2
      IF (U .LE. ZERO .OR. XBND .LE. TNYTOL .OR. GU .GT. ZERO)
     *     RETURN
      ITEST = 1
      IF (XBND .LT. ABSTOL) ABSTOL = XBND
      TOL = ABSTOL
      TWOTOL = TOL + TOL
C
C A AND B DEFINE THE INTERVAL OF UNCERTAINTY, X AND XW ARE POINTS
C WITH LOWEST AND SECOND LOWEST FUNCTION VALUES SO FAR OBTAINED.
C INITIALIZE A,SMIN,XW AT ORIGIN AND CORRESPONDING VALUES OF
C FUNCTION AND PROJECTION OF THE GRADIENT ALONG DIRECTION OF SEARCH
C AT VALUES FOR LATEST ESTIMATE AT MINIMUM.
C
      A = ZERO
      XW = ZERO
      XMIN = ZERO
      OLDF = FU
      FMIN = FU
      FW = FU
      GW = GU
      GMIN = GU
      STEP = U
      FACTOR = FIVE
C
C      THE MINIMUM HAS NOT YET BEEN BRACKETED.
C
      BRAKTD = .FALSE.
C
C SET UP XBND AS A BOUND ON THE STEP TO BE TAKEN. (XBND IS NOT COMPUTED
C EXPLICITLY BUT SCXBND IS ITS SCALED VALUE.)  SET THE UPPER BOUND
C ON THE INTERVAL OF UNCERTAINTY INITIALLY TO XBND + TOL(XBND).
C
      SCXBND = XBND
      B = SCXBND + RELTOL*DABS(SCXBND) + ABSTOL
      E = B + B
      B1 = B
C
C COMPUTE THE CONSTANTS REQUIRED FOR THE TWO CONVERGENCE CRITERIA.
C
      GTEST1 = -RMU*GU
      GTEST2 = -ETA*GU
C
C SET IENTRY TO INDICATE THAT THIS IS THE FIRST ITERATION
C
      IENTRY = 2
      GO TO 210
C
C IENTRY = 2
C
C UPDATE A,B,XW, AND XMIN
C
20      IF (FU .GT. FMIN) GO TO 60
C
C IF FUNCTION VALUE NOT INCREASED, NEW POINT BECOMES NEXT
C ORIGIN AND OTHER POINTS ARE SCALED ACCORDINGLY.
C
      CHORDU = OLDF - (XMIN + U)*GTEST1
      IF (FU .LE. CHORDU) GO TO 30
C
C THE NEW FUNCTION VALUE DOES NOT SATISFY THE SUFFICIENT DECREASE
C CRITERION. PREPARE TO MOVE THE UPPER BOUND TO THIS POINT AND
C FORCE THE INTERPOLATION SCHEME TO EITHER BISECT THE INTERVAL OF
C UNCERTAINTY OR TAKE THE LINEAR INTERPOLATION STEP WHICH ESTIMATES
C THE ROOT OF F(ALPHA)=CHORD(ALPHA).
C
      CHORDM = OLDF - XMIN*GTEST1
      GU = -GMIN
      DENOM = CHORDM-FMIN
      IF (DABS(DENOM) .GE. 1.D-15) GO TO 25
          DENOM = 1.D-15
          IF (CHORDM-FMIN .LT. 0.D0)  DENOM = -DENOM
25    CONTINUE
      IF (XMIN .NE. ZERO) GU = GMIN*(CHORDU-FU)/DENOM
      FU = HALF*U*(GMIN+GU) + FMIN
      IF (FU .LT. FMIN) FU = FMIN
      GO TO 60
30      FW = FMIN
      FMIN = FU
      GW = GMIN
      GMIN = GU
      XMIN = XMIN + U
      A = A-U
      B = B-U
      XW = -U
      SCXBND = SCXBND - U
      IF (GU .LE. ZERO) GO TO 40
      B = ZERO
      BRAKTD = .TRUE.
      GO TO 50
40    A = ZERO
50    TOL = DABS(XMIN)*RELTOL + ABSTOL
      GO TO 90
C
C IF FUNCTION VALUE INCREASED, ORIGIN REMAINS UNCHANGED
C BUT NEW POINT MAY NOW QUALIFY AS W.
C
60    IF (U .LT. ZERO) GO TO 70
      B = U
      BRAKTD = .TRUE.
      GO TO 80
70    A = U
80    XW = U
      FW = FU
      GW = GU
90    TWOTOL = TOL + TOL
      XMIDPT = HALF*(A + B)
C
C CHECK TERMINATION CRITERIA
C
      CONVRG = DABS(XMIDPT) .LE. TWOTOL - HALF*(B-A) .OR.
     *     DABS(GMIN) .LE. GTEST2 .AND. FMIN .LT. OLDF .AND.
     *     (DABS(XMIN - XBND) .GT. TOL .OR. .NOT. BRAKTD)
      IF (.NOT. CONVRG) GO TO 100
      ITEST = 0
      IF (XMIN .NE. ZERO) RETURN
C
C IF THE FUNCTION HAS NOT BEEN REDUCED, CHECK TO SEE THAT THE RELATIVE
C CHANGE IN F(X) IS CONSISTENT WITH THE ESTIMATE OF THE DELTA-
C UNIMODALITY CONSTANT, TOL.  IF THE CHANGE IN F(X) IS LARGER THAN
C EXPECTED, REDUCE THE VALUE OF TOL.
C
      ITEST = 3
      IF (DABS(OLDF-FW) .LE. FPRESN*(ONE + DABS(OLDF))) RETURN
      TOL = POINT1*TOL
      IF (TOL .LT. TNYTOL) RETURN
      RELTOL = POINT1*RELTOL
      ABSTOL = POINT1*ABSTOL
      TWOTOL = POINT1*TWOTOL
C
C CONTINUE WITH THE COMPUTATION OF A TRIAL STEP LENGTH
C
100   R = ZERO
      Q = ZERO
      S = ZERO
      IF (DABS(E) .LE. TOL) GO TO 150
C
C FIT CUBIC THROUGH XMIN AND XW
C
      R = THREE*(FMIN-FW)/XW + GMIN + GW
      ABSR = DABS(R)
      Q = ABSR
      IF (GW .EQ. ZERO .OR. GMIN .EQ. ZERO) GO TO 140
C
C COMPUTE THE SQUARE ROOT OF (R*R - GMIN*GW) IN A WAY
C WHICH AVOIDS UNDERFLOW AND OVERFLOW.
C
      ABGW = DABS(GW)
      ABGMIN = DABS(GMIN)
      S = DSQRT(ABGMIN)*DSQRT(ABGW)
      IF ((GW/ABGW)*GMIN .GT. ZERO) GO TO 130
C
C COMPUTE THE SQUARE ROOT OF R*R + S*S.
C
      SUMSQ = ONE
      P = ZERO
      IF (ABSR .GE. S) GO TO 110
C
C THERE IS A POSSIBILITY OF OVERFLOW.
C
      IF (S .GT. RTSMLL) P = S*RTSMLL
      IF (ABSR .GE. P) SUMSQ = ONE +(ABSR/S)**2
      SCALE = S
      GO TO 120
C
C THERE IS A POSSIBILITY OF UNDERFLOW.
C
110   IF (ABSR .GT. RTSMLL) P = ABSR*RTSMLL
      IF (S .GE. P) SUMSQ = ONE + (S/ABSR)**2
      SCALE = ABSR
120   SUMSQ = DSQRT(SUMSQ)
      Q = BIG
      IF (SCALE .LT. BIG/SUMSQ) Q = SCALE*SUMSQ
      GO TO 140
C
C COMPUTE THE SQUARE ROOT OF R*R - S*S
C
130   Q = DSQRT(DABS(R+S))*DSQRT(DABS(R-S))
      IF (R .GE. S .OR. R .LE. (-S)) GO TO 140
      R = ZERO
      Q = ZERO
      GO TO 150
C
C COMPUTE THE MINIMUM OF FITTED CUBIC
C
140   IF (XW .LT. ZERO) Q = -Q
      S = XW*(GMIN - R - Q)
      Q = GW - GMIN + Q + Q
      IF (Q .GT. ZERO) S = -S
      IF (Q .LE. ZERO) Q = -Q
      R = E
      IF (B1 .NE. STEP .OR. BRAKTD) E = STEP
C
C CONSTRUCT AN ARTIFICIAL BOUND ON THE ESTIMATED STEPLENGTH
C
150   A1 = A
      B1 = B
      STEP = XMIDPT
      IF (BRAKTD) GO TO 160
      STEP = -FACTOR*XW
      IF (STEP .GT. SCXBND) STEP = SCXBND
      IF (STEP .NE. SCXBND) FACTOR = FIVE*FACTOR
      GO TO 170
C
C IF THE MINIMUM IS BRACKETED BY 0 AND XW THE STEP MUST LIE
C WITHIN (A,B).
C
160   IF ((A .NE. ZERO .OR. XW .GE. ZERO) .AND. (B .NE. ZERO .OR.
     *     XW .LE. ZERO)) GO TO 180
C
C IF THE MINIMUM IS NOT BRACKETED BY 0 AND XW THE STEP MUST LIE
C WITHIN (A1,B1).
C
      D1 = XW
      D2 = A
      IF (A .EQ. ZERO) D2 = B
C THIS LINE MIGHT BE
C     IF (A .EQ. ZERO) D2 = E
      U = - D1/D2
      STEP = FIVE*D2*(POINT1 + ONE/U)/ELEVEN
      IF (U .LT. ONE) STEP = HALF*D2*DSQRT(U)
170   IF (STEP .LE. ZERO) A1 = STEP
      IF (STEP .GT. ZERO) B1 = STEP
C
C REJECT THE STEP OBTAINED BY INTERPOLATION IF IT LIES OUTSIDE THE
C REQUIRED INTERVAL OR IT IS GREATER THAN HALF THE STEP OBTAINED
C DURING THE LAST-BUT-ONE ITERATION.
C
180   IF (DABS(S) .LE. DABS(HALF*Q*R) .OR.
     *     S .LE. Q*A1 .OR. S .GE. Q*B1) GO TO 200
C
C A CUBIC INTERPOLATION STEP
C
      STEP = S/Q
C
C THE FUNCTION MUST NOT BE EVALUTATED TOO CLOSE TO A OR B.
C
      IF (STEP - A .GE. TWOTOL .AND. B - STEP .GE. TWOTOL) GO TO 210
      IF (XMIDPT .GT. ZERO) GO TO 190
      STEP = -TOL
      GO TO 210
190   STEP = TOL
      GO TO 210
200   E = B-A
C
C IF THE STEP IS TOO LARGE, REPLACE BY THE SCALED BOUND (SO AS TO
C COMPUTE THE NEW POINT ON THE BOUNDARY).
C
210   IF (STEP .LT. SCXBND) GO TO 220
      STEP = SCXBND
C
C MOVE SXBD TO THE LEFT SO THAT SBND + TOL(XBND) = XBND.
C
      SCXBND = SCXBND - (RELTOL*DABS(XBND)+ABSTOL)/(ONE + RELTOL)
220   U = STEP
      IF (DABS(STEP) .LT. TOL .AND. STEP .LT. ZERO) U = -TOL
      IF (DABS(STEP) .LT. TOL .AND. STEP .GE. ZERO) U = TOL
      ITEST = 1
      RETURN
      END
C--------------------------------------------------------------------
C%% TRUNCATED-NEWTON METHOD: BLAS
C   NOTE: ALL ROUTINES HERE ARE FROM LINPACK WITH THE EXCEPTION
C         OF DXPY (A VERSION OF DAXPY WITH A=1.0)
C   WRITTEN BY:  STEPHEN G. NASH
C                OPERATIONS RESEARCH AND APPLIED STATISTICS DEPT.
C                GEORGE MASON UNIVERSITY
C                FAIRFAX, VA 22030
C****************************************************************
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(1),DY(1),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      DDOT = 0.0D0
      DTEMP = 0.0D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DTEMP + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      DDOT = DTEMP
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF(M .EQ. 0) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF(N .LT. 5) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DTEMP = DTEMP + DX(I)*DY(I) + DX(I + 1)*DY(I + 1) +
     *   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
   60 DDOT = DTEMP
      RETURN
      END
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
C
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(1),DY(1),DA
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF (DA .EQ. 0.0D0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DY(IY) + DA*DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,4)
      IF(M .EQ. 0) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF(N .LT. 4) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DNRM2 (N, DX, INCX)
C-----------------------------------------------------------------------
C     EUCLIDEAN NORM OF THE N-VECTOR STORED IN DX() WITH STORAGE
C     INCREMENT INCX .
C     IF    N .LE. 0 RETURN WITH RESULT = 0.
C     IF N .GE. 1 THEN INCX MUST BE .GE. 1
C
C           C.L.LAWSON, 1978 JAN 08
C
C     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE
C     HOPEFULLY APPLICABLE TO ALL MACHINES.
C         CUTLO = MAXIMUM OF  DSQRT(U/EPS)  OVER ALL KNOWN MACHINES.
C         CUTHI = MINIMUM OF  DSQRT(V)      OVER ALL KNOWN MACHINES.
C     WHERE
C         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.
C         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)
C         V   = LARGEST  NO.            (OVERFLOW  LIMIT)
C
C     BRIEF OUTLINE OF ALGORITHM..
C
C     PHASE 1    SCANS ZERO COMPONENTS.
C     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO
C     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO
C     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M
C     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.
C
C     VALUES FOR CUTLO AND CUTHI..
C     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER
C     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..
C     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE
C                   UNIVAC AND DEC AT 2**(-103)
C                   THUS CUTLO = 2**(-51) = 4.44089E-16
C     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.
C                   THUS CUTHI = 2**(63.5) = 1.30438E19
C     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC.
C                   THUS CUTLO = 2**(-33.5) = 8.23181D-11
C     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D19
C     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
C-----------------------------------------------------------------------
      INTEGER          NEXT, I, J, NN, N, INCX
      DOUBLE PRECISION DX(1),CUTLO,CUTHI,HITEST,SUM,XMAX,ZERO,ONE
      DATA   ZERO, ONE /0.0D0, 1.0D0/
      DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C-----------------------------------------------------------------------
      IF(N .GT. 0) GO TO 10
         DNRM2  = ZERO
         GO TO 300
C
   10 NEXT = 30
      SUM = ZERO
      NN = N * INCX
C                                                 BEGIN MAIN LOOP
      I = 1
C                                       Replace obsolete
C                                       ASSIGN n TO NEXT
C                                       GO TO NEXT, (30,50,70,110)
 20   IF (NEXT.EQ.50) THEN
         GO TO 50
      ELSE IF (NEXT.EQ.70) THEN
         GO TO 70
      ELSE IF (NEXT.EQ.110) THEN
         GO TO 110
         END IF
      IF(DABS(DX(I)) .GT. CUTLO) GO TO 85
      NEXT = 50
      XMAX = ZERO
C
C                        PHASE 1.  SUM IS ZERO
C
 50   IF(DX(I) .EQ. ZERO) GO TO 200
      IF(DABS(DX(I)) .GT. CUTLO) GO TO 85
C
C                                PREPARE FOR PHASE 2.
      NEXT = 70
      GO TO 105
C
C                                PREPARE FOR PHASE 4.
C
 100  I = J
      NEXT = 110
      SUM = (SUM / DX(I)) / DX(I)
 105  XMAX = DABS(DX(I))
      GO TO 115
C
C                   PHASE 2.  SUM IS SMALL.
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF(DABS(DX(I)) .GT. CUTLO) GO TO 75
C
C                     COMMON CODE FOR PHASES 2 AND 4.
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF(DABS(DX(I)) .LE. XMAX) GO TO 115
         SUM = ONE + SUM * (XMAX / DX(I))**2
         XMAX = DABS(DX(I))
         GO TO 200
C
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
C
C
C                  PREPARE FOR PHASE 3.
C
   75 SUM = (SUM * XMAX) * XMAX
C
C
C     FOR REAL OR D.P. SET HITEST = CUTHI/N
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
C
   85 HITEST = CUTHI/FLOAT(N)
C
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J =I,NN,INCX
      IF(DABS(DX(J)) .GE. HITEST) GO TO 100
   95    SUM = SUM + DX(J)**2
      DNRM2 = DSQRT(SUM)
      GO TO 300
C
  200 CONTINUE
      I = I + INCX
      IF (I .LE. NN) GO TO 20
C
C              END OF MAIN LOOP.
C
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
C
      DNRM2 = XMAX * DSQRT(SUM)
  300 CONTINUE
      RETURN
      END
      SUBROUTINE BLASCP(N,DX,INCX,DY,INCY)
C
C     COPIES A VECTOR, X, TO A VECTOR, Y.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(1),DY(1)
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,7)
      IF(M .EQ. 0) GO TO 40
      DO 30 I = 1,M
        DY(I) = DX(I)
   30 CONTINUE
      IF(N .LT. 7) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,7
        DY(I) = DX(I)
        DY(I + 1) = DX(I + 1)
        DY(I + 2) = DX(I + 2)
        DY(I + 3) = DX(I + 3)
        DY(I + 4) = DX(I + 4)
        DY(I + 5) = DX(I + 5)
        DY(I + 6) = DX(I + 6)
   50 CONTINUE
      RETURN
      END
C******************************************************************
C SPECIAL BLAS FOR Y = X+Y
C******************************************************************
      SUBROUTINE DXPY(N,DX,INCX,DY,INCY)
C
C     VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     STEPHEN G. NASH 5/30/89.
C
      DOUBLE PRECISION DX(1),DY(1)
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DY(IY) + DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,4)
      IF(M .EQ. 0) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DX(I)
   30 CONTINUE
      IF(N .LT. 4) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DX(I)
        DY(I + 1) = DY(I + 1) + DX(I + 1)
        DY(I + 2) = DY(I + 2) + DX(I + 2)
        DY(I + 3) = DY(I + 3) + DX(I + 3)
   50 CONTINUE
      RETURN
      END
