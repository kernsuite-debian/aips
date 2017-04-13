LOCAL INCLUDE 'BPASS.INC'
C                                                         Include BPASS
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       Local include for BPASS
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, XCALCO*4,
     *   NAME3*12, CLAS3*6, CMETH*4, SOLTYP*4, CMOD*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XNAME2(3),
     *   XCLAS2(2), XXCALC, XNAME3(3), XCLAS3(2), XMETH, XCMOD, XSOLT,
     *   CATH3(256)
      INTEGER   SEQIN, SEQ2, SEQ3, DISKIN, DISK2, DISK3, SEQOUT, CNOIN,
     *   CNOIN2, CNOIN3, REFANT, CATIN(256), CATI3(256), PRTLV, VISDSK,
     *   VISCNO, CCTVER, VER, NUMHIS, CHNSEL(3,20,MAXIF), NUMBL, MODE,
     *   MINNO, NBL, BLCODE(MXBASE), IS(MXBASE), JS(MXBASE), LUNI,
     *   FINDI, LUNBP, LUN3, FIND3, NCLSER, MAXTEL, BPOVER, BPREC
      INTEGER   MXENTR, MXLOOP, MXCALS
C                                       MXCALS max # cals in spectral
C                                       index adverbs
      PARAMETER (MXCALS = 30)
C                                       MXLOOP is the maximum number
C                                       of loops through BASOLV
C                                       permitted.
      PARAMETER (MXLOOP = 15)
C                                       MXENTR is the maximum number
C                                       of time entries allowed in the
C                                       BP table.
      PARAMETER (MXENTR = 5000)
      REAL     XSIN, XDISIN, XQUAL, XUVR(2), XTIME(8), XBAND, XFREQ,
     *   XFQID, XSUBA, XANTNS(50), XS2, XD2, XVER, XNCOMP(MAXAFL),
     *   XFLUX, XNMAP, SMODEL(7), XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSOLIN, XREFAN, XBOVER, XSMOTH(3),
     *   XANTW(30), XWTIT, AMPMAX, PHMAX, XMINNO, APARM(11),
     *   XCHNS(4,20), DOSCAL, ASPEC, ACURVE(3), XS3, XD3, XBADD(10)
      REAL   SOLINT, BUFF1(UVBFSL), BUFF2(UVBFSL), ANTWT(MAXANT), TINTG,
     *   TINTGH, AMPAXX, PHMAXX, CATR3(256), FINC(MAXIF), XSPEC(MXCALS),
     *   XCURVE(3,MXCALS)
      DOUBLE PRECISION CHNSHF(MAXIF), CATD3(128), TIMWRT(MXENTR),
     *   FOFF(MAXIF)
      INTEGER   REFUSE(MAXANT), NCOMP(MAXFLD), MXBPRN, CURBPN, NCALNO,
     *   TIMPT(MXLOOP), JLOOP, BPREF(MXENTR,2), XCALNO(MXCALS),
     *   DELRNG(2,MAXANT), DELINC, MAXSHF, STRTSH, BPNORM, ISBAND(MAXIF)
      LOGICAL   ACONLY, AVGPOL, DOMODL, SINGLE, DONDX, PHSONL, DIVCH0,
     *   PHONLY, AVGALL, LINEAR, IQUV, SCALAR, EXTCH0, DOINTP, WSCALE,
     *   ANTWRT(MXENTR,MAXANT), VLBA, CHNDWT, EVN, AMPONL
      INTEGER   JBUFSZ, NUMFRQ, MAXIFS, IKLOCF, DOSPEC
      CHARACTER BNDCOD(MAXIF)*8
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XXCALC, XUVR, XTIME, XBAND, XFREQ, XFQID, XSUBA, XANTNS,
     *   XNAME2, XCLAS2, XS2, XD2, XVER, XNCOMP, XFLUX, XNMAP, XMETH,
     *   XCMOD, SMODEL, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSOLIN, XSOLT, XREFAN, XBOVER, XSMOTH, XANTW,
     *   XWTIT, AMPMAX, PHMAX, XMINNO, APARM, XCHNS, DOSCAL, ASPEC,
     *   ACURVE, XNAME3, XCLAS3, XS3, XD3, XBADD
      COMMON /CHPARM/ NAMEIN, CLAIN, NAME2, CLAS2, XCALCO, NAME3, CLAS3,
     *   CMETH, CMOD, SOLTYP, BNDCOD
      COMMON /BPPARM/ CHNSHF, TIMWRT,
     *   NUMHIS, CHNSEL, BUFF1, BUFF2, DELRNG, DELINC, MAXSHF, STRTSH,
     *   SOLINT, ANTWT, TINTG, TINTGH, AMPAXX, PHMAXX, REFUSE,
     *   NCOMP, MXBPRN, ACONLY, AVGPOL, DOMODL, SINGLE, DONDX,
     *   EXTCH0, PHSONL, DIVCH0, PHONLY, AVGALL, LINEAR, IQUV, SCALAR,
     *   BPNORM, DOINTP, ANTWRT, REFANT, CATIN, PRTLV, VISDSK, VISCNO,
     *   JBUFSZ, VER, NUMFRQ, NUMBL, MAXTEL, MODE, MINNO, NBL, BLCODE,
     *   IS, JS, LUNI, FINDI, LUNBP, LUN3, FIND3, CURBPN, TIMPT, MAXIFS,
     *   IKLOCF, BPREF, JLOOP, VLBA, CHNDWT, NCLSER, WSCALE, EVN,
     *   BPOVER, BPREC,AMPONL, SEQIN, SEQ2, SEQ3, SEQOUT, DISK2, DISK3,
     *   DISKIN, CNOIN, CNOIN2, CNOIN3, CCTVER, XSPEC, XCURVE, NCALNO,
     *   XCALNO, DOSPEC
      COMMON /BPFRQS/ FOFF, FINC, ISBAND
      COMMON /MAP3HD/ CATI3
      EQUIVALENCE (CATI3, CATR3, CATH3, CATD3)
C                                                         End BPASS
LOCAL END
LOCAL INCLUDE 'BPASS2.INC'
C                                                         Include BPASS2
C                                       2nd local include for BPASS
C                                       NOTE: uses PARAMETER in PUVD.INC
      INTEGER   MAXPRM
C                                       MAXPRM = maximum no. parms in
C                                       Least squares solutions
      PARAMETER (MAXPRM = MAXANT * 2)
      REAL   XOBS(2,MXBASE)
      DOUBLE PRECISION HESS(MAXPRM,MAXPRM), SHESS(MAXPRM,MAXPRM),
     *   XPRM(MAXPRM), GRAD(MAXPRM), STEP(MAXPRM), GWORK(MAXPRM)
      INTEGER SMCHNW(MAXCHA), WTIT, IGWORK(2*MAXPRM)
      EQUIVALENCE (IGWORK, GWORK)
      COMMON /CLBMEM/ HESS, SHESS, XPRM, GRAD, STEP, GWORK, XOBS,
     *   SMCHNW, WTIT
C                                                          End BPASS2
LOCAL END
LOCAL INCLUDE 'EZERO.INC'
      CHARACTER ISORT3*2
      REAL      BUFF3(UVBFSL)
      INTEGER   ILOCT3, ILOCB3, ILOCS3, ILOCQ3, ILCA13, ILCA23, ILCSA3,
     *   JLOCS3, JLOCF3, JLOCI3, INCS3, INCF3, INCIF3, ICOR03, NPARM3,
     *   NCHAN3, NVIS3, NCOR3, BUFSZ3, NIO3, INCOR
      COMMON /EZCHR/ ISORT3
      COMMON /EZPARM/ BUFF3, ILOCT3, ILOCB3, ILOCS3, ILOCQ3, ILCA13,
     *   ILCA23, ILCSA3, JLOCS3, JLOCF3, JLOCI3, INCS3, INCF3, INCIF3,
     *   ICOR03, NPARM3, NVIS3, NCOR3, NCHAN3, BUFSZ3, NIO3, INCOR
LOCAL END
LOCAL INCLUDE 'RECRD.INC'
      INTEGER   RECI4(10+MAXCIF*2+MAXIF*4)
      REAL      RECR4(10+MAXCIF*2+MAXIF*4)
      DOUBLE PRECISION REC8(5+MAXCIF+MAXIF*2)
      EQUIVALENCE (REC8, RECR4, RECI4)
      COMMON /RECRDS/ REC8
LOCAL END
      PROGRAM BPASS
C-----------------------------------------------------------------------
C! BPASS generates the BandPass tables from uv-data.
C# Calibration Spectral VLA VLB EXT-appl AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2017
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
C   the bandpass response functions of the antennas. The program
C   can form the functions in two ways: a) by decomposing the
C   baseline based functions (using a least-squares method) into
C   the antenna based complex bandpass function, or b) by using
C   the autocorrelation data as the amplitude part of the function
C   and setting the phase of the function to zero.
C   AIPS Adverbs:
C     INNAME.....Input UV file name (name).      Standard defaults.
C     INCLASS....Input UV file name (class).     Standard defaults.
C     INSEQ......Input UV file name (seq. #).    0 => highest.
C     INDISK.....Disk drive # of input UV file.  0 => any.
C     SOURCES....Source list.  If the data is a multi-source file
C                BPASS will form the cross-power spectrum for the
C                first source specified. If the data is a single
C                source file no source name need be specified.
C     UVRANGE....Range (min, max) of projected baselines to include
C                0,0 => all baselines (units: klamda)
C     TIMERANG...Time range of the data to be selected. In order:
C                Start day, hour, min. sec,
C                end day, hour, min. sec. Days relative to ref.
C                date.
C     BIF........First IF to copy. 0=>all.
C     EIF........Highest IF to copy. 0=>all higher than BIF
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
C     FLUX.......Minimum CC flux included
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
C     CHANSEL....Array of up to ten sets of start,stop and increment
C                channel numbers.
C     BADDISK....A list of disks on which scratch files are not to
C                be placed.  This will not affect the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'BPASS '/
C-----------------------------------------------------------------------
C                                               Get input parameters
      CALL BPSIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                               Transfer needed data to
C                                               scratch with optional
C                                               calibration.
      CALL BPSEL (IRET)
      IF (IRET.NE.0) GO TO 990
C                                               Divide data by model if
C                                               neccessary.
      IF ((.NOT.DIVCH0) .OR. (PHONLY)) THEN
         IF (DOMODL) CALL BPMOD (IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                               Calculate the bandpass
C                                               functions and write them
C                                               to the BP table
      CALL BPSOL (IRET)
      IF (IRET.NE.0) GO TO 990
C                                               Write history
      CALL BPHIS
C                                               Close down files etc.
 990  CALL DIE (IRET, BUFF1)
C
      STOP
      END
      SUBROUTINE BPSIN (PRGN, JERR)
C-----------------------------------------------------------------------
C  BPSIN get input parameters for BPASS and finds input file.
C  All selection criteria are filled into commons in D/CSEL.INC
C
C  Inputs:
C      PRGN      C*6)     Program name (2chars/word)
C  Outputs:
C      JERR      I        Error code : 0 => OK
C                                      5 => catalog troubles
C                                      8 => can't start
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, UTYPE*2
      INTEGER   JERR,  NPARM, IROUND, IERR, IRET, I, LUNTB, NANT, NFREQ,
     *   MXANT, IUSER, LUN, J, LCOR0, ANVER, K, K1, K2
      REAL      CATR(256)
      LOGICAL   T, F, TABLE, MULTI, FITASC, MATCH, SINGL3, FAIL, NXPRS
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NW(MAXIF)
      INCLUDE 'BPASS.INC'
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
      EQUIVALENCE (CATR,CATD,CATBLK)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNTB / 29 /
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      EVN = .FALSE.
      BPREC = 0
C
      JBUFSZ = UVBFSL*2
      NUMHIS = 0
      NCLSER = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 374 + MAXAFL
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
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAME3, NAME3)
      CALL H2CHR (6, 1, XCLAS3, CLAS3)
      CALL H2CHR (4, 1, XMETH, CMETH)
      CALL H2CHR (4, 1, XCMOD, CMOD)
      CALL H2CHR (4, 1, XSOLT, SOLTYP)

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
         CALL CATIO ('READ', DISK3, CNOIN3, CATBLK, 'REST', BUFF1, IERR)
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
         SINGL3 = (.NOT.MULTI) .OR. (ILOCSU.LT.0)
         CALL COPY (256, CATBLK, CATI3)
         NCHAN3 = CATBLK(KINAX+JLOCF)
         IF (NCHAN3.GT.1) THEN
            WRITE (MSGTXT,1150) NCHAN3
            JERR = 1
            GO TO 990
            END IF
         ISORT3 = ISORT
         IF (ISORT3.NE.'TB') THEN
            WRITE (MSGTXT,1160)
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
         LUN3 = 59
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
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNTB, BUFF1, TABLE, MULTI,
     *   FITASC, IERR)
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      SINGLE = (.NOT.MULTI) .OR. (ILOCSU.LT.0)
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
C                                       Check compatibility of two files
      IF (EXTCH0) THEN
         IF ((ICOR03.NE.ICOR0) .OR. (NCOR3.NE.NCOR)) THEN
            WRITE (MSGTXT,1170)
            JERR = 1
            GO TO 990
            END IF
         IF (NVIS3.NE.NVIS) THEN
            WRITE (MSGTXT,1180)
            JERR = 1
            GO TO 990
            END IF
         FAIL = T
         IF (SINGLE .AND. SINGL3) FAIL = F
         IF ((.NOT.SINGLE) .AND. (.NOT.SINGL3)) FAIL = F
         IF (FAIL) THEN
            WRITE (MSGTXT,1190)
            JERR = 1
            GO TO 990
            END IF
         IF ((XDOCAL.GT.0.0) .AND. (.NOT.SINGL3)) THEN
            MSGTXT = 'WARNING: DOCALIB APPLIES ONLY TO DATA, NOT'
     *         // ' CHANNEL 0 FILE'
            CALL MSGWRT (6)
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
      DO 20 I= 1,30
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
      STOKES = ' '
      NFREQ = CATBLK(KINAX+JLOCF)
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
      NUMFRQ = ECHAN
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = 1
         EIF = CATBLK(KINAX+JLOCIF)
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
C                                       spectral index parameters
      CALL RFILL (MXCALS, 0.0, XSPEC)
      CALL RFILL (3*MXCALS, 0.0, XCURVE)
      XSPEC(1) = ASPEC
      XCURVE(1,1) = ACURVE(1)
      XCURVE(2,1) = ACURVE(2)
      XCURVE(3,1) = ACURVE(3)
      NCALNO = 0
      CALL FILL (MXCALS, 0, XCALNO)
      DOSPEC = 0
      IF (DOSCAL.GE.0.0) DOSPEC = 1
      IF (DOSCAL.GT.1.5) DOSPEC = 2
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
C        MSGTXT = 'The results of BPASS will be incorrect'
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
         WRITE (MSGTXT,1050)
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
      AVGALL = XSOLIN.LT.0.0
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
C                                       Use autocorrelations only ?
      ACONLY = APARM(1).EQ.1
      DOACOR = ACONLY
      DOXCOR = .NOT.ACONLY
C                                       Print level
      PRTLV = IROUND (APARM(2))
C                                       Divide by model ?
      DOMODL = APARM(3).LE.0.0
C                                       Phases only ?
      PHSONL = APARM(4).GT.0.0
      AMPONL = APARM(4).LT.0.0
C                                       Closure errors
      IF ((AMPMAX.GT.0.0) .AND. (PHMAX.GT.0.0)) THEN
         WRITE (MSGTXT,1060) AMPMAX, PHMAX
         CALL MSGWRT (3)
         AMPMAX = LOG10 (1.0 + AMPMAX * 0.01)
         PHMAX = PHMAX / 57.296
      ELSE
         AMPMAX = 0.0
         PHMAX = 0.0
         END IF
      AMPAXX = APARM(6)
      PHMAXX = APARM(7)
C                                       Scalar average?
      SCALAR = APARM(8).GT.0.0
C                                       Interpolation
      DOINTP = APARM(9).GT.0.0
C                                       VLBI style normalization
      BPNORM = IROUND (APARM(10))
      IF (BPNORM.GT.0) THEN
         IF (BPNORM.LE.2) THEN
            MSGTXT = 'Bandpass table (amplitudes) will be normalized'
         ELSE IF (BPNORM.LE.4) THEN
            MSGTXT = 'Bandpass table (amp & phase) will be normalized'
         ELSE
            MSGTXT = 'Bandpass power (amp & phase) will be normalized'
            END IF
         CALL MSGWRT (2)
         END IF
C                                       channel-dependent weights
      CHNDWT = APARM(11).LE.0.0
      IF (CHNDWT) THEN
         MSGTXT = 'Weights in solution will be channel-dependent'
      ELSE
         MSGTXT = 'Weights in solution will be independent of channel'
         END IF
      CALL MSGWRT (2)
C                                       Divide by channel 0?
      WSCALE = APARM(5).LE.-1.5
      DIVCH0 = (APARM(5).LT.0.5) .AND. (APARM(5).GT.-1.5)
      PHONLY = (APARM(5).LE.-0.5) .AND. (APARM(5).GT.-1.5)
C                                       Channel selection for
C                                       channel 0
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
 50            CONTINUE
            IF (CHNSEL(3,NW(K),K).EQ.0) CHNSEL(3,NW(K),K) = 1
 55         CONTINUE
 60      CONTINUE
 65   J = CATBLK(KINAX+JLOCF)
      DO 75 K = 1,MAXIF
         IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = (J + 1) / 8 + 1
            CHNSEL(2,1,K) = J - ((J+1)/8)
            CHNSEL(3,1,K) = 1
         ELSE
            DO 70 I = 1,NW(K)
               CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), J))
               IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K)) CHNSEL(2,I,K) = J
               CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), J))
 70            CONTINUE
            END IF
 75      CONTINUE
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
      MAXIFS = EIF - BIF + 1
      NUMBL = (NANT * (NANT - 1)) / 2
      MAXTEL = NANT
C                                       VLBA ?
      VLBA = ANAME(1:4).EQ.'VLBA'
      EVN = ANAME(1:4).EQ.'EVN '
      ISVLBA = VLBA
      IF (VLBA) THEN
         MSGTXT = 'Array name in AN table is VLBA'
         CALL MSGWRT (2)
         MSGTXT = 'Will assume this is data from the VLBA correlator'
         CALL MSGWRT (2)
         MSGTXT = 'and that it is all fringe-rotated to Earth Centre'
         CALL MSGWRT (2)
         MSGTXT = 'Will remove effects of that fringe-rotation'
         CALL MSGWRT (2)
         MSGTXT = 'to provide consistent bandpass calibration'
         CALL MSGWRT (2)
         MSGTXT = 'If incorrect, abort and change array name keyword'
         CALL MSGWRT (2)
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
 1050 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1060 FORMAT ('Excess closure errors defined at',F6.2,' percent and',
     *   F6.1,' degrees')
 1100 FORMAT ('ANMAXA RETURNS ANTMAX',I4,'  ERROR',I5)
 1150 FORMAT ('EXTERNAL CHANNEL 0 FILE HAS ',I3,' CHANNELS ',
     *   '- CHECK INPUTS')
 1160 FORMAT ('EXTERNAL CHANNEL 0 FILE FILE NOT IN TB SORT ORDER')
 1170 FORMAT ('LINE & CHANNEL 0 FILES HAVE INCOMPATIBLE POLARIZATION')
 1180 FORMAT ('LINE & CHANNEL 0 FILES HAVE DIFFERENT NO. VIS. POINTS')
 1190 FORMAT ('LINE & CHANNEL 0 FILES NOT BOTH MULTI-SOURCE FORMAT')
      END
      SUBROUTINE BPHIS
C-----------------------------------------------------------------------
C   BPHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72
      INTEGER   LUN2, IERR, I, TIME(3), DATE(3), K, J, I1, I2
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'BPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
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
      IF (SINGLE) GO TO 130
C                                       calibration
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       BP version
      WRITE (HILINE, 3080) TSKNAM, BPOVER
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Write control info.
C                                       CC tables
 130  IF ((DOMODL) .AND. (SMODEL(1).LE.0.0)) THEN
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
      IF ((XSOLIN.EQ.0.0) .AND. (.NOT.SINGLE)) THEN
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
C                                       Divide by channel 0
      IF (DIVCH0) THEN
         HILINE = TSKNAM // '/ Dividing data by channel 0 before' //
     *      ' average'
         IF (PHONLY) HILINE = TSKNAM // '/ Subtracting channel 0 phase'
     *      // ' before average'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      IF (WSCALE) THEN
         HILINE = TSKNAM // '/ Dividing data by channel 0 after' //
     *      ' average'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Normalization
      IF (BPNORM.EQ.1) THEN
         HILINE = TSKNAM // '/ Amplitude normalized by all channels'
      ELSE IF (BPNORM.EQ.2) THEN
         HILINE = TSKNAM // '/ Amplitude normalized by ICHANSEL chans'
      ELSE IF (BPNORM.EQ.3) THEN
         HILINE = TSKNAM // '/ Amp & phase normalized by ICHANSEL chans'
      ELSE IF (BPNORM.EQ.4) THEN
         HILINE = TSKNAM // '/ Amp & phase normalized by all channels'
      ELSE IF (BPNORM.EQ.5) THEN
         HILINE = TSKNAM // '/ Amp & phase normalized by ICHANSEL chans'
     *      // ' using BP power'
      ELSE IF (BPNORM.EQ.6) THEN
         HILINE = TSKNAM // '/ Amp & phase normalized by all channels'
     *      // ' using BP power'
      ELSE
         HILINE = TSKNAM // '/ Bandpass not normalized'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
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
C                                       interpolation
      IF (DOINTP) THEN
         HILINE = TSKNAM // '/ Blanks in BP interpolated if possible'
      ELSE
         HILINE = TSKNAM // '/ Blanks in BP not interpolated'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Autocorrelation data
      WRITE (HILINE,2015) TSKNAM, SOLTYP
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
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
      IF (AMPONL) THEN
         WRITE (HILINE,3024) TSKNAM
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
      IF (ABS (SMODEL(1)).GT.0.0) THEN
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
C                                       spectral index
      IF (DOSPEC.GT.0) THEN
         DO 70 I = 1,NCALNO
            WRITE (HILINE,2040) TSKNAM, I, XSPEC(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            WRITE (HILINE,2041) TSKNAM, I, XCURVE(1,I), XCURVE(2,I),
     *         XCURVE(3,I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
 70         CONTINUE
         END IF
C                                        Antenna weights.
      I2 = 0
 80   I1 = I2 + 1
      IF (I1.LE.NUMANT) THEN
         I2 = MIN (I1+8, NUMANT)
         IF (I1.EQ.1) THEN
            WRITE (HILINE,2050) TSKNAM, (ANTWT(I),I=I1,I2)
         ELSE
            WRITE (HILINE,2051) TSKNAM, (ANTWT(I),I=I1,I2)
            END IF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         GO TO 80
         END IF
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
 2015 FORMAT (A6,'SOLTYPE = ''',A4,''' /Gain solution method')
 2020 FORMAT (A6,'SMODEL = ',2(F10.5,','),F10.5,
     *   ' /Pt. model parameters')
 2021 FORMAT (A6,'        ',4F10.5,' / Other parms.')
 2022 FORMAT (A6,'APARM(3) = 1 /Data not divided by model in bpass')
 2040 FORMAT (A6,'SPECINDX(',I3,')=',F7.3,
     *   '  / Corrected by spectral index')
 2041 FORMAT (A6,'SPECURVE(',I3,')=',3F7.4,
     *   '  / Correct spectral curvature')
 2050 FORMAT (A6,'ANTWT=',9F5.1,' /Ant. wt')
 2051 FORMAT (A6,'     ',9F5.1)
 3021 FORMAT (A6,'/ Phases only written to BP table')
 3024 FORMAT (A6,'/ Amplitudes only written to BP table')
 3050 FORMAT (A6,'/ Ch. 0 Avgd: Start, Stop, Inc ',2I5,I4,'  IF=',I3)
 3051 FORMAT (A6,'/ External channel 0 filename: ')
 3060 FORMAT (A6,'/ Data scalar averaged before determining bandpass')
 3080 FORMAT (A6,'BPVER = ',I5,' /Version of BP table written')
      END
      SUBROUTINE BPSEL (IRET)
C-----------------------------------------------------------------------
C   BPSEL will read a multi source data set into a temporary scratch
C   file.  Editing, calibration, averaging and, in the case of VLBA
C   data, shifting may be applied.
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
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IRET, LUN1, LUN2, TVER1, TVER2
      REAL      FRINC(MAXIF), DUM
      CHARACTER TEMP*2, PHC*10
      DOUBLE PRECISION DFOFF(MAXIF)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'BPASS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN1, LUN2 /46, 47/
C-----------------------------------------------------------------------
C                                       Setup
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.GT.0) GO TO 999
      IF ((NVIS.LE.0) .OR. (IRET.LT.0)) THEN
         MSGTXT = 'BPSEL: NO DATA FOUND.  CHECK ADVERBS'
         CALL MSGWRT (8)
         NVIS = 0
         CALL UVGET ('CLOS', DUM, DUM, IRET)
         IRET = 10
         GO TO 999
         END IF
      IF (EXTCH0) THEN
         CALL INITEX ('INIT', IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,2005) IRET
            GO TO 990
            END IF
         END IF
C                                       Message
      WRITE (MSGTXT,2000)
      TEMP = 'CL'
      IF (SINGLE) TEMP = 'SN'
      IF (DOCAL) WRITE (MSGTXT,2001) TEMP, CLUSE
      IF (DOFLAG) WRITE (MSGTXT,2002)
      IF (DOCAL.AND.DOFLAG) WRITE (MSGTXT,2003) TEMP, CLUSE
      CALL MSGWRT (6)
      IF (DIVCH0) THEN
         PHC = ' '
         IF (PHONLY) PHC = 'phase only'
         IF (.NOT.EXTCH0) WRITE (MSGTXT,2004) PHC
         IF (EXTCH0) WRITE (MSGTXT,2006) PHC
         CALL MSGWRT (3)
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
     *   SFREQS, BUFF1, DFOFF, UBUFF, FRINC, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 2000 FORMAT ('Selecting the data')
 2001 FORMAT ('Selecting and calibrating the data with ',A,' vers',I4)
 2002 FORMAT ('Selecting and editing the data')
 2003 FORMAT ('Selecting, editing and calibrating the data with ',A,
     *   ' vers',I4)
 2004 FORMAT ('Dividing the spectral data by channel 0 ',A)
 2005 FORMAT ('BPSEL: ERROR ',I3,' OPENING EXTERNAL CHANNEL 0')
 2006 FORMAT ('Dividing the spectral data by external channel 0 ',A)
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
C                        If.LE.0 then the output file is a /CFILES/
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
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER NAME*48, PRESOU(200)*16
      INTEGER   DISK, CNOSCR, IRET, VOL, LUN, FIND, BIND, LENBU, NIO,
     *   BUFSZ, CNO, IA1, IA2, BO, VO, I, ISIZE, WRKSIZ, GUSE, CVLSOU,
     *   VISNUM, NCOUNT
      PARAMETER (WRKSIZ = 4 * MAXCHA)
      LOGICAL   T, F, SUPRMS, WUVCMP
      REAL      BUFFER(*), WORK(WRKSIZ), CVPARM(10)
      INCLUDE 'BPASS.INC'
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
      CALL RFILL (10, 0.0, CVPARM)
C                                       Create output file if necessary
      IF ((DISK.GT.0) .OR. (CNOSCR.GT.0)) GO TO 30
C                                       Determine size.
         CALL UVSIZE (LREC, NVIS, ISIZE)
C                                       Create scratch file.
         CALL SCREAT (ISIZE, BUFFER, IRET)
         CNOSCR = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) WRITE (MSGTXT,1010)
            IF (IRET.GT.1) WRITE (MSGTXT,1011) IRET
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
C                                       Set up for shifting
      IF (VLBA) THEN
         GUSE = 1
         SUPRMS = T
         WUVCMP = F
         VISNUM = 1
         END IF
C                                       Copy file
      DO 100 I = 1,NVIS
C                                       Read old.
         CALL UVGET ('READ', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
         IF (IRET.LT.0) GO TO 110
         IF (IRET.NE.0) GO TO 999
C                                       Write new
         IF (ACONLY) THEN
            IF (ILOCB.GT.0) THEN
               IA1 = BUFFER(BIND+ILOCB)/256 + 0.1
               IA2 = BUFFER(BIND+ILOCB) - IA1*256 + 0.1
            ELSE
               IA1 = BUFFER(BIND+ILOCA1) + 0.1
               IA2 = BUFFER(BIND+ILOCA2) + 0.1
               END IF
            IF (IA1.NE.IA2) GO TO 100
            END IF
C                                       divide by channel 0
         IF (DIVCH0) THEN
            IF (EXTCH0) THEN
               CALL EXTDIV (BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
               IF (IRET.GT.0) GO TO 999
            ELSE
               CALL DIVCHZ (BUFFER(BIND+NRPARM))
               END IF
            END IF
C                                       Shift VLBA data to remove the
C                                       effects of the lobe-rotation to
C                                       earth centre.
C                                       Do it only if ACONLY. Otherwise
C                                       shift the solution later
C                                       L. Kogan, Jan 2001
C
         IF (VLBA .AND. ACONLY) THEN
            CVLSOU = BUFFER(BIND+KLOCSU)
            CALL DATSHF (DISKIN, CNOIN, BUFFER(BIND),
     *         BUFFER(BIND+NRPARM), WORK, EVN, GUSE, CVLSOU, CVPARM,
     *         SUPRMS, PRESOU, WUVCMP, VISNUM, FRQSEL, CATUV, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1050) IRET
               GO TO 990
               END IF
            VISNUM = VISNUM + 1
            END IF
C                                       Write data
         NIO = 1
         CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET
            GO TO 990
            END IF
         NCOUNT = NCOUNT + 1
 100     CONTINUE
C                                       Flush output
 110  NIO = 0
      CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1060) IRET
         GO TO 990
         END IF
C                                       Close input
      CALL UVGET ('CLOS', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
      IF (EXTCH0) CALL INITEX ('CLOS', IRET)
C                                       Close down shifting area
      IF (VLBA .AND. ACONLY) THEN
         VISNUM = -1
         CALL DATSHF (DISKIN, CNOIN, BUFFER(BIND), BUFFER(BIND+NRPARM),
     *      WORK, EVN, GUSE, CVLSOU, CVPARM, SUPRMS, PRESOU,
     *      WUVCMP, VISNUM, FRQSEL, CATUV, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1070) IRET
            GO TO 990
            END IF
         END IF
C                                       Compress output file.
      NVIS = NCOUNT
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
      WRITE (MSGTXT,1110) NCOUNT
      IF (NCOUNT.GT.0) THEN
         CALL MSGWRT (3)
         IRET = 0
         GO TO 999
      ELSE
         IRET = 1
         END IF
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('BPCOPY: TOO LITTLE DISK SPACE FOR SCRATCH FILE')
 1011 FORMAT ('BPCOPY: ERROR ',I5,' CREATING SCRATCH FILE')
 1020 FORMAT ('BPCOPY: ERROR ',I5,' UPDATING SCRATCH FILE CATBLK')
 1030 FORMAT ('BPCOPY: ERROR',I5,' OPENING OUTPUT FILE')
 1040 FORMAT ('BPCOPY: ERROR',I5,' INIT. OUTPUT FILE')
 1050 FORMAT ('BPCOPY: ERROR',I5,' SHIFTING SPECTRA')
 1060 FORMAT ('BPCOPY: ERROR',I5,' WRITING OUTPUT FILE')
 1070 FORMAT ('BPCOPY: ERROR',I5,' CLOSING SHIFTING ROUTINES')
 1110 FORMAT ('BPCOPY: copied',I10,' vis records for bandpass fitting')
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
      REAL      TEMP, DENOM, AMPD
      INCLUDE 'INCS:PUVD.INC'
      REAL      CHZ(MAXIF*12)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'BPASS.INC'
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
         DO 200 LOOPIF = 1,MAXIFS
            INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-1) * INCIF
            JNDEX = 1 + (LOOPS-1) * JNCS + (LOOPIF-1) * JNCIF
            DENOM = 0.0
            IF (CHZ(JNDEX+2).GT.0.0) DENOM = CHZ(JNDEX) * CHZ(JNDEX) +
     *         CHZ(JNDEX+1) * CHZ(JNDEX+1)
            IF (DENOM.LE.0) THEN
               DO 10 I = 1,NUMFRQ
                  INP = INDEX + (I-1) * INCF
                  VIS(INP) = 0.0
                  VIS(INP+1) = 0.0
                  VIS(INP+2) = 0.0
 10               CONTINUE
            ELSE
               IF (PHONLY) DENOM = SQRT (DENOM)
               DO 100 I = 1,NUMFRQ
                  INP = INDEX + (I-1) * INCF
                  TEMP = VIS(INP)
                  AMPD = TEMP*TEMP + VIS(INP+1)*VIS(INP+1)
                  VIS(INP+2) = DENOM * DENOM * VIS(INP+2) * CHZ(JNDEX+2)
     *               / (DENOM * CHZ(JNDEX+2) + AMPD * VIS(INP+2))
                  VIS(INP)   = (CHZ(JNDEX)*TEMP +
     *               CHZ(JNDEX+1)*VIS(INP+1)) / DENOM
                  VIS(INP+1) = (CHZ(JNDEX)*VIS(INP+1) -
     *               CHZ(JNDEX+1)*TEMP) / DENOM
 100              CONTINUE
               END IF
 200        CONTINUE
 300     CONTINUE
      RETURN
      END
      SUBROUTINE BPMOD (IRET)
C-----------------------------------------------------------------------
C   BPMOD divides the CLEAN model visibilities into the data.
C   If no model is found or a point model is specified then the data
C   is divided by the flux density found in the Source (SU) table.
C   Inputs: from commons
C     NCOMP     R    Number of components to be divided.
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
      INTEGER   IRET, MODEL, METHOD, ISTOKE, DISKO, ISCR, CHAN,
     *   NCHAN, I, IROUND, IIF
      LOGICAL   DOMSG, F, NONAM, WASOME
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INTEGER   BITER(MAXFLD)
      INCLUDE 'BPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSCD.INC'
      REAL RBUF(MAXIF)
      DATA DOMSG, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if multiple sources
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
C                                       Set model and method
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
      IF (CMOD.EQ.'SUBI') MODEL = 3
      METHOD = 0
      IF (CMETH.EQ.'DFT ') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
C                                       Point source parameters
      DOPTMD = ABS (SMODEL(1)).GT.1.0E-20
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
      IF ((MAXIFS.GT.1) .AND. (MODEL.EQ.1) .AND.
     *   (ABS(FACGRD(1)-1.0).GT.1.E-10)) THEN
C                                       number of channels
         NCHAN = CATBLK(KINAX+JLOCF)
C                                       For each IF
         DO 15 IIF = 1,MAXIFS
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
         NCHAN = CATBLK(KINAX+JLOCF) * MAXIFS
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
      REAL      SFLUX(16384), XSFLUX
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'BPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
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
      WRITE (MSGTXT,2000)
      CALL MSGWRT (6)
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
            DOIT = DOIT .OR. (ABS (SFLUX(INDX+I)-1.0).GT.1.0E-10)
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
 2000 FORMAT ('Dividing data by source flux densities')
      END
      SUBROUTINE BPSOL (IRET)
C-----------------------------------------------------------------------
C   BPSOL is the routine which controls BASOLV (which does all
C   the work). The purpose of BPSOL is to determine if the solution
C   can be done in one pass, if not it controls the loop which calls
C   BASOLV and sets up various parameters needed by it.
C   Output:
C     IRET            I   Error code. 0 => All OK
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48, LBPKEY*8
      INTEGER   SCOR, MX1, TNPOL, LCOR, I, J, NANTM1, I1, I2, I1P1,
     *   DISK, LUNP, CVER, MXANT, SCHIF, VBUFSZ, WSIZE, VSIZE, CSIZE
      LONGINT   VRPTR, VIPTR, SPPTR, AWPTR, CRPTR, CIPTR, WRPTR, WIPTR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      REAL      TIMB(MXBASE), VREAL(2), CREAL(2), CIMAG(2), WREAL(2),
     *   WIMAG(2)
      REAL      TIMEC
      DOUBLE PRECISION REFF, CATD(128)
      INTEGER   NN, IBASE
      LOGICAL   T, F, FIRST
      EQUIVALENCE (CATUV, CATD)
      DATA T, F /.TRUE., .FALSE./
      DATA MXANT /MAXANT/, LBPKEY /' '/
C-----------------------------------------------------------------------
      IRET = 0
      LUNI = 16
C                                       check spectral index stuff
      CALL BPSPEC
C                                       Message
      MSGTXT = 'Determining solutions'
      CALL MSGWRT (2)
      IF (SCALAR) THEN
         MSGTXT = 'Using amp-scalar averaging'
         CALL MSGWRT (2)
         END IF
C                                       Setup for amp-phase soln.
      TINTG = SOLINT
      TINTGH = TINTG * 0.5
      NUMBL = (NUMANT * (NUMANT-1)) / 2
C                                       Little trick to fool
C                                       baseline based loops in
C                                       BASOLV. When NUMANT < 3,
C                                       NUMANT > NUMBL.
      IF ((ACONLY) .AND. (NUMBL.EQ.1)) NUMBL = 2
      MODE = 0
      MINNO = XMINNO + 0.1
      MINNO = MAX (2, MINNO)
C                                       Set up for Table writing
      MXBPRN = 0
      LUNBP = 48
      LUNP  = 47
      MX1 = MXENTR
      DO 5 I = 1,MX1
         TIMWRT(I) = -999.D0
 5       CONTINUE
      MX1 = MXLOOP
      DO 10 I = 1, MX1
         TIMPT(I) = 0
 10      CONTINUE
      DO 20 I = 1,MXANT
         DO 19 J = 1, MX1
            ANTWRT(J,I) = F
 19         CONTINUE
 20      CONTINUE
C                                        Set baseline arrays.
      IF (ILOCB.GE.0) THEN
         IBASE = 256
      ELSE
         IBASE = 32768
         END IF
      IF (.NOT. ACONLY) THEN
         NANTM1 = NUMANT - 1
         NBL = 0
         DO 30 I1 = 1, NANTM1
            I1P1 = I1 + 1
            DO 30 I2 = I1P1, NUMANT
               NBL = NBL + 1
               BLCODE(NBL) = I1 * IBASE + I2
               IS(NBL) = I1
               JS(NBL) = I2
 30            CONTINUE
      ELSE
         NBL = 0
         DO 40 I1 = 1, NUMANT
            I2 = I1
            NBL = NBL + 1
            BLCODE(NBL) = I1 * IBASE + I2
            IS(NBL) = I1
            JS(NBL) = I2
 40         CONTINUE
      END IF
C                                        Init. BP file.
      NUMPOL = 1
      IF (NCOR.GT.1) NUMPOL = 2
      IF ((NCOR.GT.1) .AND. (ICOR0.GT.0)) NUMPOL = 1
      NANTBP = NUMANT
      NPOLBP = NUMPOL
C                                        Write the full # IF's in
C                                        the BP table, if IF selection
C                                        is performed then some will
C                                        be blank filled.
      NIFBP  = NUMIF
      NCHNBP = NUMFRQ
      BCHNBP = 1
      NUMSHF = 0
      IF (.NOT.ACONLY) NUMSHF = 1
      IF (ACONLY) NUMSHF = 2
      LOWSHF = 0.0
      DELSHF = 0.0
      CALL FNDEXT ('BP', CATUV, NN)
      CALL CATFIX (DISKIN, CNOIN, 'NOTR')
      CALL BPINI ('WRIT', BPBUFF, DISKIN, CNOIN, BPOVER, CATUV, LUNBP,
     *   IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP, BCHNBP,
     *   NUMSHF, LOWSHF, DELSHF, LBPKEY, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1080) IRET
         GO TO 990
         END IF
C                                       sort order: new file
      IF (BPOVER.GT.NN) THEN
         BPBUFF(43) = 1
         BPBUFF(44) = 5
C                                       sort order unknown after append
      ELSE
         BPBUFF(43) = 0
         BPBUFF(44) = 0
         END IF
C                                       Can't append to polynomial
C                                        BP table.
      CALL CHBLNK (8, 1, LBPKEY, NN)
      IF (NN.NE.0) THEN
         IRET = 9
         WRITE (MSGTXT,1082) BPOVER
         GO TO 990
         END IF
C
      CURBPN = IBPRNO
      WRITE (MSGTXT,1110) BPOVER
      CALL MSGWRT (2)
C                                        Get IF freq. offsets
      CVER = 1
      CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, CVER, CATUV, LUNP,
     *   SCHIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) IRET
         GO TO 990
         END IF
      REFF = CATD(KDCRV+IKLOCF)
      CALL DFILL (SCHIF, 0.D0, CHNSHF)
C                                        Open vis. file
      IF (VISDSK.EQ.0) THEN
         DISK = SCRVOL(VISCNO)
         CALL ZPHFIL ('SC', DISK, SCRCNO(VISCNO), 1, IFILE, IRET)
      ELSE
         DISK = VISDSK
         CALL ZPHFIL ('UV', DISK, VISCNO, 1, IFILE, IRET)
         END IF
      CALL ZOPEN (LUNI, FINDI, DISK, IFILE, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         GO TO 990
         END IF
C                                      allocate memory
      TNPOL = NUMPOL
      IF (NUMPOL.GE.2) TNPOL = 2
      WSIZE = 2 * NUMFRQ * NUMIF
      CSIZE = WSIZE * MAXTEL
      VSIZE = NUMBL * NUMFRQ * MAXIFS * TNPOL
      VSIZE = VSIZE + 1.25 * (NUMFRQ * MAXIFS + NBPS)
      I1 = (4 * VSIZE - 1) / 1024 + 2
      CALL ZMEMRY ('GET ', 'BPSOL ', I1, VREAL, VRPTR, IRET)
      VIPTR = VRPTR + VSIZE
      SPPTR = VIPTR + VSIZE
      AWPTR = SPPTR + VSIZE
      CSIZE = (CSIZE - 1) / 1024 + 2
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'BPSOL ', CSIZE, CREAL, CRPTR,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'BPSOL ', CSIZE, CIMAG, CIPTR,
     *   IRET)
      WSIZE = (WSIZE - 1) / 1024 + 2
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'BPSOL ', WSIZE, WREAL, WRPTR,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'BPSOL ', WSIZE, WIMAG, WIPTR,
     *   IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'MEMORY ALLOCATION FAILS - TRY A BIGGER COMPUTER'
         GO TO 990
         END IF
      JLOOP = 1
C                                       Allow dual polzn data
C                                       to be processed simultaneously
      SCOR = ABS(ICOR0)
      LCOR = SCOR
      IF (NCOR.GE.2) LCOR = SCOR + 1
      IF (ICOR0.EQ.1) LCOR = SCOR
C                                      deal with LL only
      IF (NCOR.EQ.1) THEN
         IF (ABS(ICOR0).GT.1) THEN
            SCOR = 1
            LCOR = 1
            END IF
         END IF
      FIRST = T
      CALL BASOLV (VREAL(1+VRPTR), VREAL(1+VIPTR), VREAL(1+SPPTR), TIMB,
     *   WREAL(1+WRPTR), WIMAG(1+WIPTR), NUMBL, NUMFRQ, MAXIFS, MAXTEL,
     *   TNPOL, SCOR, LCOR, FIRST, CREAL(1+CRPTR),  CIMAG(1+CIPTR),
     *   VREAL(1+AWPTR), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      FIRST = F
C                                       Close BP file and
C                                       uv file.
      CALL TABIO ('CLOS', 0, IBPRNO, TIMEC, BPBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      CALL ZCLOSE (LUNI, FINDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                       Close index file
      IF (DONDX) CALL TABIO ('CLOS', 0, INXRNO, TIMEC, NXBUFF, IRET)
      IRET = 0
      WRITE (MSGTXT,1600) BPREC
      CALL MSGWRT (5)
C                                       Rereference spectra
C                                       VREAL used
C                                       as work arrays
      VBUFSZ = 4 * VSIZE
      CALL BPADJ (VBUFSZ, VREAL(1+VRPTR), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
C                                       If VLBA correlator data then
C                                       BPASS must create a special BP
C                                       table that contains BP entries
C                                       for each antenna shifted by 1/10
C                                       of a channel from the previous
C                                       one. Do it by reading the
C                                       original BP file, copying and
C                                       shifting to a new BP file,
C                                       deleting the original, copying
C                                       the new and deleting the highest
C                                       numbered.
C      IF (VLBA) THEN
C         CALL BPFAN (DISKIN, CNOIN, CATUV, DELRNG, DELINC, MAXSHF,
C     *      STRTSH, ACONLY, BPNORM, BUFF1, IRET)
C         IF (IRET.NE.0) THEN
C            WRITE (MSGTXT,1120) IRET
C            GO TO 990
C            END IF
C         END IF
      CALL ZMEMRY ('FRAL', 'BPSOL ', I1, VREAL, VRPTR, I2)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('BPSOL: ERROR ',I3,' OPENING VISIBILITY FILE')
 1010 FORMAT ('BPSOL: ERROR ',I3,' DETERMINING BANDPASS FUNCTIONS')
 1020 FORMAT ('BPSOL: ERROR ',I3,' CLOSING BANDPASS TABLE')
 1030 FORMAT ('BPSOL: ERROR ',I3,' CLOSING UV FILE')
 1040 FORMAT ('BPSOL: ERROR ',I3,' TIDYING UP BANDPASS TABLE')
 1080 FORMAT ('BPSOL: ERROR ',I3,' INITIALIZING BANDPASS TABLE')
 1082 FORMAT ('BPSOL: BP',I4,' CONTAINS POLYNOMIAL SOLUTIONS:',
     *   ' CANNOT APPEND')
 1090 FORMAT ('BPSOL: ERROR ',I3,' READING CH TABLE')
 1110 FORMAT ('Writing BP table ',I4)
C1120 FORMAT ('BPSOL: ERROR ',I3,' FANNING OUT VLBA BANDPASSES')
 1600 FORMAT ('BPSOL: wrote',I5,' records to the BP table')
      END
      SUBROUTINE BASOLV (VREAL, VIMAG, SPCWT, TIMB, WREAL, WIMAG, LBL,
     *   LCHAN, LIF, LANT, MAXPOL, SCOR, LCOR, FIRST, CREAL, CIMAG,
     *   AWORK, IERR)
C-----------------------------------------------------------------------
C   BASOLV reads thru a data file and performs a least-squares
C   decomposition of the baseline-based bandpasses into antenna-
C   based ones and writes them to a BP table.
C   If the input data are auto-correlation data then they are
C   just averaged and written to the BP table.
C   Input:
C    VREAL(LCHAN,LBL,MAXPOL,LIF)     R    Work array.
C    VIMAG(LCHAN,LBL,MAXPOL,LIF)     R    Work array.
C    AWORK(LCHAN,LBL,MAXPOL,LIF)     R    Work array.
C    SPCWT(LCHAN,LBL,MAXPOL,LIF)     R    Spectral weight array.
C    TIMB(LBL)            R    Array to hold times.
C    WREAL(2,LCHAN,LIF)   R    Work array
C    WIMAG(2,LCHAN,LIF)   R    Work array
C    LBL           I    Max. # of baselines in data.
C    LCHAN         I    Last channel # to process
C    LIF           I    Maximum number of IFs
C    LANT          I    Maximum number of antennas
C    SCOR          I    First polzn # to process
C    LCOR          I    Last polzn # to process
C   From common:
C    SOLINT        R    Solution interval (days).
C    TINTG         R    Integration time (sec)
C    REFANT        I    Ref ant to use.
C    NUMBL         I    Number of baselines
C    NUMTIM        I    Number of time intervals
C    NUMIF         I    Number of IFs
C    PRTLV         I    Print level
C    ANTWT(20)     R    Antenna weights.
C    CATBLK(256)   I    Output catalog header.
C    CATUV(256)    I    Input catalog header.
C    CNOIN         I    Input data cat. #.
C    CNOOUT        I    Output data cat #.
C    DISKIN        I    Input data disk number.
C    DISOUT        I    Output data disk number.
C    JBUFSZ        I    Buffer size.
C    BUFF1(*)      I    Work buffer
C    BUFF2(*)      I    Work buffer. Used for EQUIVALENCEs.
C   Output:
C    CREAL(2,LCHAN,NUMANT,LIF)  R    Solution
C    CIMAG(2,LCHAN,NUMANT,LIF)  R    Solution
C    IERR          I    Return code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C                                       Adjustable array dimensions
      INTEGER   LBL, LCHAN, LIF, LANT, MAXPOL
      INTEGER    WRKSIZ
      PARAMETER (WRKSIZ = 4 * MAXCHA)
      INTEGER   VISNUM, CVLSOU
      REAL      TREAL(MAXCHA), TIMAG(MAXCHA), WORK(WRKSIZ)
      REAL      VREAL(LCHAN,LBL,MAXPOL,LIF),
     *          VIMAG(LCHAN,LBL,MAXPOL,LIF),
     *          SPCWT(LCHAN,LBL,MAXPOL,LIF),
     *          AWORK(LCHAN,LBL,MAXPOL,LIF),
     *          CREAL(2,LCHAN,LANT,LIF),
     *          CIMAG(2,LCHAN,LANT,LIF),
     *          TIMB(LBL),
     *          WREAL(2,LCHAN,LIF),
     *          WIMAG(2,LCHAN,LIF)
      CHARACTER POLSTR(6)*3
      INTEGER   LUNSS, BINDI, J, NIN, IANT, IITEMP, IERR, IRET,
     *   NBLANK, I, IBL, ICOR, IIF, IREF, INDEX, JBL, KK1, KK3, I1, I2,
     *   I3, I4, IDAY, IROUND, SCNSOU, STRPT, POLCNT, ICOFF, SCNSUB,
     *   SUBA, INTNO, NUMINT, ICHAN, IFRQ, ITIME(4), ICMUL, NUMERR,
     *   NPOLL, SCOR, LCOR, IFCNT, BO, VO, VISCNT, IDUM1, IDUM2,
     *   FREQID, IPOL, KIF, KPOL, KERR
      REAL      DELT, CATR(256), TIME(512), WT, ENDTIM, CURTIM, LASTIM,
     *   TIMEX, BANDR4,  CATIN4(128), WTT(512), TIMNOM, SIUSE, SCNTIM,
     *   SCNDT, SCNEND, BLFACT, STTIME, MINTIM, MAXTIM, XXAMP, YYAMP,
     *   LSPEC, LCURVE(3)
      LOGICAL   T, F, JUSRED, REINIT, FIRST, MAKINT, DONHED
      LOGICAL   DONDXT
      DOUBLE PRECISION    X8, TIMEC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'BPASS.INC'
      INCLUDE 'BPASS2.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSOU.INC'
      LOGICAL   GOTANT(MAXANT), ALLFLG
      REAL      WEIGHT(2,MAXIF), WWTS(2,MAXANT,MAXIF),
     *   MWWTS, CLSERR(2,3), TEMP
      INTEGER   CLSCNT(MAXANT)
      EQUIVALENCE (CATUV, CATIN4),        (CATBLK, CATR)
      EQUIVALENCE (TIME, BUFF2(1)),    (WTT, BUFF2(513))
      DATA T,F /.TRUE.,.FALSE./
      DATA LUNSS /25/
      DATA BO /1/, VO /0/
      DATA POLSTR /'RR','LL','I ','XX','YY','ALL'/
C-----------------------------------------------------------------------
C                                        Set up some local variables
      WTIT = XWTIT + 0.5
      WTIT = MAX (0, MIN (3, WTIT))
      NPOLL = LCOR - SCOR + 1
      ICMUL = 1
      ICOFF = 0
      IF (CHNDWT) THEN
         ICMUL = NUMFRQ
         ICOFF = NUMBL
         END IF
      ICOR = SCOR
      REINIT = F
      MINTIM = 1.0E10
      MAXTIM = -1.0E10
      STRPT = ABS(ICOR0)
      IF (ICOR0.GT.0) THEN
         IF (ICOR0.EQ.1) STRPT = 3
         IF (ICOR0.GT.4) STRPT = ICOR0 - 1
      ELSE IF (ICOR0.LE.-5) THEN
         STRPT = SCOR - 2
      ELSE
         IF ((SCOR.GT.1) .AND. (STRPT.EQ.1)) STRPT = 2
         END IF
      IF (NPOLL.EQ.2) STRPT = 6
C                                        First initialize.
      NIN = 1
      VO = 0
      CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC,NIN, JBUFSZ,
     *   BUFF1, BO, BINDI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                        Setup.
      VISCNT = 0
C                                        Clear "got data" flag
      JUSRED = F
C                                       Initialize I/O to INDEX file
C                                       first time through
      IF (FIRST) THEN
         MSGSUP = 32000
         CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, 1, CATUV, IXLUN,
     *      INXRNO, NXKOLS, NXNUMV, IRET)
         MSGSUP = 0
         DONDX = IRET.EQ.0
C                                       store DONDX
         DONDXT = DONDX
         END IF
      INXRNO = 1
C                                       Dummy if no scans
      SCNTIM = -1.0E10
      SCNEND =  1.0E10
      SCNSOU = 0
      SCNSUB = 0
C                                   Read first scan info
      IF (DONDX) THEN
         CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, SCNTIM,
     *      SCNDT, SCNSOU, SCNSUB, IDUM1, IDUM2, FREQID, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       store SCNSOU for BPSHFT
         CVLSOU = SCNSOU
         IF (AVGALL) DONDX = .FALSE.
         END IF
C                                       Add .5 sec to each end
      SCNDT = SCNDT + 1.0E-5
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
 10   NIN = 1
C                                       Update vis. offset.
      VO = VO + VISCNT
C                                       Clear "Got data" flags
      DO 20 KK1 = 1,NUMANT
         GOTANT(KK1) = F
 20      CONTINUE
C                                       Blank/zero solution values
      NBLANK = 2 * MAXIFS * NUMANT * NUMFRQ
      CALL RFILL (NBLANK, FBLANK, CREAL)
      CALL RFILL (NBLANK, FBLANK, CIMAG)
C                                       Do we need to reinitialize
C                                       the uv reading ?
      IF (REINIT) THEN
         NIN = 1
         CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC,NIN, JBUFSZ,
     *      BUFF1, BO, BINDI, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         END IF
C                                       Init. for sol. interval.
      VISCNT = 0
C                                       Serious error commented out
C                                       9/16/02
C     IF (ICOR.GT.SCOR) JUSRED = F
C                                       Zero weights.
      CALL RFILL (2, 0.0, WTT)
      CALL RFILL (2, 0.0, TIME)
C                                       Zero fill VREAL,VIMAG,SPCWT
      KK3 = MAXIFS * 2 * MAXANT
      CALL RFILL (KK3, 0.0, WWTS)
      KK3 = MAXIFS * NUMBL * MAXPOL * NUMFRQ
      CALL RFILL (KK3, 0.0, VREAL)
      CALL RFILL (KK3, 0.0, VIMAG)
      CALL RFILL (KK3, 0.0, SPCWT)
      CALL RFILL (KK3, 0.0, AWORK)
C                                    Read first record (if nec)
C                                       and setup
      IF (.NOT.JUSRED) CALL UVDISK ('READ', LUNI, FINDI, BUFF1,
     *   NIN, BINDI, IERR)
      IF (NIN.LE.0) GO TO 200
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR, 'READ'
         GO TO 990
         END IF
      CURTIM = BUFF1(BINDI+ILOCT)
      IF (CURTIM.GT.MAXTIM) MAXTIM = CURTIM
      IF (CURTIM.LT.MINTIM) MINTIM = CURTIM
      IF (ILOCB.GT.0) THEN
         IITEMP = BUFF1(BINDI+ILOCB) + 0.1
         SUBA = (BUFF1(BINDI+ILOCB) - IITEMP) * 100.0 + 1.5
      ELSE
         SUBA = BUFF1(BINDI+ILOCSA) + 0.1
         END IF
      JUSRED = T
C
      IF (DONDX) THEN
         IF ((CURTIM.GT.SCNEND) .OR. ((SUBA.NE.SCNSUB).AND.
     *      (SCNSUB.NE.0)) .OR.
     *      ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0))) THEN
C                                       Find next index record
 70         IF (INXRNO.LE.NXBUFF(5)) THEN
               CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *            SCNTIM, SCNDT, SCNSOU, SCNSUB, IDUM1, IDUM2,
     *            FREQID, IERR)
               IF (IERR.NE.0) GO TO 999
C                                       store SCNSOU for BPSHFT
               CVLSOU = SCNSOU
C                                       Add .43 sec to each end
               SCNDT = SCNDT + 1.0E-5
               SCNTIM = SCNTIM - 0.5 * SCNDT
               SCNEND = SCNTIM + SCNDT
               SCNDT = MAX (SCNDT, SOLINT)
               IF ((CURTIM.GT.SCNEND) .OR. ((SUBA.NE.SCNSUB).AND.
     *            (SCNSUB.NE.0)) .OR.
     *            ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0))) GO TO 70
            ELSE
               SCNEND = CURTIM + 0.5E-4
               END IF
            END IF
         END IF
      IDAY = CURTIM
      X8 = (CURTIM - IDAY) / TINTG
      TIMNOM = DINT (X8) * TINTG + TINTGH + IDAY
      LASTIM = TIMNOM + SOLINT - TINTGH
C                                      Adjust by 1 sec.
      LASTIM = CURTIM + SOLINT - 1.157407E-5
      TIMNOM = CURTIM
      WTT(1) = 1.0
      TIME(1) = TIMNOM
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
C                                       Load data into array.
C                                       Begin Loop.
C                                       If next point already read,
C                                       skip read.
      IF (JUSRED) GO TO 110
 100  CALL UVDISK ('READ', LUNI, FINDI, BUFF1, NIN, BINDI, IERR)
      IF (NIN.LE.0) GO TO 200
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR, 'READ'
         GO TO 990
         END IF
 110  CURTIM = BUFF1(BINDI+ILOCT)
      IF (CURTIM.GT.MAXTIM) MAXTIM = CURTIM
      IF (CURTIM.LT.MINTIM) MINTIM = CURTIM
C                                        Check for last time.
      IF (CURTIM.GT.LASTIM) GO TO 200
C                                        Check if finished.
      IF (NIN.LE.0) GO TO 200
C                                        Determine baseline code.
      IF (ILOCB.GE.0) THEN
         JBL = BUFF1(BINDI+ILOCB) + 0.1
         I1 = JBL / 256
         I2 = JBL - 256 * I1
      ELSE
         I1 = BUFF1(BINDI+ILOCA1) + 0.1
         I2 = BUFF1(BINDI+ILOCA2) + 0.1
         JBL = 32768*I1 + I2
         END IF
C                                        Look for match.
      DO 120 I = 1,NBL
         IBL = I
         IF (JBL.EQ.BLCODE(I)) GO TO 130
 120     CONTINUE
C                                        Bad baseline code.
      IERR = 1
C                                        No message on AC data
      IF (I2.NE.I1) THEN
          WRITE (MSGTXT,1130) I1, I2, NUMANT
          CALL MSGWRT (6)
          END IF
C                                        Go to next data point
      GO TO 100
C                                       Baseline factors
 130  I1 = IS(IBL)
      I2 = JS(IBL)
      GOTANT(I1) = T
      GOTANT(I2) = T
      BLFACT = ANTWT(I1) * ANTWT(I2)
C                                        Load up the work arrays
C                                        with the data.
C                                        Note that the work arrays
C                                        start at 1,1,1,1 even
C                                        though we may be working
C                                        on IF 2 starting at channel
C                                        64 etc. This causes all
C                                        the confusion later on, but
C                                        is necessary to ensure
C                                        we can deal with all cases.
      ENDTIM = CURTIM
      VISCNT = VISCNT + 1
      IFCNT = 0
      DO 150 IIF = 1,LIF
         IFCNT = IFCNT + 1
         POLCNT = 0
         DO 145 IPOL = SCOR,LCOR
            POLCNT = POLCNT + 1
            DO 140 IFRQ = 1,NUMFRQ
               INDEX = BINDI + NRPARM + (IFRQ-1) * INCF +
     *            (IIF-1) * INCIF + (IPOL-SCOR) * INCS
               WT = BLFACT * BUFF1(INDEX+2)
               IF (BUFF1(INDEX+2).LE.0.0) WT = 0.0
               IF (WT.GT.0.0) THEN
                  VREAL(IFRQ,IBL,POLCNT,IFCNT) =
     *               VREAL(IFRQ,IBL,POLCNT,IFCNT) + BUFF1(INDEX)*WT
                  VIMAG(IFRQ,IBL,POLCNT,IFCNT) =
     *               VIMAG(IFRQ,IBL,POLCNT,IFCNT) + BUFF1(INDEX+1)*WT
C                                       Sum the weights
                  SPCWT(IFRQ,IBL,POLCNT,IFCNT) =
     *               SPCWT(IFRQ,IBL,POLCNT,IFCNT) + WT
                  IF (SCALAR) AWORK(IFRQ,IBL,POLCNT,IFCNT) =
     *               AWORK(IFRQ,IBL,POLCNT,IFCNT) + WT *
     *               SQRT ((BUFF1(INDEX)**2) + (BUFF1(INDEX+1)**2))
                  END IF
 140           CONTINUE
 145        CONTINUE
 150     CONTINUE
      TIMB(IBL) = CURTIM
      GO TO 100
C                                        Marks the end of the
C                                        data averaging section.
 200  JUSRED = T
C                                        End of solution interval.
C                                        Do solution.
C                                        Adjust time to center.
      TIMEC = (STTIME + ENDTIM) * 0.5
      IF (AVGALL) TIMEC = (MINTIM + MAXTIM) * 0.5
      TIME(1) = TIME(1) - TIMEC
      DO 210 J = 1,NBL
         TIMB(J) = TIMB(J) - TIMEC
 210     CONTINUE
C                                       Normalize visibility array
      MWWTS = 0.01
      DO 240 I4 = 1,MAXIFS
         DO 235 I3 = 1,NPOLL
            DO 225 I2 = 1,NUMBL
               DO 220 I1 = 1,NUMFRQ
                  IF (SPCWT(I1,I2,I3,I4).GT.1.0E-20) THEN
                     VREAL(I1,I2,I3,I4) = VREAL(I1,I2,I3,I4)
     *                  / SPCWT(I1,I2,I3,I4)
                     VIMAG(I1,I2,I3,I4) = VIMAG(I1,I2,I3,I4)
     *                  / SPCWT(I1,I2,I3,I4)
                     WWTS(I3,IS(I2),I4) = WWTS(I3,IS(I2),I4) +
     *                  SPCWT(I1,I2,I3,I4)
                     WWTS(I3,JS(I2),I4) = WWTS(I3,JS(I2),I4) +
     *                  SPCWT(I1,I2,I3,I4)
                     END IF
 220              CONTINUE
 225           CONTINUE
            DO 230 I2 = 1,NUMANT
               MWWTS = MAX (MWWTS, WWTS(I3,I2,I4))
 230           CONTINUE
 235        CONTINUE
 240     CONTINUE
      DO 247 I4 = 1,MAXIFS
         DO 246 I3 = 1,NPOLL
            DO 245 I2 = 1,NUMANT
               WWTS(I3,I2,I4) = WWTS(I3,I2,I4) / MWWTS
 245           CONTINUE
 246        CONTINUE
 247     CONTINUE
C                                       Amp-scalar average
      IF ((SCALAR) .OR. (WSCALE)) THEN
         DO 270 I4 = 1,MAXIFS
            DO 260 I3 = 1,NPOLL
               DO 255 I2 = 1,NUMBL
                  IF (SCALAR) THEN
                     DO 250 I1 = 1,NUMFRQ
                        IF (SPCWT(I1,I2,I3,I4).GT.1.0E-20) THEN
                           XXAMP = SQRT ((VREAL(I1,I2,I3,I4)**2) +
     *                        (VIMAG(I1,I2,I3,I4)**2)) *
     *                        SPCWT(I1,I2,I3,I4)
                           YYAMP = 1.0
                           IF (XXAMP.GT.1.0E-20)
     *                        YYAMP = AWORK(I1,I2,I3,I4) / XXAMP
                           VREAL(I1,I2,I3,I4) = VREAL(I1,I2,I3,I4)*YYAMP
                           VIMAG(I1,I2,I3,I4) = VIMAG(I1,I2,I3,I4)*YYAMP
                           END IF
 250                    CONTINUE
                     END IF
                  IF (WSCALE) CALL XPSCAL (VREAL(1,I2,I3,I4),
     *               VIMAG(1,I2,I3,I4), NUMFRQ, CHNSEL(1,1,I4))
 255              CONTINUE
 260           CONTINUE
 270        CONTINUE
         END IF
C                                       Write time if requested
      TIMEX = TIMEC
      CALL TODHMS (TIMEX, ITIME)
      WRITE (MSGTXT,2001) ITIME, BIF, (LIF+BIF-1), 1, NUMFRQ,
     *   POLSTR(STRPT)
      CALL MSGWRT (2)
C                                       Amplitude, phase soln.
C                                       If have AC data then just copy
C                                       averaged data.
      IF (ACONLY) THEN
         MAKINT = F
         DO 300 IIF = 1,MAXIFS
            KIF = BIF + IIF - 1
            DO 295 ICOR = 1, NPOLL
               DO 290 IANT = 1,NUMANT
                  DO 280 I = 1,NUMFRQ
                     IF (SPCWT(I,IANT,ICOR,IIF).LE.0.0) THEN
                        CREAL(ICOR,I,IANT,KIF) = FBLANK
                        CIMAG(ICOR,I,IANT,KIF) = FBLANK
                        IF (DOINTP) MAKINT = T
                     ELSE
                        CREAL(ICOR,I,IANT,KIF) = SQRT (ABS
     *                     (VREAL(I,IANT,ICOR,IIF)))
                        CIMAG(ICOR,I,IANT,KIF) = 0.0
                        END IF
 280                 CONTINUE
 290              CONTINUE
 295           CONTINUE
 300        CONTINUE
      ELSE
C                                       Cross-power section
         NUMERR = 0
         MAKINT = F
         DO 320 IIF = 1,MAXIFS
C                                       IF offset if looping
C                                       outside of BASOLV
            KIF = BIF + IIF - 1
            DO 315 ICOR = 1,NPOLL
               CALL FILL (NUMFRQ, 1, SMCHNW)
               CALL FILL (MAXANT, 0, CLSCNT)
               DONHED = .FALSE.
               DO 310 ICHAN = 1,NUMFRQ
                  CALL RFILL (6, 0.0, CLSERR)
C                                       bugger the weights
                  IF ((CHNDWT) .AND. (APARM(11).LT.-0.5)) THEN
                     IF (APARM(11).GE.-1.5) THEN
                        DO 305 I2 = 1,NUMBL
                           SPCWT(ICHAN,I2,ICOR,IIF) =
     *                        SPCWT(ICHAN,I2,ICOR,IIF) *
     *                        (VREAL(ICHAN,I2,ICOR,IIF)**2 +
     *                        VIMAG(ICHAN,I2,ICOR,IIF)**2)
 305                       CONTINUE
                     ELSE
                        DO 306 I2 = 1,NUMBL
                           TEMP = (VREAL(ICHAN,I2,ICOR,IIF)**2 +
     *                        VIMAG(ICHAN,I2,ICOR,IIF)**2)
                           IF (TEMP.GT.0.0) SPCWT(ICHAN,I2,ICOR,IIF) =
     *                        SPCWT(ICHAN,I2,ICOR,IIF) / TEMP
 306                       CONTINUE
                        END IF
                     END IF
C                                       Channel offset if looping
C                                       outside of BASOLV
                  CALL BNDSET (SOLTYP, IS, JS, VREAL, VIMAG, NUMFRQ,
     *               NUMBL, MAXTEL, MAXPOL, MINNO, ICHAN, IIF, ICOR,
     *               REFANT, SPCWT, PRTLV, IREF, AMPMAX, PHMAX, TIMEC,
     *               NUMERR, CREAL, CIMAG, KIF, ICOR, CLSERR, CLSCNT,
     *               NCLSER)
                  IF ((PRTLV.GT.0) .AND. (CLSERR(1,3).GT.0.0)) THEN
                     CLSERR(1,1) = CLSERR(1,1) / CLSERR(1,3)
                     CLSERR(1,1) = (10.0 ** CLSERR(1,1)) * 100.0 - 100.0
                     CLSERR(1,2) = SQRT (CLSERR(1,2) / CLSERR(1,3))
                     CLSERR(1,2) = (10.0 ** CLSERR(1,2)) * 100.0 - 100.0
                     CLSERR(2,1) = CLSERR(2,1) / CLSERR(1,3) * 57.296
                     CLSERR(2,2) = SQRT (CLSERR(2,2) / CLSERR(1,3)) *
     *                  57.296
                     IF (((AMPAXX.GT.0.0) .AND. (CLSERR(1,1).GT.AMPAXX))
     *                  .OR. ((PHMAXX.GT.0.0) .AND.
     *                  (CLSERR(2,1).GT.PHMAXX)) .OR.
     *                  (CLSERR(2,3).GT.0.0)) THEN
                        IF (.NOT.DONHED) THEN
                           WRITE (MSGTXT,1306) IIF, ICOR
                           CALL MSGWRT (2)
                           MSGTXT = 'Channel   Mean amp &  amp**2    '
     *                        // 'Mean phase & phase**2'
     *                        // '    Excess'
                           CALL MSGWRT (2)
                           DONHED = .TRUE.
                           END IF
                        KK3 = CLSERR(2,3) + 0.5
                        WRITE (MSGTXT,1307) ICHAN, CLSERR(1,1),
     *                     CLSERR(1,2), CLSERR(2,1), CLSERR(2,2), KK3
                        CALL MSGWRT (2)
                        END IF
                     END IF
 310              CONTINUE
C
               KERR = 0
               DO 500 ICHAN = 1,NUMFRQ
                  IF (SMCHNW(ICHAN).EQ.0) KERR = KERR + 1
 500              CONTINUE
               IF (KERR.GT.0) THEN
                  WRITE (MSGTXT,2004) KERR, KIF
                  CALL MSGWRT (4)
                  END IF
               IF ((NUMERR.GT.0) .AND. (DOINTP)) THEN
                  KPOL = ICOR0 + SCOR + 1
                  NUMERR = NUMERR - KERR
                  WRITE (MSGTXT,2002) NUMERR, KIF, KPOL
                  IF (NUMERR.GT.0) CALL MSGWRT (2)
                  WRITE (MSGTXT,2003)
                  CALL MSGWRT (2)
                  NUMERR = 0
                  MAKINT = T
                  END IF
C                                        Store reference antennas used
               BPREF(TIMPT(JLOOP)+1,1) = IREF
               BPREF(TIMPT(JLOOP)+1,2) = IREF
C                                       antenna overflow
               IF (AMPMAX * PHMAX.GT.1.0E-20) THEN
                  KPOL = ABS(ICOR0) + ICOR - 1
                  DO 314 I1 = 1,MAXTEL
                     IF (CLSCNT(I1).GT.0) THEN
                        WRITE (MSGTXT,1310) I1, KIF, KPOL, CLSCNT(I1)
                        CALL MSGWRT (2)
                        END IF
 314                 CONTINUE
                  END IF
 315           CONTINUE
 320        CONTINUE
         REFUSE(IREF) = REFUSE(IREF) + 1
         END IF
C
      REINIT = LCOR.GT.SCOR
      IF (NPOLL.EQ.2) REINIT = F
C                                       End of solution loop
C                                       Write solution record.
      DELT = ENDTIM - STTIME
      IF (ACONLY) DELT = SOLINT
      IF (AVGALL) DELT = MAXTIM - MINTIM
      IF (DELT.LE.0.0) DELT = 0.01
C                                       Determine source info.
      CALL GETSOU (SCNSOU,DISKIN,CNOIN,CATUV,LUNSS,IERR)
      IF ((.NOT. DONDX) .AND. SCNSOU.EQ.0) SCNSOU = IDSOUR
      BANDR4 = BANDW
      IF (SINGLE .AND. (BANDW.EQ.0.0)) BANDR4 = CATIN4(KRCIC+IKLOCF)
      IF (AVGALL) THEN
         SCNSOU = 0
         IF ((DOSPEC.GT.0) .AND. (NCALNO.EQ.1)) SCNSOU = XCALNO(1)
         BANDR4 = CATIN4(KRCIC+IKLOCF)
         END IF
C                                      Loop over antennae
C
C                                      Fill in the BP table
C                                      Transfer data to WREAL, WIMAG
C                                      arrays, involves some reorg.
C                                      C_ arrays have frequency channels
C                                      stored sequentially with nth IF
C                                      following last freq. channel of
C                                      (n-1)th IF.
C                                      W_ arrays have IF stored as a
C                                      third dimension,in the same
C                                      manner as the BP table.
      IF (.NOT.FIRST) IBPRNO = CURBPN
      DO 370 IANT = 1,NUMANT
C                                      Blank fill arrays first
         DO 360 IIF = 1, NIFBP
            WEIGHT(1,IIF) = 0.0
            WEIGHT(2,IIF) = 0.0
            DO 350 I = 1,NCHNBP
               DO 345 IPOL = 1, 2
                  WREAL(IPOL,I,IIF) = FBLANK
                  WIMAG(IPOL,I,IIF) = FBLANK
 345              CONTINUE
 350           CONTINUE
 360        CONTINUE
C                                       If AC data and its all flagged,
C                                       do not write entry
         IF ((ACONLY) .AND. (.NOT.GOTANT(IANT))) GO TO 370
C                                      Fill in if have solution for
C                                      this antenna.
         IF (GOTANT(IANT)) THEN
            VISNUM = 1
            DO 340 IIF = 1,LIF
               KIF = BIF + IIF - 1
               IF (NPOLL.EQ.2) THEN
                  WEIGHT(1,KIF) = WWTS(1,IANT,KIF)
                  WEIGHT(2,KIF) = WWTS(2,IANT,KIF)
               ELSE
                  WEIGHT(1,KIF) = WWTS(1,IANT,KIF)
                  END IF
C
               DO 330  ICOR = 1,NPOLL
                  DO 325 IFRQ = 1,NUMFRQ
                     TREAL(IFRQ) = CREAL(ICOR,IFRQ,IANT,KIF)
                     TIMAG(IFRQ) = CIMAG(ICOR,IFRQ,IANT,KIF)
 325                 CONTINUE
C                                       The shift of the BP solution
C                                       for VLBA data with cross
C                                       correlation included
                  IF (VLBA .AND. (.NOT.ACONLY)) THEN
C                     CVLSOU = SCNSOU
                     CALL BPINTP (TREAL, TIMAG, 1, NUMFRQ, 1, 1, 1,
     *                  ALLFLG)
                     IF (.NOT.ALLFLG) THEN
                        CALL BPSHFT (DISKIN, CNOIN, TIMEX, IANT, KIF,
     *                     TREAL, TIMAG, WORK, CVLSOU, REFANT, ACONLY,
     *                     VISNUM, FRQSEL,CATUV,IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1050) IRET
                           GO TO 990
                           END IF
                        END IF
                     VISNUM = VISNUM + 1
                     END IF
                  DO 327 IFRQ = 1, NUMFRQ
                     WREAL(ICOR,IFRQ,KIF) = TREAL(IFRQ)
                     WIMAG(ICOR,IFRQ,KIF) = TIMAG(IFRQ)
 327                 CONTINUE
 330              CONTINUE
 340           CONTINUE
            END IF
C                                       Normalization
         IF ((BPNORM.GT.0) .OR. (DOSPEC.GT.0)) THEN
            LSPEC = 0.0
            LCURVE(1) = 0.0
            LCURVE(2) = 0.0
            LCURVE(3) = 0.0
            DO 342 J = 1,NCALNO
               IF (XCALNO(J).EQ.SCNSOU) THEN
                  LSPEC = XSPEC(J)
                  LCURVE(1) = XCURVE(1,J)
                  LCURVE(2) = XCURVE(2,J)
                  LCURVE(3) = XCURVE(3,J)
                  END IF
 342           CONTINUE
            IF ((DOSPEC.GT.0) .AND. (LSPEC.EQ.0.0) .AND.
     *         (LCURVE(1).EQ.0.0)) THEN
               WRITE (MSGTXT,1342) SCNSOU
               CALL MSGWRT (7)
               END IF
            CALL NORMBP (WREAL(1,1,BIF), WIMAG(1,1,BIF), LIF, NUMFRQ,
     *         SCOR, LCOR, LSPEC, LCURVE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1150) IERR
               GO TO 990
               END IF
            END IF
C                                       Write table entry
         CALL BPUPDT (NUMFRQ, MAXIFS, TIMEC, DELT, SCNSOU, SUBA, IANT,
     *      BANDR4, WREAL, WIMAG, WEIGHT, SCOR, LCOR, MAKINT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1140) IERR
            GO TO 990
            END IF
         BPREC = BPREC + 1
 370     CONTINUE
C
C                                        Loop back for next time range
      IF (NIN.GT.0) GO TO 10
C                                       restore DONDX
      DONDX = DONDXT
      GO TO 999
C                                        Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('BASOLV: ERROR',I3,' INITING UV FILE')
 1050 FORMAT ('BASOLV: ERROR',I5,' SHIFTING BP table')
 1100 FORMAT ('BASOLV: ERROR',I3,1X,A4,'ING UV FILE')
 1130 FORMAT ('BASOLV: Bad baseline code=',I4,'-',I4,' no. ant.=',I4)
 1140 FORMAT ('BASOLV: ERROR ',I3,' RECEIVED FROM BPUPDT')
 1150 FORMAT ('BASOLV: ERROR ',I3,' RECEIVED FROM NORMBP')
 1306 FORMAT ('Closure error statistics: IF',I3,' correlator',I3)
 1307 FORMAT (I5,F12.2,F10.2,F14.2,F11.2,I10)
 1310 FORMAT ('Antenna',I3,'  IF',I3,'  corr',I2,'  had',I10,
     *   ' excess closure errors')
 1342 FORMAT ('BASOLV WARNING: SPECTRAL INDEX FOR SOURCE',I4,' ZERO')
 2001 FORMAT (' Time=',I4,'/',3I3.2,' IFs=',I3,' - ',I3,
     *   '  Chann=',I4,' - ',I4,' Polzn=',A3)
 2002 FORMAT ('BASOLV:',I5,' errors in the lsq solution for IF # ',2I4)
 2003 FORMAT ('BASOLV: will attempt to interpolate across band')
 2004 FORMAT ('BASOLV:',I5,' flagged channels in IF ',I2)
      END
      SUBROUTINE BPSPEC
C-----------------------------------------------------------------------
C   BPSPEC checks the spectral index parameters in common and fills
C   in when the calsour is well known
C   Output in common
C      XSPEC    R      Spectral index - 0.0 means do none
C      XCURVE   R(3)   Spectral curvature parameters
C-----------------------------------------------------------------------
      INCLUDE 'BPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
C
      INTEGER   XSOUR, NDATES, LXSOUR
      PARAMETER (XSOUR=5, NDATES=17, LXSOUR=6)
C
      INTEGER   IRET, LUNTMP, LUN, I, J, ISRC, ID(3), IDNUM, LSRC,
     *   ICTYPE, JTRIM, LS, NTERM
      REAL      TCOEFF(4,XSOUR), DATES(NDATES), DCOEFF(4,NDATES,3), DD,
     *   W1, W2, LCOEFF(5,LXSOUR), SCOEFF(4,3), TEMP(3)
      CHARACTER KNOSOU(4,XSOUR)*16, LNOSOU(4,LXSOUR)*16, DATE*8,
     *   SNOSOU(3,3)*16
      DOUBLE PRECISION DT
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATH, CATBLK)
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
C                                       default - is source known?
      IF (DOSPEC.GT.0) THEN
         IF ((NSOUWD.GT.1) .AND. (AVGALL)) THEN
            MSGTXT = 'MORE THAN ONE CALSOUR, AVERAGED TOGETHER'
            CALL MSGWRT (6)
            MSGTXT = '*** NO SPECTRAL INDEX CORRECTION'
            CALL MSGWRT (6)
            DOSPEC = 0
         ELSE
            LUN = LUNTMP (1)
            DO 100 LS = 1,NSOUWD
               NCALNO = NCALNO + 1
               XCALNO(LS) = SOUWAN(LS)
               IF (XSPEC(LS).EQ.0.0) THEN
                  CALL GETSOU (SOUWAN(LS), DISKIN, CNOIN, CATUV, LUN,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'SU TABLE PROBLEM,' //
     *                  ' NO SPECTRAL INDEX DEFAULT'
                     CALL MSGWRT (6)
                  ELSE
                     ICTYPE = 1
                     IF (FREQ.LT.0.75D9) ICTYPE = -1
                     ISRC = 0
                     IF (ICTYPE.EQ.1) THEN
                        DO 10 I = 1,3
                           DO 5 J = 1,3
                              IF (SNAME(:JTRIM(SNOSOU(J,I))).EQ.
     *                           SNOSOU(J,I)) ISRC = I + XSOUR
 5                            CONTINUE
 10                        CONTINUE
                        DO 20 I = 1,XSOUR
                           DO 15 J = 1,4
                              IF (SNAME(:JTRIM(KNOSOU(J,I))).EQ.
     *                           KNOSOU(J,I)) ISRC = I
 15                           CONTINUE
 20                        CONTINUE
                     ELSE
                        DO 40 I = 1,LXSOUR
                           DO 30 J = 1,4
                              IF (SNAME(:JTRIM(LNOSOU(J,I))).EQ.
     *                           LNOSOU(J,I)) ISRC = I
 30                           CONTINUE
 40                        CONTINUE
                        END IF
C                                       non-standard source
                     IF (ISRC.LE.0) THEN
                        NTERM = DOSPEC
                        CALL RFILL (3, 0.0, XCURVE(1,LS))
                        CALL FNDSPX (DISKIN, CNOIN, SOUWAN(LS), FRQSEL,
     *                     CATUV, NTERM, TEMP, IRET)
                        IF (IRET.EQ.0) THEN
                           XSPEC(LS) = TEMP(2)
                           IF (NTERM.EQ.2) XCURVE(1,LS) = TEMP(3)
                           END IF
C                                       low frequency
                     ELSE IF (ICTYPE.EQ.-1) THEN
C                                       return wrt 1 GHz, not 150 MHz
                        DT = LOG10 (1.0D3 / 150.0D0)
                        XSPEC(LS) = LCOEFF(2,ISRC) +
     *                     2.D0*DT*LCOEFF(3,ISRC) +
     *                     3.D0*DT*DT*LCOEFF(4,ISRC) +
     *                     4.D0*DT*DT*DT * LCOEFF(5,ISRC)
                        XCURVE(1,LS) = LCOEFF(3,ISRC) +
     *                     3.D0*DT*LCOEFF(4,ISRC) +
     *                     6.D0*DT*DT*LCOEFF(5,ISRC)
                        XCURVE(2,LS) = LCOEFF(4,ISRC) +
     *                     4.D0*DT*LCOEFF(5,ISRC)
                        XCURVE(3,LS) = LCOEFF(5,ISRC)
                        WRITE (MSGTXT,1020) LNOSOU(1,ISRC), XSPEC(LS),
     *                     XCURVE(1,LS), XCURVE(2,LS), XCURVE(3,LS)
                        CALL MSGWRT (3)
C                                       stable ones
                     ELSE IF (ISRC.GT.XSOUR) THEN
                        ISRC = ISRC - XSOUR
                        XSPEC(LS) = SCOEFF(2,ISRC)
                        XCURVE(1,LS) = SCOEFF(3,ISRC)
                        XCURVE(2,LS) = SCOEFF(4,ISRC)
                        XCURVE(3,LS) = 0.0
                        WRITE (MSGTXT,1020) SNOSOU(1,ISRC), XSPEC(LS),
     *                     XCURVE(1,LS), XCURVE(2,LS), XCURVE(3,LS)
                        CALL MSGWRT (3)
C                                       3C286, 3C295 stable
                     ELSE IF ((ISRC.EQ.1) .OR. (ISRC.EQ.5)) THEN
                        XSPEC(LS) = TCOEFF(2,ISRC)
                        XCURVE(1,LS) = TCOEFF(3,ISRC)
                        XCURVE(2,LS) = TCOEFF(4,ISRC)
                        XCURVE(3,LS) = 0.0
                        WRITE (MSGTXT,1020) KNOSOU(1,ISRC), XSPEC(LS),
     *                     XCURVE(1,LS), XCURVE(2,LS), XCURVE(3,LS)
                        CALL MSGWRT (3)
C                                       time variable
                     ELSE IF (ISRC.GT.0) THEN
                        CALL H2CHR (8, 1, CATH(KHDOB), DATE)
                        CALL DATEST (DATE, ID)
                        CALL DAYNUM (ID(1), ID(3), ID(2), IDNUM)
                        DD = ID(1) + IDNUM/365.25
                        LSRC = ISRC - 1
                        IF ((DD.LE.DATES(1)) .OR. (DD.GE.DATES(NDATES)))
     *                     THEN
                           I = NDATES
                           IF (DD.LE.DATES(1)) I = 1
                           XSPEC(LS) = DCOEFF(2,I,LSRC)
                           XCURVE(1,LS) = DCOEFF(3,I,LSRC)
                           XCURVE(2,LS) = DCOEFF(4,I,LSRC)
                           XCURVE(3,LS) = 0.0
                           WRITE (MSGTXT,1020) KNOSOU(1,ISRC),
     *                         XSPEC(LS), XCURVE(1,LS), XCURVE(2,LS),
     *                        XCURVE(3,LS)
                           CALL MSGWRT (3)
C                                       interpolate
                        ELSE
                           DO 50 I = 2,NDATES
                              IF (DD.LT.DATES(I)) THEN
                                 W1 = (DATES(I) - DD) / (DATES(I)
     *                              -DATES(I-1))
                                 W2 = 1.0 - W1
                                 XSPEC(LS) = W2 * DCOEFF(2,I,LSRC) +
     *                              W1 * DCOEFF(2,I-1,LSRC)
                                 XCURVE(1,LS) = W2 * DCOEFF(3,I,LSRC) +
     *                              W1 * DCOEFF(3,I-1,LSRC)
                                 XCURVE(2,LS) = W2 * DCOEFF(4,I,LSRC) +
     *                              W1 * DCOEFF(4,I-1,LSRC)
                                 XCURVE(3,LS) = 0.0
                                 WRITE (MSGTXT,1020) KNOSOU(1,ISRC),
     *                              XSPEC(LS), XCURVE(1,LS),
     *                              XCURVE(2,LS), XCURVE(3,LS)
                                 CALL MSGWRT (3)
                                 GO TO 100
                                 END IF
 50                           CONTINUE
                           END IF
                        END IF
                     END IF
                  END IF
 100           CONTINUE
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (A5,' default spectral index',3F8.4,F5.1)
      END
      SUBROUTINE XPSCAL (XRE, XIM, N, CHNSEL)
C-------------------------------------------------------------------
C   Auto-scale a spectrum to zero mean phase and unit amplitude.
C   Inputs:
C      XRE     R(*)    Real parts of spectrum.
C      XIM     R(*)    Imaginary parts of spectrum.
C      N       I       Dimension of XRE, XIM.
C      CHNSEL  I(3,20) Channel selection array
C-------------------------------------------------------------------
      INTEGER   N, CHNSEL(3,20)
      REAL      XRE(N), XIM(N)
C
      DOUBLE PRECISION DSUMA, DSUMRE, DSUMIM, DPHASE, DCOSP,
     *   DSINP, DFACT
      REAL      XRETMP, XIMTMP, W
      INTEGER   I, M
      INCLUDE 'INCS:DDCH.INC'
C-------------------------------------------------------------------
C                                       Initialization
      DSUMA = 0.0D0
      DSUMRE = 0.0D0
      DSUMIM = 0.0D0
C                                       Sum amp. and phase
      M = 0
      DO 100 I = 1,N
         W = 1.0
         CALL WANTCH (CHNSEL, I, W)
         IF ((XRE(I).NE.FBLANK) .AND. (XIM(I).NE.FBLANK) .AND.
     *      (W.GT.0.0)) THEN
            DSUMA = DSUMA + SQRT (XRE(I) * XRE(I) + XIM(I) * XIM(I))
            DPHASE = ATAN2 (XIM(I), XRE(I))
            DSUMRE = DSUMRE + COS (DPHASE)
            DSUMIM = DSUMIM + SIN (DPHASE)
            M = M + 1
            END IF
 100     CONTINUE
C                                       Normalize
      IF (DSUMA.GT.0.0) THEN
         DFACT = M / DSUMA
         DPHASE = ATAN2 (DSUMIM, DSUMRE)
         DSINP = SIN (DPHASE)
         DCOSP = COS (DPHASE)
C                                       Correct data
         DO 200 I = 1,N
C                                       Scale amplitude
            IF ((XRE(I).NE.FBLANK) .AND. (XIM(I).NE.FBLANK)) THEN
               XRETMP = XRE(I) * DFACT
               XIMTMP = XIM(I) * DFACT
C                                       Rotate phase
               XRE(I) = XRETMP * DCOSP + XIMTMP * DSINP
               XIM(I) = XIMTMP * DCOSP - XRETMP * DSINP
               END IF
 200        CONTINUE
         END IF
C                                       Exit
 999  RETURN
      END
      SUBROUTINE BNDSET (SOLTYP, IS, JS, VREAL, VIMAG, LCHAN, NUMBL,
     *   MAXTEL, MAXPOL, MINNO, ICHAN, IIF, ICOR, REFANT, SPCWT, PRTLV,
     *   IREF, AMPMAX, PHMAX, TIME, NERR, CREAL, CIMAG, KIF, IPOL,
     *   CLSERR, CLSCNT, NCLSER)
C-----------------------------------------------------------------------
C   BNDSET does least squares solutions for phase and optionally
C   amplitude.
C   Input:
C    IS(*)      I    First ant. of baseline numbers
C    JS(*)      I    2nd ant. of baseline numbers
C    VREAL(LCHAN,NUMBL,MAXPOL,LIF) R Real part of visibility array
C    VIMAG(LCHAN,NUMBL,MAXPOL,LIF) R Imag part of visibility array
C    LCHAN      I    Maximum number of frequency channels.
C    NUMBL      I    Maximum number of baselines
C    MAXTEL     I    maximum number of antennas
C    ICHAN      I    Channel number to process in VREAL/VIMAG arrays
C    IIF        I    IF number to process in VREAL/VIMAG arrays
C    ICOR       I    Stokes being solved for in VREAL/VIMAG arrays
C    NUMANT     I    Number of antennas
C    REFANT     I    Reference antenna to use.
C    SPCWT(*)   R    weight array as VREAL
C    PRTLV      I    Print level, .ge. 2 gives some print.
C    TIME       D    Time in days of data, used for labeling.
C    NERR       I    Cumulative error total
C    KIF        I    True IF, used in loading up CREAL/CIMAG
C    IPOL       I    Polzn count used in loading up CREAL/CIMAG
C   Output:
C    CREAL(2,LCHAN,MAXTEL,LIF)   R    Real part of solution
C    CIMAG(2,LCHAN,MAXTEL,LIF)   R    Imag part of solution
C    CLSERR(2,3)                 R    sums closure error
C    CLSCNT(MAXTEL)              I    count of closure failures
C    IREF       I    Reference antenna used
C-----------------------------------------------------------------------
C                                       Adjustable array dimensions
      CHARACTER SOLTYP*4
      INTEGER   LCHAN, NUMBL, MAXTEL, MAXPOL
      INTEGER   IS(*), JS(*), REFANT, ICHAN, IREF, IBL, IANT, IST, NST,
     *   ICOR, KIF, IPOL, IIF, MODE, MINNO, PRTLV, IERR, NERR,
     *   CLSCNT(*), NCLSER, LPRTLV
      REAL      VREAL(LCHAN,NUMBL,MAXPOL,*),
     *          VIMAG(LCHAN,NUMBL,MAXPOL,*),
     *          SPCWT(LCHAN,NUMBL,MAXPOL,*),
     *          CREAL(2,LCHAN,MAXTEL,*),
     *          CIMAG(2,LCHAN,MAXTEL,*),
     *          AMPMAX, PHMAX, CLSERR(2,3)
      DOUBLE PRECISION TIME
      LOGICAL   INIT
      INCLUDE 'INCS:PUVD.INC'
      REAL      GAIN(2,MAXANT), WTBT(MXBASE), GNWORK(2,MXBASE), FFLAST,
     *   FRAC, CONFAC, GAERR(MAXANT), RMS, FFLIM
      INCLUDE 'BPASS2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE INIT
      DATA GAERR /MAXANT * 1.0/
      DATA INIT /.TRUE./
C-----------------------------------------------------------------------
      NST = 1
      MODE = 10*WTIT
      IF ((SOLTYP.EQ.'R') .OR. (SOLTYP.EQ.'L1R') .OR.
     *   (SOLTYP.EQ.'GCOR')) MODE = MODE + 4
      IST = ICOR
C                                       Copy data to XOBS
      DO 50 IBL = 1,NUMBL
         XOBS(1,IBL) = VREAL(ICHAN,IBL,IST,IIF)
         XOBS(2,IBL) = VIMAG(ICHAN,IBL,IST,IIF)
         WTBT(IBL) = SPCWT(ICHAN,IBL,IST,IIF)
 50      CONTINUE
C                                       Blank output
      DO 60 IANT = 1,MAXTEL
         CREAL(IPOL,ICHAN,IANT,KIF) = FBLANK
         CIMAG(IPOL,ICHAN,IANT,KIF) = FBLANK
 60      CONTINUE
      IF ((ICHAN.EQ.1) .AND. (IIF.EQ.1) .AND. (IST.EQ.1)) INIT = .TRUE.
      IF (INIT) IREF = REFANT
      IF (ICHAN.EQ.1) INIT = .TRUE.
      LPRTLV = PRTLV - 10
      FFLAST = 3.0
      FFLIM = 2.0
      CONFAC = 1.0
C                                       amplitude constrained sort of
      IF (SOLTYP(:3).EQ.'GCO') THEN
         CALL NCALC (INIT, XOBS, IS, JS, WTBT, NUMBL, REFANT, MODE,
     *      MINNO, GAERR, CONFAC, GAIN, IREF, LPRTLV, FFLIM, FFLAST,
     *      FRAC, RMS, IERR)
C                                       L1 solution
      ELSE IF (SOLTYP(:2).EQ.'L1') THEN
         CALL GCALC1 (INIT, XOBS, IS, JS, WTBT, NUMBL, REFANT, MODE,
     *      MINNO, GAIN, IREF, LPRTLV, FFLIM, FFLAST, FRAC, RMS, IERR)
C                                       Normal
      ELSE
         CALL GCALC (INIT, XOBS, IS, JS, WTBT, NUMBL, REFANT, MODE,
     *      MINNO, GAIN, IREF, GNWORK, LPRTLV, FFLIM, FFLAST, FRAC, RMS,
     *      IERR)
         END IF
C                                       Handle results
      IF (IERR.NE.0) THEN
         NERR = NERR + 1
         SMCHNW(ICHAN) = 0
C                                       Compute closure errors
      ELSE
         INIT = .FALSE.
         CALL BPCLER (XOBS, IS, JS, WTBT, NUMBL, MAXTEL, GAIN, AMPMAX,
     *      PHMAX, TIME, IIF, IST, ICHAN, IGWORK, PRTLV, CLSERR, CLSCNT,
     *      NCLSER)
C                                       Save results
         DO 150 IANT = 1,MAXTEL
            IF ((GAIN(1,IANT).NE.1.0) .OR. (GAIN(2,IANT).NE.0.0)) THEN
               CREAL(IPOL,ICHAN,IANT,KIF) = GAIN(1,IANT)
               CIMAG(IPOL,ICHAN,IANT,KIF) = GAIN(2,IANT)
               END IF
 150        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE NORMBP (WREAL, WIMAG, LIF, LCHAN, SCOR, LCOR, LSPEC,
     *   LCURVE, IERR)
C-----------------------------------------------------------------------
C   Routine to normalize the amplitude portion of the bandpass function,
C   to ensure the area under the curve is = 1.0. Very useful for VLBI
C   applications where the channel 0 is not formed so easily.
C   Input:
C      LIF     I          Final IF to do: rel. to BIF
C      LCHAN   I          Final channel to do - 1,LCHAN
C                         must be the whole channel range or cannot do
C                         the normalization.
C      SCOR    I          First polzn to do.
C      LCOR    I          Final polzn to do.
C   Input/output:
C      WREAL   R(2,n,m)   Real part of complex bandpass
C                            m IF's, n channels, 2 polzns
C      WIMAG   R(2,n,m)   Imag part of complex bandpass
C                            on output contain the normalized function.
C   Output:
C      IERR    I          Error flag: 0 => OK
C                                     1 => bad channel range
C                                     2 => too many flagged chns.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IERR, LIF, LCHAN, SCOR, LCOR
      REAL      WREAL(2,LCHAN,LIF), WIMAG(2,LCHAN,LIF), LSPEC, LCURVE(3)
C
      INTEGER   IPOL, IFLP, IFRQ, COUNT, IP
      REAL      AMP, AR, AI, TEMP, TR, TI, CATUVR(256), RAMP
      DOUBLE PRECISION FMUL, RMUL
      INCLUDE 'BPASS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATUV, CATUVR)
C-----------------------------------------------------------------------
      IERR = 0
C                                       Check # channels
      IF (NUMFRQ.NE.CATUV(KINAX+KLOCFY)) THEN
         IERR = 1
         WRITE (MSGTXT,1000) 1, NUMFRQ
         GO TO 990
         END IF
C                                       Spectral index:
      IF (LSPEC.NE.0.0) THEN
         DO 50 IFLP = 1,LIF
C                                       ref chan multiplier
            RMUL = LOG10 ((FREQ + FOFF(IFLP))/1.D9)
            RMUL =  RMUL * (LSPEC + RMUL * (LCURVE(1) + RMUL *
     *         (LCURVE(2) + RMUL * LCURVE(3))))
            RMUL = 10.0 ** (-RMUL/2.0D0)
            DO 20 IFRQ = 1,NUMFRQ
C                                       channel multiplier
               FMUL = LOG10 (((FREQ + FOFF(IFLP) +
     *            (IFRQ-CATUVR(KRCRP+KLOCFY)) * FINC(IFLP)) / 1.D9))
               FMUL =  FMUL * (LSPEC + FMUL * (LCURVE(1) + FMUL *
     *            (LCURVE(2) + FMUL * LCURVE(3))))
C                                       scale so ref channel value
C                                       unchanged (was set in SETJY)
               FMUL = (10.0 ** (-FMUL/2.0D0)) / RMUL
               DO 10 IPOL = SCOR,LCOR
                  IP = IPOL - SCOR + 1
                  IF (WREAL(IP,IFRQ,IFLP).NE.FBLANK)
     *               WREAL(IP,IFRQ,IFLP) = WREAL(IP,IFRQ,IFLP)*FMUL
                  IF (WIMAG(IP,IFRQ,IFLP).NE.FBLANK)
     *               WIMAG(IP,IFRQ,IFLP) = WIMAG(IP,IFRQ,IFLP)*FMUL
 10               CONTINUE
 20            CONTINUE
 50         CONTINUE
         END IF
C                                       Amplitude, all channels
      IF (BPNORM.EQ.1) THEN
         DO 150 IPOL = SCOR,LCOR
            IP = IPOL - SCOR + 1
            DO 140 IFLP = 1,LIF
               AMP = 0.0
               COUNT = 0
               DO 110 IFRQ = 1,NUMFRQ
                  IF ((WREAL(IP,IFRQ,IFLP).NE.FBLANK) .AND.
     *               (WIMAG(IP,IFRQ,IFLP).NE.FBLANK)) THEN
                     COUNT = COUNT + 1
                     RAMP = SQRT (WREAL(IP,IFRQ,IFLP)**2 +
     *                  WIMAG(IP,IFRQ,IFLP)**2)
                     AMP = AMP + RAMP
                     END IF
 110              CONTINUE
               IF (COUNT.LT.NUMFRQ) THEN
                  IF ((COUNT/NUMFRQ).GT.3) THEN
                     IERR = 2
                     WRITE (MSGTXT,1010) IPOL, IFLP, COUNT
                     GO TO 990
                     END IF
                  END IF
               TEMP = COUNT
               IF (AMP.GT.0.0) AMP = TEMP / AMP
C                                       Correct the data.
               DO 120 IFRQ = 1,NUMFRQ
                  IF ((WREAL(IP,IFRQ,IFLP).NE.FBLANK) .AND.
     *               (WIMAG(IP,IFRQ,IFLP).NE.FBLANK)) THEN
                     WREAL(IP,IFRQ,IFLP) = WREAL(IP,IFRQ,IFLP) * AMP
                     WIMAG(IP,IFRQ,IFLP) = WIMAG(IP,IFRQ,IFLP) * AMP
                     END IF
                  IF (AMP.EQ.0.0) THEN
                     WREAL(IP,IFRQ,IFLP) = FBLANK
                     WIMAG(IP,IFRQ,IFLP) = FBLANK
                     END IF
 120              CONTINUE
 140           CONTINUE
 150        CONTINUE
C                                       Amplitude only, selected chan
      ELSE IF (BPNORM.EQ.2) THEN
         DO 250 IPOL = SCOR,LCOR
            IP = IPOL - SCOR + 1
            DO 240 IFLP = 1,LIF
               AMP = 0.0
               COUNT = 0
               DO 210 IFRQ = 1,NUMFRQ
                  TEMP = 1.0
                  CALL WANTCH (CHNSEL(1,1,IFLP), IFRQ, TEMP)
                  IF ((TEMP.GT.0.0) .AND.
     *               (WREAL(IP,IFRQ,IFLP).NE.FBLANK) .AND.
     *               (WIMAG(IP,IFRQ,IFLP).NE.FBLANK)) THEN
                     COUNT = COUNT + 1
                     AMP = AMP + SQRT (WREAL(IP,IFRQ,IFLP)**2 +
     *                  WIMAG(IP,IFRQ,IFLP)**2)
                     END IF
 210              CONTINUE
               TEMP = COUNT
               IF (AMP.GT.0.0) AMP = TEMP / AMP
C                                       Correct the data.
               DO 220 IFRQ = 1,NUMFRQ
                  IF ((WREAL(IP,IFRQ,IFLP).NE.FBLANK) .AND.
     *               (WIMAG(IP,IFRQ,IFLP).NE.FBLANK)) THEN
                     WREAL(IP,IFRQ,IFLP) = WREAL(IP,IFRQ,IFLP) * AMP
                     WIMAG(IP,IFRQ,IFLP) = WIMAG(IP,IFRQ,IFLP) * AMP
                     END IF
                  IF (AMP.EQ.0.0) THEN
                     WREAL(IP,IFRQ,IFLP) = FBLANK
                     WIMAG(IP,IFRQ,IFLP) = FBLANK
                     END IF
 220              CONTINUE
 240           CONTINUE
 250        CONTINUE
C                                       Amp & phase, selected chan
      ELSE IF ((BPNORM.EQ.3) .OR. (BPNORM.EQ.4)) THEN
         DO 350 IPOL = SCOR,LCOR
            IP = IPOL - SCOR + 1
            DO 340 IFLP = 1,LIF
               AMP = 0.0
               COUNT = 0
               AR = 0.0
               AI = 0.0
               DO 310 IFRQ = 1,NUMFRQ
                  TEMP = 1.0
                  IF (BPNORM.EQ.3) CALL WANTCH (CHNSEL(1,1,IFLP), IFRQ,
     *               TEMP)
                  IF ((TEMP.GT.0.0) .AND.
     *               (WREAL(IP,IFRQ,IFLP).NE.FBLANK) .AND.
     *               (WIMAG(IP,IFRQ,IFLP).NE.FBLANK)) THEN
                     COUNT = COUNT + 1
                     TEMP = SQRT (WREAL(IP,IFRQ,IFLP)**2 +
     *                  WIMAG(IP,IFRQ,IFLP)**2)
                     AMP = AMP + TEMP
                     IF (TEMP.GT.0.0) THEN
                        AR = AR + WREAL(IP,IFRQ,IFLP)/TEMP
                        AI = AI + WIMAG(IP,IFRQ,IFLP)/TEMP
                        END IF
                     END IF
 310              CONTINUE
               TEMP = COUNT
               AMP = AMP * SQRT (AR*AR + AI*AI)
               IF (AMP.GT.0.0) AMP = TEMP / AMP
C                                       Correct the data.
               DO 320 IFRQ = 1,NUMFRQ
                  IF ((WREAL(IP,IFRQ,IFLP).NE.FBLANK) .AND.
     *               (WIMAG(IP,IFRQ,IFLP).NE.FBLANK)) THEN
                     TR = WREAL(IP,IFRQ,IFLP)
                     TI = WIMAG(IP,IFRQ,IFLP)
                     WREAL(IP,IFRQ,IFLP) = AMP * (TR*AR+TI*AI)
                     WIMAG(IP,IFRQ,IFLP) = AMP * (TI*AR-TR*AI)
                     END IF
                  IF (AMP.EQ.0.0) THEN
                     WREAL(IP,IFRQ,IFLP) = FBLANK
                     WIMAG(IP,IFRQ,IFLP) = FBLANK
                     END IF
 320              CONTINUE
 340           CONTINUE
 350        CONTINUE
C                                       Amp & phase, selected chan
      ELSE IF ((BPNORM.EQ.5) .OR. (BPNORM.EQ.6)) THEN
         DO 450 IPOL = SCOR,LCOR
            IP = IPOL - SCOR + 1
            DO 440 IFLP = 1,LIF
               AMP = 0.0
               COUNT = 0
               AR = 0.0
               AI = 0.0
               DO 410 IFRQ = 1,NUMFRQ
                  TEMP = 1.0
                  IF (BPNORM.EQ.5) CALL WANTCH (CHNSEL(1,1,IFLP), IFRQ,
     *               TEMP)
                  IF ((TEMP.GT.0.0) .AND.
     *               (WREAL(IP,IFRQ,IFLP).NE.FBLANK) .AND.
     *               (WIMAG(IP,IFRQ,IFLP).NE.FBLANK)) THEN
                     COUNT = COUNT + 1
                     TEMP = (WREAL(IP,IFRQ,IFLP)**2 +
     *                  WIMAG(IP,IFRQ,IFLP)**2)
                     AMP = AMP + TEMP
                     IF (TEMP.GT.0.0) THEN
                        TEMP = SQRT (TEMP)
                        AR = AR + WREAL(IP,IFRQ,IFLP)/TEMP
                        AI = AI + WIMAG(IP,IFRQ,IFLP)/TEMP
                        END IF
                     END IF
 410              CONTINUE
               IF (COUNT.GT.0) THEN
                  TEMP = COUNT
                  AMP = SQRT (AMP/TEMP)
                  AR = AR / TEMP
                  AI = AI / TEMP
                  AMP = AMP * SQRT (AR*AR + AI*AI)
                  IF (AMP.GT.0.0) AMP = 1.0 / AMP
                  END IF
C                                       Correct the data.
               DO 420 IFRQ = 1,NUMFRQ
                  IF ((WREAL(IP,IFRQ,IFLP).NE.FBLANK) .AND.
     *               (WIMAG(IP,IFRQ,IFLP).NE.FBLANK)) THEN
                     TR = WREAL(IP,IFRQ,IFLP)
                     TI = WIMAG(IP,IFRQ,IFLP)
                     WREAL(IP,IFRQ,IFLP) = AMP * (TR*AR+TI*AI)
                     WIMAG(IP,IFRQ,IFLP) = AMP * (TI*AR-TR*AI)
                     END IF
                  IF (AMP.EQ.0.0) THEN
                     WREAL(IP,IFRQ,IFLP) = FBLANK
                     WIMAG(IP,IFRQ,IFLP) = FBLANK
                     END IF
 420              CONTINUE
 440           CONTINUE
 450        CONTINUE
         END IF
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NORMBP:CHNS ',I4,' - ',I4,' NOT FULL RANGE, CHECK INPUT')
 1010 FORMAT ('NORMBP:POL ',I2,' IF ',I2,' HAS ',I4,' FLAGGED CHANNELS')
      END
      SUBROUTINE BPUPDT (LCHAN, LIF, TIME, INTERV, SOURID, SUBA, ANT,
     *   BANDW, WREAL, WIMAG, WEIGHT, SCOR, LCOR, MAKINT, IERR)
C-----------------------------------------------------------------------
C  Updates the BP table. It checks to see if any of the table entry
C  has yet been written, if so it inserts the data in the correct
C  place; if not it creates a new table entry. When updating an already
C  existing row the row number IBPRNO should be reset by the calling
C  routine.
C    Inputs:
C     LCHAN        I   Final channel to write
C     LIF          I   Final IF number to write, rel to BIF
C     TIME         D   Center time of record (Days)
C     INTERV       R   Time interval of record (Days)
C     SOURID       I   Source ID number.
C     SUBA         I   Subarray number.
C     ANT          I   Antenna number.
C     BANDW        R   Bandwidth of an individual channel (Hz)
C     WREAL(2,n,m) R   Real part of complex bandpass
C                      m IFS; n channels; 2 polns
C     WIMAG(2,n,m) R   Imag part of complex bandpass
C                      m IFS; n channels; 2 polns
C     WEIGHT(2,*)  R   weights 2 polns by IF
C     SCOR         I   First polarization to write
C     LCOR         I   Final polarization to write
C     MAKINT       L   Error in solution:
C                      interpolation across band will be necessary
C    Inputs from common:
C     BPBUFF(*)    I   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by BPINI or
C                      TABINI.
C     IBPRNO       I   Next entry number to read or write.
C     BPKOLS(MAXBPC) I   The column pointer array in order,
C                        TIME, INTERVAL, SOURID,
C                        SUBARRAY, ANTENNA,
C                        BANDW (of individual channel), IFREQ, FRQID,
C                        REFANT1, REAL1, IMAG1, .. etc for all channels
C                        Following used if 2 polarizations per IF
C                        REFANT2, REAL2, IMAG2.
C     BPNUMV(MAXBPC) I   Element count in each column.
C     CURPBN       I   Current max row number in existing BP table
C     NCHNBP       I   # frq channels in the table
C     CHNSHF(m)    D   Reference frequency for each IF (Hz)
C     FRQSEL       I   Freq id number
C     NIFBP        I   # IF's in BP table
C     PHSONL       L   Store phases only
C     NPOLBP       I   Total # polzns in BP table (used for
C                      initialization)
C     BPREF(*,2)   I   Reference Antenna; one for each poln/time
C                      interval
C    Input/Output in common:
C     ANTWRT(*,*)  L   F if solution for antenna and soln interval
C                      not written yet
C     TIMWRT(*)    D   Time of solution written, needed to check
C                      if have already written this
C     TIMPT(*)     I   Number of solutions written per polzn
C    Output in common:
C     IBPRNO       I   Next solution number.
C     MXBPRN       I   Highest solution number written to date.
C    Output:
C     IERR         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LCHAN, LIF, NUMPLZ, IERR, SOURID, ANT, SUBA,
     *   KOLS(MAXBPC), TIMKOL, SOUKOL, SUBKOL, ANTKL, INTKOL, BWKOL,
     *   IFKOL, FRQKOL, REF1KL, REF2KL, RE1KL, IM1KL, RE2KL, IM2KL,
     *   WT1KL, WT2KL, LOOP, IFLP, NFILL, TIT(4), MXL,
     *   SCOR, LCOR, STKLR1, STKLR2, STKLI1, STKLI2, FBPRNO,
     *   NDATA, I, TIMPT1, TIMPT2
      REAL      INTERV, WREAL(2,LCHAN,LIF), WIMAG(2,LCHAN,LIF),
     *   BANDW, TEMPP, RTIME, WEIGHT(2,LIF)
      DOUBLE PRECISION TIME
      LOGICAL  ALLFLG, MAKINT, NEWSOL
      INCLUDE 'BPASS.INC'
      INCLUDE 'RECRD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCVL.INC'
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), INTKOL),
     *   (KOLS(3), SOUKOL),
     *   (KOLS(4), SUBKOL), (KOLS(5), ANTKL),
     *   (KOLS(6), BWKOL),  (KOLS(7), IFKOL),
     *   (KOLS(8), FRQKOL), (KOLS(9), REF1KL),
     *   (KOLS(10), WT1KL), (KOLS(11), RE1KL),  (KOLS(12), IM1KL),
     *   (KOLS(13), REF2KL), (KOLS(14), WT2KL),
     *   (KOLS(15),RE2KL),  (KOLS(16),IM2KL)
C-----------------------------------------------------------------------
C                                       Check sizes
      IF ((NIFBP.GT.MAXIF) .OR. (NCHNBP.GT.MAXCHA) .OR.
     *   (NPOLBP*NIFBP*NCHNBP.GT.MAXCIF)) THEN
         IERR = 1
         MSGTXT = 'BPUPDT: RECORDS TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
C                                         Set pointers
      NDATA = MAXBPC
      CALL COPY (NDATA, BPKOLS, KOLS)
C                                         NUMPLZ tells us how many
C                                         polzns to write this time,
C                                         but not which.
      NUMPLZ = LCOR - SCOR + 1
C                                         Check if undefined values
C                                         in spectra and interpolate
C                                         if there are
      IF (MAKINT) CALL BPINTP (WREAL, WIMAG, 2, NUMFRQ, MAXIFS, SCOR,
     *   LCOR, ALLFLG)
C                                         Is this a new solution?
      NEWSOL = .TRUE.
      MXL = MXLOOP
      TIMPT2 = 0
      DO 4 I = 1,MXL
         IF (TIMPT(I).GT.TIMPT2) TIMPT2 = TIMPT(I)
 4       CONTINUE
      DO 5 I = 1,TIMPT2
         IF (TIME.EQ.TIMWRT(I)) THEN
            NEWSOL = .FALSE.
            TIMPT1 = I
            TIMPT(JLOOP) = I
            GO TO 6
            END IF
 5       CONTINUE
 6    IF (NEWSOL) THEN
         TIMPT(JLOOP) = TIMPT(JLOOP) + 1
         TIMPT1 = TIMPT(JLOOP)
         TIMWRT(TIMPT(JLOOP)) = TIME
         END IF
C                                         New table entry
      IF ((NEWSOL) .OR. (.NOT.ANTWRT(TIMPT1,ANT))) THEN
         ANTWRT(TIMPT1,ANT) = .TRUE.
         REC8(TIMKOL) = TIME
         RECR4(INTKOL) = INTERV
         RECI4(SOUKOL) = SOURID
         RECI4(SUBKOL) = SUBA
         RECI4(ANTKL) = ANT
         RECR4(BWKOL) = BANDW
         DO 10 IFLP = 1,NIFBP
            REC8(IFKOL) = CHNSHF(IFLP)
            IF (ISVLBA) REC8(IFKOL) = AVDELI(ANT,IFLP)
            IFKOL = IFKOL + 1
 10         CONTINUE
         RECI4(FRQKOL) = FRQSEL
         RECI4(REF1KL) = BPREF(TIMPT(JLOOP),1)
C                                       Blank fill record
         NFILL = NIFBP * NCHNBP
         CALL RFILL (NIFBP, 0.0, RECR4(WT1KL))
         CALL RFILL (NFILL, FBLANK, RECR4(RE1KL))
         CALL RFILL (NFILL, FBLANK, RECR4(IM1KL))
         IF (NPOLBP.GT.1) THEN
            CALL RFILL (NIFBP, 0.0, RECR4(WT2KL))
            CALL RFILL (NFILL, FBLANK, RECR4(RE2KL))
            CALL RFILL (NFILL, FBLANK, RECR4(IM2KL))
            END IF
C                                       Determine starting offset
         STKLR1 = RE1KL + NCHNBP * (BIF - 1)
         STKLI1 = IM1KL + NCHNBP * (BIF - 1)
         IF (NPOLBP.GT.1) THEN
            STKLR2 = RE2KL + NCHNBP * (BIF - 1)
            STKLI2 = IM2KL + NCHNBP * (BIF - 1)
            END IF
C                                       First polarization
         DO 30 IFLP = BIF,EIF
            RECR4(WT1KL+IFLP-1) = WEIGHT(1,IFLP)
            DO 20 LOOP = 1,NUMFRQ
               RECR4(STKLR1) = WREAL(1,LOOP,IFLP)
               RECR4(STKLI1) = WIMAG(1,LOOP,IFLP)
               IF ((RECR4(STKLR1).NE.FBLANK) .AND.
     *            (RECR4(STKLI1).NE.FBLANK)) THEN
                  IF (PHSONL) THEN
                     TEMPP = ATAN2 (RECR4(STKLI1),
     *                  (RECR4(STKLR1)+1.0E-20))
                     RECR4(STKLR1) = 1.0 * COS(TEMPP)
                     RECR4(STKLI1) = 1.0 * SIN(TEMPP)
                  ELSE IF (AMPONL) THEN
                     TEMPP = SQRT (RECR4(STKLI1)** 2 +
     *                  RECR4(STKLR1)**2)
                     RECR4(STKLR1) = TEMPP
                     RECR4(STKLI1) = 0.0
                     END IF
                  END IF
               STKLR1 = STKLR1 + 1
               STKLI1 = STKLI1 + 1
 20            CONTINUE
 30         CONTINUE
         IF (NUMPLZ.EQ.2) THEN
            RECI4(REF2KL) = BPREF(TIMPT(JLOOP),2)
            DO 50 IFLP = BIF,EIF
               RECR4(WT2KL+IFLP-1) = WEIGHT(2,IFLP)
               DO 40 LOOP = 1,NUMFRQ
                  RECR4(STKLR2) = WREAL(2,LOOP,IFLP)
                  RECR4(STKLI2) = WIMAG(2,LOOP,IFLP)
                  IF ((RECR4(STKLR2).NE.FBLANK) .AND.
     *                (RECR4(STKLI2).NE.FBLANK)) THEN
                     IF (PHSONL) THEN
                        TEMPP = ATAN2 (RECR4(STKLI2),
     *                     (RECR4(STKLR2)+1.0E-20))
                        RECR4(STKLR2) = 1.0 * COS(TEMPP)
                        RECR4(STKLI2) = 1.0 * SIN(TEMPP)
                     ELSE IF (AMPONL) THEN
                        TEMPP = SQRT (RECR4(STKLI2)**2 +
     *                     RECR4(STKLR2)**2)
                        RECR4(STKLR2) = TEMPP
                        RECR4(STKLI2) = 0.0
                        END IF
                     END IF
                  STKLR2 = STKLR2 + 1
                  STKLI2 = STKLI2 + 1
 40               CONTINUE
 50            CONTINUE
            END IF
C                                       Determine the current
C                                       highest entry number
         IF (MXBPRN.EQ.0) THEN
            IBPRNO = CURBPN
         ELSE
            IBPRNO = MXBPRN + 1
            END IF
         MXBPRN = IBPRNO
C                                       Process record.
 60      CALL TABIO ('WRIT', 0, IBPRNO, RECR4, BPBUFF, IERR)
         IBPRNO = IBPRNO + 1
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, MXBPRN
            GO TO 990
            END IF
         IF (IERR.LT.0) GO TO 60
         GO TO 999
         END IF
C                                         Entry already exists
      FBPRNO = IBPRNO
 110  CALL TABIO ('READ', 0, IBPRNO, RECR4, BPBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1010) IERR, IBPRNO
         CALL MSGWRT (6)
         MSGTXT = 'TRYING TO MATCH FOLLOWING BP ENTRY :'
         CALL MSGWRT (6)
         RTIME = TIME
         CALL TODHMS (RTIME, TIT)
         WRITE (MSGTXT,2000) TIT
         CALL MSGWRT (6)
         CALL TODHMS (INTERV, TIT)
         WRITE (MSGTXT,2001) TIT
         CALL MSGWRT (6)
         WRITE (MSGTXT,2002) SOURID
         CALL MSGWRT (6)
         WRITE (MSGTXT,2003) SUBA
         CALL MSGWRT (6)
         WRITE (MSGTXT,2004) ANT
         CALL MSGWRT (6)
         WRITE (MSGTXT,2005) BANDW
         CALL MSGWRT (6)
         WRITE (MSGTXT,2006) FRQSEL
         CALL MSGWRT (6)
         WRITE (MSGTXT,2007) BPREF(TIMPT(JLOOP),1)
         GO TO 990
         END IF
      IF (IERR.LT.0) GO TO 110
C                                       sample matches
      IF ((TIME     .EQ.REC8(TIMKOL))   .AND.
     *    (INTERV   .EQ.RECR4(INTKOL)) .AND.
     *    (SOURID   .EQ.RECI4(SOUKOL))   .AND.
     *    (SUBA     .EQ.RECI4(SUBKOL))   .AND.
     *    (ANT      .EQ.RECI4(ANTKL))    .AND.
     *    (BANDW    .EQ.RECR4(BWKOL))  .AND.
     *    (FRQSEL   .EQ.RECI4(FRQKOL))   .AND.
     *    (BPREF(TIMPT(JLOOP),1) .EQ.RECI4(REF1KL))) THEN
C                                         Calculate starting offsets
C                                         in the table array
         STKLR1 = RE1KL + NCHNBP * (BIF-1)
         STKLI1 = IM1KL + NCHNBP * (BIF-1)
C                                       First polarization
         IF (SCOR.LE.1) THEN
            DO 130 IFLP = BIF,EIF
               RECR4(WT1KL+IFLP-1) = WEIGHT(1,IFLP)
C                                       Update the table offsets
               DO 120 LOOP = 1,NUMFRQ
                  RECR4(STKLR1) = WREAL(1,LOOP,IFLP)
                  RECR4(STKLI1) = WIMAG(1,LOOP,IFLP)
                  IF ((RECR4(STKLR1).NE.FBLANK) .AND.
     *               (RECR4(STKLI1).NE.FBLANK)) THEN
                     IF (PHSONL) THEN
                        TEMPP = ATAN2 (RECR4(STKLI1),
     *                     (RECR4(STKLR1)+1.0E-20))
                        RECR4(STKLR1) = 1.0 * COS(TEMPP)
                        RECR4(STKLI1) = 1.0 * SIN(TEMPP)
                     ELSE IF (AMPONL) THEN
                        TEMPP = SQRT (RECR4(STKLI1)**2 +
     *                     RECR4(STKLR1)**2)
                        RECR4(STKLR1) = TEMPP
                        RECR4(STKLI1) = 0.0
                        END IF
                     END IF
                  STKLR1 = STKLR1 + 1
                  STKLI1 = STKLI1 + 1
 120              CONTINUE
 130           CONTINUE
            END IF
C                                         2nd polzn, NUMPLZ maybe 1
C                                         but SCOR can be > 1
         IF ((NUMPLZ.GE.2) .OR. (SCOR.GE.2)) THEN
C                                         Calculate starting offsets
C                                         in the table array
            STKLR2 = RE2KL + NCHNBP * (BIF-1)
            STKLI2 = IM2KL + NCHNBP * (BIF-1)
            RECI4(REF2KL) = BPREF(TIMPT(JLOOP),2)
            DO 150 IFLP = BIF,EIF
               RECR4(WT2KL+IFLP-1) = WEIGHT(2,IFLP)
C                                       Update the table offsets
               DO 140 LOOP = 1,NUMFRQ
                  RECR4(STKLR2) = WREAL(2,LOOP,IFLP)
                  RECR4(STKLI2) = WIMAG(2,LOOP,IFLP)
                  IF ((RECR4(STKLR2).NE.FBLANK) .AND.
     *               (RECR4(STKLI2).NE.FBLANK)) THEN
                     IF (PHSONL) THEN
                        TEMPP = ATAN2 (RECR4(STKLI2),
     *                     (RECR4(STKLR2)+1.0E-20))
                        RECR4(STKLR2) = 1.0 * COS(TEMPP)
                        RECR4(STKLI2) = 1.0 * SIN(TEMPP)
                     ELSE IF (AMPONL) THEN
                        TEMPP = SQRT (RECR4(STKLI2)**2 +
     *                     RECR4(STKLR2)**2)
                        RECR4(STKLR2) = TEMPP
                        RECR4(STKLI2) = 0.0
                        END IF
                     END IF
                  STKLR2 = STKLR2 + 1
                  STKLI2 = STKLI2 + 1
 140              CONTINUE
 150           CONTINUE
            END IF
C                                               Update the entry
         CALL TABIO ('WRIT', 0, IBPRNO, RECR4, BPBUFF, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, IBPRNO
            GO TO 990
            END IF
         IBPRNO = IBPRNO + 1
         GO TO 999
         END IF
C                                               Otherwise search for the
C                                               correct entry
      IBPRNO = IBPRNO + 1
      IF (IBPRNO.LE.MXBPRN) THEN
         GO TO 110
      ELSE
C                                               See if should search
C                                               from beginning. This
C                                               will ensure that the
C                                               table is searched twice.
         IF (FBPRNO.NE.1) THEN
            IBPRNO = CURBPN
            FBPRNO = CURBPN
            GO TO 110
            END IF
C                                       Create new table entry
C         WRITE (MSGTXT,1020) IBPRNO
C         IERR = 3
C         GO TO 990
C                                       Mark table as unsorted
         BPBUFF(43) = 0
         BPBUFF(44) = 0
C                                       Initialize record
         REC8(TIMKOL) = TIME
         RECR4(INTKOL) = INTERV
         RECI4(SOUKOL) = SOURID
         RECI4(SUBKOL) = SUBA
         RECI4(ANTKL) = ANT
         RECR4(BWKOL) = BANDW
         DO 210 IFLP = 1, NIFBP
            REC8(IFKOL) = CHNSHF(IFLP)
            IFKOL = IFKOL + 1
 210        CONTINUE
         RECI4(FRQKOL) = FRQSEL
         RECI4(REF1KL) = BPREF(TIMPT(JLOOP),1)
C                                       Blank fill record
         NFILL = NIFBP * NCHNBP
         CALL RFILL (NFILL, FBLANK, RECR4(RE1KL))
         CALL RFILL (NFILL, FBLANK, RECR4(IM1KL))
         IF (NPOLBP.GT.1) THEN
            CALL RFILL (NFILL, FBLANK, RECR4(RE2KL))
            CALL RFILL (NFILL, FBLANK, RECR4(IM2KL))
            END IF
C                                         Calculate starting offsets
C                                         in the table array
         STKLR1 = RE1KL + NCHNBP * (BIF-1)
         STKLI1 = IM1KL + NCHNBP * (BIF-1)
C                                       First polarization
         IF (SCOR.LE.1) THEN
            DO 230 IFLP = BIF,EIF
               RECR4(WT1KL+IFLP-1) = WEIGHT(1,IFLP)
C                                       Update the table offsets
               DO 220 LOOP = 1,NUMFRQ
                  RECR4(STKLR1) = WREAL(1,LOOP,IFLP)
                  RECR4(STKLI1) = WIMAG(1,LOOP,IFLP)
                  IF ((RECR4(STKLR1).NE.FBLANK) .AND.
     *               (RECR4(STKLI1).NE.FBLANK)) THEN
                     IF (PHSONL) THEN
                        TEMPP = ATAN2 (RECR4(STKLI1),
     *                     (RECR4(STKLR1)+1.0E-20))
                        RECR4(STKLR1) = 1.0 * COS(TEMPP)
                        RECR4(STKLI1) = 1.0 * SIN(TEMPP)
                     ELSE IF (AMPONL) THEN
                        TEMPP = SQRT (RECR4(STKLI1)**2 +
     *                     RECR4(STKLR1)**2)
                        RECR4(STKLR1) = TEMPP
                        RECR4(STKLI1) = 0.0
                        END IF
                     END IF
                  STKLR1 = STKLR1 + 1
                  STKLI1 = STKLI1 + 1
 220              CONTINUE
 230           CONTINUE
            END IF
C                                         2nd polzn, NUMPLZ maybe 1
C                                         but SCOR can be > 1
         IF ((NUMPLZ.GE.2) .OR. (SCOR.GE.2)) THEN
C                                         Calculate starting offsets
C                                         in the table array
            STKLR2 = RE2KL + NCHNBP * (BIF-1)
            STKLI2 = IM2KL + NCHNBP * (BIF-1)
            RECI4(REF2KL) = BPREF(TIMPT(JLOOP),2)
            DO 250 IFLP = BIF,EIF
               RECR4(WT2KL+IFLP-1) = WEIGHT(2,IFLP)
C                                       Update the table offsets
               DO 240 LOOP = 1,NUMFRQ
                  RECR4(STKLR2) = WREAL(2,LOOP,IFLP)
                  RECR4(STKLI2) = WIMAG(2,LOOP,IFLP)
                  IF ((RECR4(STKLR2).NE.FBLANK) .AND.
     *               (RECR4(STKLI2).NE.FBLANK)) THEN
                     IF (PHSONL) THEN
                        TEMPP = ATAN2 (RECR4(STKLI2),
     *                     (RECR4(STKLR2)+1.0E-20))
                        RECR4(STKLR2) = 1.0 * COS(TEMPP)
                        RECR4(STKLI2) = 1.0 * SIN(TEMPP)
                     ELSE IF (AMPONL) THEN
                        TEMPP = SQRT (RECR4(STKLI2)**2 +
     *                     RECR4(STKLR2)**2)
                        RECR4(STKLR2) = TEMPP
                        RECR4(STKLI2) = 0.0
                        END IF
                     END IF
                  STKLR2 = STKLR2 + 1
                  STKLI2 = STKLI2 + 1
 240              CONTINUE
 250           CONTINUE
            END IF
C                                               Update the entry
         CALL TABIO ('WRIT', 0, IBPRNO, RECR4, BPBUFF, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, IBPRNO
            GO TO 990
            END IF
         IBPRNO = IBPRNO + 1
         GO TO 999
         END IF
         GO TO 999
C      GO TO 110
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPUPDT: ERROR ',I3,' FROM TABIO WRITING RECORD',I10)
 1010 FORMAT ('BPUPDT: ERROR ',I3,' FROM TABIO READING RECORD',I10)
 2000 FORMAT ('BPUPDT: RECORD TIME = ',4I4)
 2001 FORMAT ('BPUPDT: RECORD INTERVAL = ',4I4)
 2002 FORMAT ('BPUPDT: RECORD SOURCE ID = ',I4)
 2003 FORMAT ('BPUPDT: RECORD SUBARRAY = ',I4)
 2004 FORMAT ('BPUPDT: RECORD ANTENNA = ',I4)
 2005 FORMAT ('BPUPDT: RECORD BANDWIDTH = ',F15.4)
 2006 FORMAT ('BPUPDT: RECORD FRQSEL = ',I4)
 2007 FORMAT ('BPUPDT: RECORD REFERENCE ANTENNA = ',I4)
      END
      SUBROUTINE BPADJ (WRK1SZ, WORK1, IRET)
C-----------------------------------------------------------------------
C   BPADJ massages the solutions so that interpolation between points
C   is reasonable.
C   Input:
C      WRK1SZ      I     Size of WORK1 in words
C      WORK1(*)    R     large work array
C   Output:
C      IRET   I     Return error code. 0 => OK, otherwise error.
C   NOTE: this routine uses several of the arrays in the D/CSEL.INC
C   commons as work space.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER   KEYS(14)*24
      INTEGER   IRET, ANT, REF, KOLS(14), WRKSIZ, LUN, LOOP, NKEY, LKEY,
     *   KEY(2,2), KEYSUB(2,2), IPNT, MXCNT, MXINDX, REFTMP, NUMSUB,
     *   LIMS1, LIMS2, LOOPSA, IROWNO, WRK1SZ
      HOLLERITH CATUVH(256)
      LOGICAL   T, F, DOIT
      REAL      WRKTIM(500), WORK1(*), SMOTIM(3), FKEY(2,2)
      CHARACTER LBPKEY*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATUV, CATUVH)
      DATA NKEY, LKEY /7,6/
      DATA T, F /.TRUE.,.FALSE./
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA KEYS /'ANTENNA', 'REFANT 1', 'SUBARRAY', 'TIME', 'REAL 1',
     *   'IMAG 1', 'WEIGHT 1', 'ANTENNA ', 'REFANT 2', 'SUBARRAY',
     *   'TIME', 'REAL 2', 'IMAG 2', 'WEIGHT 2'/
C-----------------------------------------------------------------------
C                                       See if any work to be done.
      DOIT = F
      MXCNT = 0
      DO 5 LOOP = 1,NUMANT
         DOIT = DOIT .OR. ((REFUSE(LOOP).GT.0) .AND. (LOOP.NE.REFANT))
         IF (REFUSE(LOOP).GT.MXCNT) MXINDX = LOOP
         IF (REFUSE(LOOP).GT.MXCNT) MXCNT = REFUSE(LOOP)
 5       CONTINUE
      IF (.NOT.DOIT) GO TO 999
C                                       Message
      WRITE (MSGTXT,2000)
      CALL MSGWRT (6)
C                                       If no REFANT specified pick the
C                                       one with the most solutions.
      REFTMP = REFANT
      IF (REFTMP.LE.0) REFTMP = MXINDX
      LUN = 29
C
      CALL BPINI ('READ', BPBUFF, DISKIN, CNOIN, BPOVER, CATUV, LUN,
     *   IROWNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN,
     *   NUMSHF, LOWSHF, DELSHF, LBPKEY, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Set column pointers for sort
      CALL FNDCOL (NKEY, KEYS, LKEY*4, T, BPBUFF, KOLS, IRET)
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         GO TO 980
C                                       Close
 10   CALL TABIO ('CLOS', 0, IROWNO, WORK1, BPBUFF, IRET)
      IF (IRET.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IRET
         GO TO 980
C                                       Sort to time-ant order.
 20   UBUFSZ = UVBFSL * 2
      KEY(1,1) = KOLS(4)
      KEY(2,1) = KOLS(4)
      KEY(1,2) = KOLS(1)
      KEY(2,2) = KOLS(1)
      CALL TABSRT (DISKIN, CNOIN, 'BP', BPOVER, BPOVER, KEY, KEYSUB,
     *   FKEY, BPBUFF, CATUV, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 980
C                                       Find number of subarrays
 30   CALL FNDEXT ('AN', CATUV, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
      LIMS1 = 1
      LIMS2 = NUMSUB
      IF (SUBARR.GT.0) LIMS1 = SUBARR
      IF (SUBARR.GT.0) LIMS2 = SUBARR
C                                       Open for write
      CALL BPINI ('WRIT', BPBUFF, DISKIN, CNOIN, BPOVER, CATUV, LUN,
     *   IROWNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN,
     *   NUMSHF, LOWSHF, DELSHF, LBPKEY, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Set column pointers
      DO 40 LOOP = 1,7
         IPNT = KOLS(LOOP)
         KOLS(LOOP) = BPKOLS(IPNT)
 40      CONTINUE
      IF (NUMPOL.LE.1) GO TO 90
C                                       Second Stokes'
      CALL FNDCOL (NKEY, KEYS(NKEY+1), LKEY*4, T, BPBUFF, KOLS(NKEY+1),
     *   IRET)
      IF (IRET.EQ.0) GO TO 50
         WRITE (MSGTXT,1000) IRET
         GO TO 980
 50   DO 60 LOOP = 8,14
         IPNT = KOLS(LOOP)
         KOLS(LOOP) = BPKOLS(IPNT)
 60      CONTINUE
C                                       Smoothing times
 90   SMOTIM(1) = 1.0E-6
      SMOTIM(2) = 1.0E-6
      SMOTIM(3) = 1.0E-6
      WRKSIZ = WRK1SZ / (NUMFRQ * NUMIF * NUMPOL)
      WRKSIZ = MIN (500, WRKSIZ)
C                                       Loop over subarrays
      DO 200 LOOPSA = LIMS1,LIMS2
C                                       Loop over reference antennas
C                                       used.
         REF = REFTMP
         DO 100 LOOP = 1,NUMANT
         IF ((REFUSE(LOOP).LE.0) .OR. (LOOP.EQ.REF)) GO TO 100
               ANT = LOOP
C                                       First Stokes'
               CALL BNDREF (ANT, REF, LOOPSA, KOLS(1), BPBUFF, NUMFRQ,
     *            NUMIF, WRKSIZ, WRKTIM, WORK1, IRET)
               IF (IRET.GT.0) GO TO 990
               IF (NUMPOL.LE.1) GO TO 100
C                                       Second Stokes'
               CALL BNDREF (ANT, REF, LOOPSA, KOLS(NKEY+1), BPBUFF,
     *            NUMFRQ, NUMIF, WRKSIZ, WRKTIM, WORK1, IRET)
               IF (IRET.GT.0) GO TO 990
 100           CONTINUE
 200     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, IROWNO, WORK1, BPBUFF, IRET)
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I5,' FINDING BP TABLE COLUMNS')
 1010 FORMAT ('TABIO ERROR ',I5,' CLOSING BP TABLE')
 1020 FORMAT ('TABSRT ERROR ',I5,' SORTING BP TABLE')
 1990 FORMAT ('ERROR OCCURED IN BPADJ')
 2000 FORMAT ('Adjusting solutions to a common reference antenna')
      END
      SUBROUTINE BNDREF (ANT, REFAN, SUB, KOLS, BUFFER, NUMFRQ, NUMIF,
     *   MAXTIM, WRKTIM, WORK1, IRET)
C-----------------------------------------------------------------------
C   Subroutine to adjust the reference antenna in a uv data file.  The
C   Table is first read to find all data relating ANT and REFAN.  These
C   data are then smoothed and the resulting ANT-REFAN are used in a
C   second pass through the table to adjust data using ANT as a
C   reference antenna to REFAN as the reference antenna.
C        Several work arrays are passed which are used for storing,
C   smoothing and interpolating data.
C        The table should already be open and BUFFER should be the
C   buffer used by TABINI (or other table opening routines).
C   Input:
C    ANT             I    Old reference antenna
C    REFAN           I    New reference antenna
C    SUB             I    Subarray desired
C    NUMFRQ          I    No. freq channels
C    NUMIF           I    No. IFs
C    KOLS(7)         I    Array of TABIO column pointers in order:
C                         antenna, ref. antenna, subarray, time,
C                         real, imag.
C    BUFFER(*)       I    Buffer for TABIO use; table must already be
C                         open
C    MAXTIM          I    Maximum number of times (dim of WRKTIM etc)
C   Output:
C    WRKTIM(*)       R    Work array - size MAXTIM
C    WORK1           R    Large work array MAXTIM * #chan #IF #Pol
C    IRET            I    Return code 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   ANT, REFAN, SUB, KOLS(7), BUFFER(*), MAXTIM, IRET
      REAL      WORK1(MAXTIM,*), WRKTIM(*)
C                                       Adjustable array dimension
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMTIM, INFRQ
      INTEGER   IRCODE, LKOLS(7), ANTKOL, REFKOL, SUBKOL, TIMKOL, REKOL,
     *   IMKOL, WTKOL, NUMFRQ, NUMIF, IIF, IOFF, I, NUMREC, LOOPR,
     *   IPNT1, IPNT2
      REAL      RE, IM, PH1, PH2,
     *   TIME1, TIME2, TIME, WT1, WT2, TRE, TIM, PHASE
      DOUBLE PRECISION  TIMOFF
      LOGICAL T, F, DO1, DO2, DO3
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RECRD.INC'
      EQUIVALENCE (LKOLS(1), ANTKOL), (LKOLS(2), REFKOL),
     *   (LKOLS(3), SUBKOL), (LKOLS(4), TIMKOL),
     *   (LKOLS(5), REKOL), (LKOLS(6), IMKOL), (LKOLS(7), WTKOL)
      DATA IRCODE /0/
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check sizes
      IF ((NUMIF.GT.MAXIF) .OR. (NUMFRQ.GT.MAXCHA) .OR.
     *   (NUMIF*NUMFRQ.GT.MAXCIF)) THEN
         IRET = 1
         MSGTXT = 'BNDREF: RECORDS TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
      INFRQ = NUMFRQ
C                                       Get column pointers
      CALL COPY (7, KOLS, LKOLS)
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
C                                       Loop thru table referring ANT
C                                       to REFAN.
      NUMTIM = 0
      DO 100 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECI4, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         IF (RECI4(SUBKOL).NE.SUB) GO TO 100
         IF ((RECI4(ANTKOL).NE.ANT) .AND. (RECI4(ANTKOL).NE.REFAN))
     *      GO TO 100
         IF ((RECI4(REFKOL).NE.ANT) .AND. (RECI4(REFKOL).NE.REFAN))
     *      GO TO 100
         IF (RECI4(ANTKOL).EQ.RECI4(REFKOL)) GO TO 100
         DO 10 IIF = 1,NUMIF
            IF ((RECR4(WTKOL+IIF-1).GT.0.0) .AND.
     *         (RECR4(WTKOL+IIF-1).NE.FBLANK)) GO TO 20
 10         CONTINUE
         GO TO 100
 20      IF (NUMTIM.GE.MAXTIM) GO TO 100
         NUMTIM = NUMTIM + 1
         IF (NUMTIM.EQ.1) TIMOFF = REC8(TIMKOL)
         WRKTIM(NUMTIM) = REC8(TIMKOL) - TIMOFF
C                                       Loop over spectrum
         DO 90 IIF = 1, NUMIF
            IOFF = NUMFRQ * (IIF-1)
            DO 80 I=1, NUMFRQ
               IF ((RECR4(WTKOL+IIF-1).LE.0.0) .OR.
     *            (RECR4(WTKOL+IIF-1).EQ.FBLANK) .OR.
     *            (RECR4(REKOL+I-1+IOFF).EQ.FBLANK)) THEN
                  WORK1(NUMTIM,I+IOFF) = FBLANK
C                                       REFAN is reference ant
               ELSE IF (RECI4(REFKOL).NE.ANT) THEN
                  WORK1(NUMTIM,I+IOFF) = ATAN2 (RECR4(IMKOL+I-1+IOFF),
     *               RECR4(REKOL+I-1+IOFF)+1.0E-20)
C                                       ANT is reference ant
               ELSE
                  WORK1(NUMTIM,I+IOFF) = -ATAN2 (RECR4(IMKOL+I-1+IOFF),
     *               RECR4(REKOL+I-1+IOFF)+1.0E-20)
                  END IF
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
      IF (NUMTIM.LE.0) GO TO 999
C                                       Set up for interpolation
      IPNT1 = 1
      IPNT2 = 2
      TIME1 = WRKTIM(1)
      TIME2 = WRKTIM(2)
C                                       Loop thru table changing any
C                                       data with ref=ANT to ref=REFAN
      DO 200 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECI4, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         IF (RECI4(SUBKOL).NE.SUB) GO TO 200
         IF (RECI4(REFKOL).NE.ANT) GO TO 200
         RECI4(REFKOL) = REFAN
C                                       Interpolate
         TIME = REC8(TIMKOL) - TIMOFF
         DO1 = F
         DO2 = F
         DO3 = F
 130     IF (TIME.LE.TIME1) GO TO 150
         IF (TIME.LT.TIME2) GO TO 160
         IF (IPNT2.GE.NUMTIM) GO TO 140
C                                       Shift in interpolation arrays
         IPNT1 = IPNT1 + 1
         TIME1 = WRKTIM(IPNT1)
         IPNT2 = IPNT2 + 1
         TIME2 = WRKTIM(IPNT2)
         GO TO 130
C                                       After last time
 140     WT1 = 0.0
         DO1 = T
         GO TO 170
C                                       Before first time
 150     WT1 = 1.0
         DO2 = T
         GO TO 170
C                                       Between entries
 160     WT1 = 1.0 - ((TIME-TIME1) / (TIME2-TIME1))
         DO3 = T
C                                       Interpolate
 170     WT2 = 1.0 - WT1
C                                       Loop over spectrum
         DO 180 IIF = 1, NUMIF
            IOFF = NUMFRQ * (IIF-1)
            DO 175 I = 1, NUMFRQ
               TRE = RECR4(REKOL+I-1+IOFF)
               IF (TRE.EQ.FBLANK) GO TO 175
               PH1 = WORK1(IPNT1,I+IOFF)
               PH2 = WORK1(IPNT2,I+IOFF)
               IF ((DO1) .AND. (PH2.NE.FBLANK)) PH1 = 0.0
               IF ((DO2) .AND. (PH1.NE.FBLANK)) PH2 = 0.0
               IF (PH1.EQ.FBLANK) THEN
                  PHASE = PH2
               ELSE IF (PH2.EQ.FBLANK) THEN
                  PHASE = PH1
               ELSE
                  PHASE = PH1 * WT1 + PH2 * WT2
                  END IF
               IF (PHASE.NE.FBLANK) THEN
                  RE = COS (PHASE)
                  IM = SIN (PHASE)
                  TIM = RECR4(IMKOL+I-1+IOFF)
                  RECR4(REKOL+I-1+IOFF) = TRE*RE - TIM*IM
                  RECR4(IMKOL+I-1+IOFF) = TRE*IM + TIM*RE
               ELSE
                  RECR4(REKOL+I-1+IOFF) = FBLANK
                  RECR4(IMKOL+I-1+IOFF) = FBLANK
                  END IF
 175           CONTINUE
 180        CONTINUE
C                                       Rewrite record
         CALL TABIO ('WRIT', IRCODE, LOOPR, RECI4, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 900
 200     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET, ANT, REFAN
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('BNDREF: TABIO ERROR',I3,' REREFERENCING ANT ',I3,' TO ',
     *   I3)
      END
      SUBROUTINE BPINTP (WREAL, WIMAG, NP, LCHAN, LIF, SCOR, LCOR,
     *   ALLFLG)
C-----------------------------------------------------------------------
C  Routine to interpolate across flagged channels in a complex
C  spectrum. A simple linear interpolation is performed.
C
C  Inputs:
C    WREAL(p,n,m)   R         Real part of complex bandpass
C                             m IF's, n channels, p polzns
C    WIMAG(p,n,m)   R         Imag part of complex bandpass
C                             m IF's, n channels, p polzns
C    NP             I         Number polarizations
C    LIF            I         Final IF number
C    LCHAN          I         Final channel number
C    SCOR           I         First polarization
C    LCOR           I         Final polarization
C
C  Outputs:
C    WREAL(2,n,m)   R         Interpolated real part of bandpass
C    WIMAG(2,n,m)   R         Interpolated imag part of bandpass
C    ALLFLG         L         If .TRUE. then whole spectrum flagged
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NP, LCHAN, LIF
      REAL      WREAL(NP,LCHAN,LIF), WIMAG(NP,LCHAN,LIF),
     *   WT1, WT2, V1R, V2R, V1I, V2I
      INTEGER   SCOR, LCOR, NUMPOL, NUMERR(MAXIF,2), I, IFLP, LOOP, IP,
     *   SICHAN, LICHAN, IPOL, NINTP
      LOGICAL   ALLFLG, INTERP, FOUBLK, T, F, FIRST(MAXIF,2),
     *   LAST(MAXIF,2)
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       Define local variables
      NUMPOL = LCOR - SCOR + 1
      DO 10 I = 1,LIF
         NUMERR(I,1) = 0
         NUMERR(I,2) = 0
         FIRST(I,1) = F
         FIRST(I,2) = F
         LAST(I,1) = F
         LAST(I,2) = F
 10      CONTINUE
      INTERP = F
      ALLFLG = T
C                                       Determine if interpolation
C                                       necessary
      DO 200 IFLP = 1,LIF
         DO 100 LOOP = 1, LCHAN
            IF (NUMPOL.EQ.1 .AND. SCOR.GT.1) GO TO 50
            IF ((WREAL(1,LOOP,IFLP).EQ.FBLANK) .OR.
     *          (WIMAG(1,LOOP,IFLP).EQ.FBLANK)) THEN
               NUMERR(IFLP,1) = NUMERR(IFLP,1) + 1
               IF (LOOP.EQ.1) FIRST(IFLP,1) = T
               IF (LOOP.EQ.LCHAN) LAST(IFLP,1) = T
               INTERP = .TRUE.
               END IF
 50         IF (NUMPOL.LT.2 .AND. SCOR.LT.2) GO TO 100
            IF ((WREAL(2,LOOP,IFLP).EQ.FBLANK) .OR.
     *          (WIMAG(2,LOOP,IFLP).EQ.FBLANK)) THEN
               NUMERR(IFLP,2) = NUMERR(IFLP,2) + 1
               IF (LOOP.EQ.1) FIRST(IFLP,2) = T
               IF (LOOP.EQ.LCHAN) LAST(IFLP,2) = T
               INTERP = .TRUE.
               END IF
 100        CONTINUE
 200     CONTINUE
C                                       No interpolation
      IF (.NOT.INTERP) THEN
         ALLFLG = .FALSE.
         GO TO 999
         END IF
C                                       Totally flagged ?
      DO 300 IFLP = 1,LIF
         DO 300 I = SCOR, LCOR
            IF (NUMERR(IFLP,I).NE.LCHAN) ALLFLG = .FALSE.
 300        CONTINUE
      IF (ALLFLG) GO TO 999
C                                       Special case, if bad channel
C                                       is first or last of whole
C                                       BP spectrum then just replace
C                                       with nearest value.
      DO 320 IFLP = 1,LIF
         DO 310 I = 1, SCOR, LCOR
            IF ((NUMERR(IFLP,I).GT.1)) THEN
               FIRST(IFLP,I) = F
               LAST(IFLP,I) = F
               END IF
 310           CONTINUE
 320        CONTINUE
C                                       Select range to interpolate
C                                       over - this could be done
C                                       multiple times
      DO 500 IFLP = 1,LIF
         DO 500 IPOL = SCOR,LCOR
            IP = IPOL - SCOR + 1
C                                       Need to do this IF?
            IF (NUMERR(IFLP,IP).EQ.0) GO TO 500
C                                       Check for special case
            IF (FIRST(IFLP,IP)) THEN
               WREAL(IP,1,IFLP) = WREAL(IP,2,IFLP)
               WIMAG(IP,1,IFLP) = WIMAG(IP,2,IFLP)
               END IF
            IF (LAST(IFLP,IP)) THEN
               WREAL(IP,LCHAN,IFLP) = WREAL(IP,(LCHAN-1),IFLP)
               WIMAG(IP,LCHAN,IFLP) = WIMAG(IP,(LCHAN-1),IFLP)
               END IF
            SICHAN = 0
            LICHAN = 0
 400        FOUBLK = F
            DO 410 I = 1, LCHAN
               IF ((WREAL(IP,I,IFLP).EQ.FBLANK  .OR.
     *            WIMAG(IP,I,IFLP).EQ.FBLANK) .AND.
     *            (I.GT.LICHAN)              .AND.
     *            (.NOT. FOUBLK)) THEN
                     FOUBLK = T
                     SICHAN = I - 1
                     END IF
               IF ((WREAL(IP,I,IFLP).NE.FBLANK  .AND.
     *            WIMAG(IP,I,IFLP).NE.FBLANK) .AND.
     *            (I.GT.SICHAN)              .AND.
     *            FOUBLK) THEN
                     LICHAN = I
                     GO TO 420
                     END IF
 410           CONTINUE
            IF (.NOT.FOUBLK) GO TO 450
            LICHAN = LCHAN + 1
C                                       # channels to interpolate
 420        NINTP = LICHAN - SICHAN - 1
            IF (NINTP.LE.0) GO TO 440
C                                       Interpolate
            IF (SICHAN.GT.0) THEN
               V1R = WREAL (IP,SICHAN,IFLP)
               V1I = WIMAG (IP,SICHAN,IFLP)
            ELSE
               V1R = 0.0
               V1I = 0.0
               END IF
            IF (LICHAN.LE.LCHAN) THEN
               V2R = WREAL (IP,LICHAN,IFLP)
               V2I = WIMAG (IP,LICHAN,IFLP)
            ELSE
               V2R = 0.0
               V2I = 0.0
               END IF
            DO 430 I = 1, NINTP
               IF (SICHAN.LE.0) THEN
                  WT2 = 1.0
               ELSE IF (LICHAN.GT.LCHAN) THEN
                  WT2 = 0.0
               ELSE
                  WT2 = I / (NINTP + 1.0)
                  END IF
               WT1 = 1.0 - WT2
               WREAL(IP,(I+SICHAN),IFLP) = WT1*V1R + WT2*V2R
               WIMAG(IP,(I+SICHAN),IFLP) = WT1*V1I + WT2*V2I
 430           CONTINUE
C                                       Loop back ?
 440        IF (LICHAN.LT.LCHAN) GO TO 400
 450        CONTINUE
 500        CONTINUE
C                                       Finish
 999  RETURN
      END
      SUBROUTINE CALAP (BUFF1, BUFF2, AMP, PHASE)
C-----------------------------------------------------------------------
      REAL      BUFF1, BUFF2, AMP, PHASE
C-----------------------------------------------------------------------
      AMP = SQRT (BUFF1*BUFF1 + BUFF2*BUFF2)
      PHASE = 57.296 * ATAN2 (BUFF2, (BUFF1+1.0E-10))
C
 999  RETURN
      END
      SUBROUTINE BPCLER (VOBS, IS, JS, WT, NUMBL, NUMANT, GAIN, AMPMAX,
     *   PHMAX, TIME, IF, IST, ICHAN, COUNT, PRTLV, CLSERR, CLSCNT,
     *   NCLSER)
C-----------------------------------------------------------------------
C   BPCLER computes antenna closure errors based on residuals from
C   a gain model.
C      If AMPMAX and PHMAX are greater than 0 then any values exceeding
C   these limits will be printed.
C   Inputs:
C    IS(*)       I    Array of first antenna numbers.
C    JS(*)       I    Array of second antenna numbers.
C    WT(*)       R    Array of visibility baseline weights.
C    NUMBL       I    Number of observations (baselines)
C    NUMANT      I    Number of antennas.
C    GAIN(2,*)   R    Antenna gains to be applied (real, imaginary)
C    AMPMAX      R    If amplitude closure errors exceed
C                     AMPMAX they will be printed, 0=>no test.
C    PHMAX       R    If phase closure errors exceed PHMAX
C                     they will be printed, 0=>no test.
C    TIME        D    Time in days of data, used for labeling.
C    IF          I    IF number used for labeling only.
C    IST         I    Stokes parameter of soln. R,L,R,L.
C    ICHAN       I    Channel number
C    PRTLV       I    Print level. > 2 causes the closure errors to be
C                     reported.
C   Input/Output:
C    VOBS(2,*)   R    Normalized visibility (real, imaginary)
C                     Zero value assumed invalid on input.
C                     On return real part is replaced with the phase
C                     difference.
C    COUNT(*)    I    A work array used for the counts for each antenna,
C                     must be at least MAXANT in size.
C    CLSERR(2,3) R    Array containing closure error accumulation for
C                     each channel
C    CLSCNT(*)   I    Count of overflows by antenna
C-----------------------------------------------------------------------
      REAL      VOBS(2,*), WT(*), GAIN(2,*), AMPMAX, PHMAX,
     *   CLSERR(2,3)
      INTEGER   IS(*), JS(*), NUMBL, NUMANT, COUNT(*), IF, IST, PRTLV,
     *   ICHAN, CLSCNT(*), NCLSER
      DOUBLE PRECISION TIME
C
      CHARACTER POL(4)*4
      INTEGER   LOOP, II, JJ, NPRT, ID, IH, IM, IROUND, BLPRT(4,3),
     *   I
      LOGICAL   DOLPRT, DOCLOS
      REAL      ZR, ZI, ZZR, ZZI, SEC, TMTEMP, PHSQ, ERRA, ERRP
      INCLUDE 'INCS:PUVD.INC'
      REAL      SUMWT(MAXANT)
      INCLUDE 'INCS:DMSG.INC'
      DATA POL /'Rpol','Lpol','Rpol','Lpol'/
C-----------------------------------------------------------------------
      DOLPRT = .TRUE.
      DOCLOS = AMPMAX * PHMAX.GT.1.0E-20
C                                       Label for any closure errors:
      IF (DOCLOS) THEN
         ID = TIME
         TMTEMP = (TIME - ID) * 24.0
         IH = TMTEMP
         TMTEMP = (TMTEMP - IH) * 60.0
         IM = TMTEMP
         SEC = (TMTEMP - IM) * 60.0
         WRITE (MSGTXT,1000) ID, IH, IM, SEC, IF, ICHAN, POL(IST)
         END IF
C                                       Zero sums, counts etc.
      NPRT = 0
      DO 10 LOOP = 1,NUMANT
         SUMWT(LOOP) = 0.0
         COUNT(LOOP) = 0
 10      CONTINUE
C                                       Determine phase residuals.
      DO 30 LOOP = 1,NUMBL
         II = IS(LOOP)
         JJ = JS(LOOP)
         ZR = GAIN(1,II) * GAIN(1,JJ) + GAIN(2,II) * GAIN(2,JJ)
         ZI = GAIN(1,II) * GAIN(2,JJ) - GAIN(2,II) * GAIN(1,JJ)
         ZZR = VOBS(1,LOOP) * ZR - VOBS(2,LOOP) * ZI
         ZZI = VOBS(1,LOOP) * ZI + VOBS(2,LOOP) * ZR
         VOBS(2,LOOP) = SQRT (ZZR*ZZR + ZZI*ZZI) /
     *      (ZR*ZR + ZI*ZI)
         IF (VOBS(2,LOOP)*WT(LOOP).GT.1.0E-20) VOBS(1,LOOP) =
     *      ATAN2 (ZZI, ZZR)
 30      CONTINUE
C                                       Sum square residuals
      DO 50 LOOP = 1,NUMBL
         IF (VOBS(2,LOOP)*WT(LOOP).LT.1.0E-20) GO TO 50
            PHSQ = VOBS(1,LOOP) * VOBS(1,LOOP) * WT(LOOP)
            II = IS(LOOP)
            COUNT(II) = COUNT(II) + 1
            SUMWT(II) = SUMWT(II) + WT(LOOP)
            JJ = JS(LOOP)
            COUNT(JJ) = COUNT(JJ) + 1
            SUMWT(JJ) = SUMWT(JJ) + WT(LOOP)
            ERRA = ABS (LOG10 (ABS (VOBS(2,LOOP)+1.E-20)))
            ERRP = ABS (VOBS(1,LOOP))
            CLSERR(1,1) = CLSERR(1,1) + ERRA
            CLSERR(1,2) = CLSERR(1,2) + ERRA * ERRA
            CLSERR(1,3) = CLSERR(1,3) + 1.0
            CLSERR(2,1) = CLSERR(2,1) + ERRP
            CLSERR(2,2) = CLSERR(2,2) + ERRP * ERRP
C                                       closure offset excess
            IF ((DOCLOS) .AND. ((ERRP.GT.PHMAX) .OR. (ERRA.GT.AMPMAX)))
     *         THEN
               CLSERR(2,3) = CLSERR(2,3) + 1.0
               CLSCNT(II) = CLSCNT(II) + 1
               CLSCNT(JJ) = CLSCNT(JJ) + 1
C                                       Print closure offsets.
               IF ((PRTLV.GE.2) .AND. (NCLSER.LE.1000)) THEN
C                                       Label if necessary
                  IF (DOLPRT) CALL MSGWRT (4)
                  DOLPRT = .FALSE.
C                                       Flush buffer if full
                  IF (NPRT.GE.3) THEN
                     WRITE (MSGTXT,1010) (BLPRT(1,I), BLPRT(2,I),
     *                  BLPRT(3,I), BLPRT(4,I), I = 1,NPRT)
                     CALL MSGWRT (4)
                     NPRT = 0
                     NCLSER = NCLSER + 1
                     IF (NCLSER.EQ.1001) THEN
                        MSGTXT = 'CLOSURE ERROR MESSAGE COUNT EXCEEDED'
     *                     // ' REMAINDER OMITTED'
                        CALL MSGWRT (6)
                        END IF
                     END IF
C                                       New entry
                  NPRT = NPRT + 1
                  BLPRT(1,NPRT) = II
                  BLPRT(2,NPRT) = JJ
                  ERRA = (10.0 ** ERRA) - 1.0
                  BLPRT(3,NPRT) = IROUND (ERRA * 100.0)
                  BLPRT(4,NPRT) = IROUND (ERRP * 57.296)
                  BLPRT(3,NPRT) = MAX (-9999, MIN (99999,
     *               BLPRT(3,NPRT)))
                  END IF
               END IF
 50         CONTINUE
C                                       Flush closure message buffer
      IF (NPRT.GT.0) THEN
         WRITE (MSGTXT,1010) (BLPRT(1,LOOP), BLPRT(2,LOOP),
     *      BLPRT(3,LOOP), BLPRT(4,LOOP), LOOP = 1,NPRT)
         CALL MSGWRT (4)
         NPRT = 0
         NCLSER = NCLSER + 1
         IF (NCLSER.EQ.1001) THEN
            MSGTXT = 'CLOSURE ERROR MESSAGE COUNT EXCEEDED'
     *         // ' REMAINDER OMITTED'
            CALL MSGWRT (6)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Closure errors at',I3,'/',2I3,F4.0,
     *   ' IF/Chn no. ',I4,I4,2X,A4)
 1010 FORMAT (3(I5,'-',I3,I5,'%',I5,'d'))
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
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'EZERO.INC'
      INCLUDE 'BPASS.INC'
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
               WRITE (MSGTXT,1050)
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
 1050 FORMAT ('INITEX: CANNOT FIND WEIGHT & SCALE FOR COMPRESSED DATA')
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
      INTEGER   TIT(3), I, CMPNT3, IPOLPT, NPOLDO, CMPNT4, LOOPIF, BO,
     *   INDEX, LOOPS, JNDEX, INP, RECOFF, BOFF, BPFVIS, BPLVIS, A1, A2,
     *   A1O, A2O
      REAL      TIMEL, TIME0, IBLL, IBL0, TITSEC, CHZ(MAXIF*12), DENOM,
     *   TEMP, AMPD
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'EZERO.INC'
      INCLUDE 'BPASS.INC'
      SAVE BPFVIS, BPLVIS
      DATA F /.FALSE./
      DATA BO /1/
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
            WRITE (MSGTXT,1050)
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
         WRITE (MSGTXT,1030)
         IERR = 1
         GO TO 990
         END IF
C                                       We've got it
      IF (RECNO3.LE.BPLVIS) THEN
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
         IBLL  = LRPARM(ILOCB+1) + 0.01
         IBL0  = BUFF3(BOFF+ILOCB3) + 0.01
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
         DO 60 I = 1,NDECM3
            CMPNT3 = BOFF + NPARM3 + DECM3(2,I)/3
            CMPNT4 = 1 + DECM3(2,I)
            CALL ZUVXPN (DECM3(1,I), BUFF3(CMPNT3),
     *         BUFF3(BOFF+KLOCW3), CHZ(CMPNT4))
 60         CONTINUE
C                                       Uncompressed data
      ELSE
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
      IF ((INCOR.GT.1) .AND. (NCOR.EQ.1)) THEN
         IPOLPT = ABS(ICOR0) - 1
         IF (ICOR0.LT.-4) IPOLPT = ABS(ICOR0) - 5
         END IF
      NPOLDO = NCOR
      IF (NCOR.GT.1) NPOLDO = 2
C                                       Do the division
      DO 300 LOOPS = 1, NPOLDO
         DO 200 LOOPIF = 1,MAXIFS
            INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-1) * INCIF
            JNDEX = CMPNT4 + (LOOPS-1+IPOLPT) * INCS3 +
     *         (LOOPIF-1) * INCIF3
            DENOM = 0.0
            IF (CHZ(JNDEX+2).GT.0.0) DENOM = CHZ(JNDEX) * CHZ(JNDEX) +
     *         CHZ(JNDEX+1) * CHZ(JNDEX+1)
            IF (DENOM.LE.0) THEN
               DO 90 I = 1,NUMFRQ
                  INP = INDEX + (I-1) * INCF
                  VIS(INP) = 0.0
                  VIS(INP+1) = 0.0
                  VIS(INP+2) = 0.0
 90               CONTINUE
            ELSE
               IF (PHONLY) DENOM = SQRT (DENOM)
               DO 100 I = 1,NUMFRQ
                  INP = INDEX + (I-1) * INCF
                  TEMP = VIS(INP)
                  AMPD = TEMP*TEMP + VIS(INP+1)*VIS(INP+1)
                  VIS(INP+2) = DENOM * DENOM * VIS(INP+2) * CHZ(JNDEX+2)
     *               / (DENOM * CHZ(JNDEX+2) + AMPD * VIS(INP+2))
                  VIS(INP)   = (CHZ(JNDEX)*TEMP +
     *               CHZ(JNDEX+1)*VIS(INP+1)) / DENOM
                  VIS(INP+1) = (CHZ(JNDEX)*VIS(INP+1) -
     *               CHZ(JNDEX+1)*TEMP) / DENOM
 100              CONTINUE
               END IF
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
 1030 FORMAT ('EXTDIV: LINE & CH. 0 DATA OUT OF SEQUENCE')
 1050 FORMAT ('INITEX: NO DATA SELECTED')
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
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BPASS.INC'
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
      SUBROUTINE DATSHF (DISKIN, CNOIN, RPARMS, VIS, WORK, EVN, GUSE,
     *   CVLSOU, APARM, SUPRMS, PRESOU, WUVCMP, VISNUM, FRQSEL, CATINP,
     *   IRET)
C-----------------------------------------------------------------------
C  DATSHF performs the calculations and shifts necessary to line up
C  VLBA bandpasses in frequency. It differs from DATSHF in that all
C  that is necessary is to remove the extra lobe-rotation from the
C  station to the the Earth centre. It determines this by running
C  CVLDOP twice.
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
C     GUSE           I         If CL table, use this version number
C     CVLSOU         I         Source # being shifted
C     APARM(*)       R         User supplied vel info for single
C                              source files
C     SUPRMS         L         If true supress messages about large
C                              shifts
C     PRESOU(*)*16   C         Names of sources to be shifted.
C     WUVCMP         L         Input/output data are compressed
C     VISNUM         I         Visibility number. 1 => some things need
C                              to be opened. -1 => some need to be
C                              closed.
C     FRQSEL         I         Freq ID working on
C     CATINP(256)    I         Catalogue block from original input file
C  Output:
C     IRET           I         Error code, = 0 => OK
C
C  P. Diamond ,  August 1994
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, GUSE, CVLSOU, VISNUM, FRQSEL,
     *   CATINP(256), IRET
      REAL      RPARMS(*), VIS(*), WORK(*), APARM(*)
      LOGICAL   EVN, SUPRMS, WUVCMP
      CHARACTER PRESOU(*)*16
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IANT1, IANT2, I, IPOL, IFNO, IFRQ, INDEX, ISB, FINDEX,
     *   CVLSUB, TIT(4), ITEL, IREFST, NUMIF, NFREQ, INCSU, INCIFU, IP,
     *   INCFU, IA, OLDSOU, ITMP, CVER, ICHLUN, SUBARR, GAMMA, IDUM1,
     *   IDUM2, CATT(256), MLOCIF, MLOCF, NAVWT, ISMTH, SOUOLD, WBUFSZ
      DOUBLE PRECISION DELTOT, RATE
      LOGICAL   DOINTP, ALLFLG, MULTI, CLSORT
      HOLLERITH CATH(256)
      REAL      VISTMP(2,MAXCHA), TMPWT(MAXCHA), INTWTS(MAXCHA),
     *   CATR(256), AVWT, RTMP, UT
      DOUBLE PRECISION   FRQOFF(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (CATT, CATR, CATH)
      SAVE MLOCIF, MLOCF, SOUOLD, NUMIF, NFREQ, MULTI, INCSU, INCIFU,
     *   INCFU, CLSORT
      DATA CLSORT /.FALSE./
C-----------------------------------------------------------------------
      CALL COPY (256, CATINP, CATT)
      IF (VISNUM.EQ.-1) THEN
C                                       Close down stuff
         IF (USEAP) CALL QRLSE
         GO TO 999
         END IF
C
      IF (VISNUM.EQ.1) THEN
C                                       Determine pointers
         CALL AXEFND (8, 'FREQ    ', CATT(KIDIM), CATH(KHCTP), MLOCF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1070) IRET
            GO TO 990
            END IF
         CALL AXEFND (8, 'IF      ', CATT(KIDIM), CATH(KHCTP), MLOCIF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1080) IRET
            GO TO 990
            END IF
C                                       Initialize stuff
         SOUOLD = -1
C                                       Do we use PSAP FFT
         NFREQ = CATINP(KINAX+MLOCF)
         POWRTO = .FALSE.
         DO 10 GAMMA = 1, 15
            IF ((2**GAMMA).EQ.NFREQ) THEN
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
C                                       the default size
            IDUM1 = 5120
            CALL QINIT (IDUM1, IDUM2, APNUM)
            IF ((APNUM.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               IRET = 10
               MSGTXT = 'DATSHF: DID NOT GET REQUESTED AP MEMORY'
               GO TO 990
               END IF
            APBEG = 0
            APTYPE = 2
            NROLL = -1
            NBYTES = 0
            END IF
C
         ITMP = MAXANT*MAXIF
         CALL RFILL (ITMP, -1000.0, AVDELI)
         CALL FILL (ITMP, 0, NDELIN)
C                                       Get frequency info.
         OLDSOU = -1
         CVER = 1
         ICHLUN = 44
         CALL CHNDAT ('READ', WORK, DISKIN, CNOIN, CVER, CATINP, ICHLUN,
     *      CNNIF, CFOFF, CSBAND, CFINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) IRET
            GO TO 990
            END IF
C                                       Fill AN information
C                                       into common in D/CANS.INC
         SUBARR = 1
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATINP, WORK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET
            GO TO 990
            END IF
C                                       Correct station positions for
C                                       centre array offset if non-zero
         DO 60 I = 1, NSTNS
            IA = TELNO(I)
            ANTX(IA) = CNTRX + STNX(IA)
            ANTY(IA) = CNTRY + STNY(IA)
            ANTZ(IA) = CNTRZ + STNZ(IA)
   60       CONTINUE
C
         NUMIF = 1
         IF (MLOCIF.GT.0) NUMIF = CATINP(KINAX+MLOCIF)
         NFREQ = CATINP(KINAX+MLOCF)
         CALL MULSDB (CATINP, MULTI)
         INCSU = INCS
         INCIFU = INCIF
         INCFU = INCF
         IF (WUVCMP) THEN
            INCSU = INCS * 3
            INCIFU = INCIF * 3
            INCFU = INCF * 3
            END IF
         END IF
C
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
      IF (EVN) THEN
         ITEL = IANT2
      ELSE
         ITEL = IANT1
         END IF
C                                       Get basic freq. parms
      IF (CVLSOU.NE.SOUOLD) THEN
         I = MSGKIL
         MSGKIL = 32005
         CALL GETFRQ (DISKIN, CNOIN, APARM, OLDSOU, CVLSOU, FRQSEL,
     *      CATINP, IRET)
         MSGKIL = I
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET
            GO TO 990
            END IF
         END IF
      SOUOLD = CVLSOU
C                                       Obtain any other frq. offsets
C                                       hiding in the CL table
      WBUFSZ = 4 * MAXCHA * 2
      I = MSGKIL
      MSGKIL = 32005
      CALL FRQUPD (DISKIN, CNOIN, GUSE, ITEL, UT, CVLSOU, SUBARR,
     *   FRQSEL, CATINP, CLSORT, FRQOFF, IRET)
      CLSORT = .TRUE.
      MSGKIL = I
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Determine full sky frequency
C                                       of reference pixel. for each IF
      DO 100 I = 1,NUMIF
         OBSFRQ(I) = REFFRQ(I) + FRQOFF(I)
 100     CONTINUE
C                                       Loop and shift
      DO 300 IPOL = 1,NCOR
         IP = IPOL
         DO 200 IFNO = 1,NUMIF
C                                       Determine shifts
            CALL DETRAT (UT, RAEPO, DECEPO, ANTX(IANT1),
     *         ANTY(IANT1), ANTZ(IANT1), REFFRQ(IFNO), RATE)
            DELTOT = RATE / CFINC(IFNO)
            DELTOT = -DELTOT
            RTMP = DELTOT
            RTMP = ABS(RTMP)
            AVDELI(ITEL,IFNO) = MAX (AVDELI(ITEL,IFNO),RTMP)
            NDELIN(ITEL,IFNO) = NDELIN(ITEL,IFNO) + 1
C                                       If shift too large, give
C                                       warning
            IF ((ABS(DELTOT/REAL(NFREQ)).GT.0.10) .AND.
     *         (.NOT.SUPRMS)) THEN
               CALL TODHMS (UT, TIT)
               WRITE (MSGTXT,1020) TIT, DELTOT
               CALL MSGWRT (8)
               WRITE (MSGTXT,1025) PRESOU(CVLSOU), IANT1, IANT2,
     *            IFNO
               CALL MSGWRT (8)
               END IF
C                                       Copy data to temp array.
            DOINTP = .FALSE.
            INDEX = 1 + (IP-1) * INCSU + (IFNO-1) * INCIFU
            AVWT = 0.0
            NAVWT = 0
            DO 120 IFRQ = 1, NFREQ
               FINDEX = INDEX + (IFRQ-1) * INCFU
               VISTMP(1,IFRQ) = VIS(FINDEX)
               VISTMP(2,IFRQ) = VIS(FINDEX+1)
               TMPWT(IFRQ)    = VIS(FINDEX+2)
               IF (TMPWT(IFRQ).LE.0.0) DOINTP = .TRUE.
               IF (TMPWT(IFRQ).GT.0.0) THEN
                  AVWT = AVWT + TMPWT(IFRQ)
                  NAVWT = NAVWT + 1
                  END IF
  120          CONTINUE
            IF (NAVWT.GT.0) AVWT = AVWT / NAVWT
C                                       Deal with flagged
C                                       spectral data
            ALLFLG = .FALSE.
            IF (DOINTP) CALL SPINTP (NFREQ, VISTMP, TMPWT,
     *         INTWTS, ALLFLG)
            IF (ALLFLG) THEN
               DO 125 IFRQ = 1, NFREQ
                  FINDEX = INDEX + (IFRQ-1) * INCFU
                  IF (VIS(FINDEX+2).GT.0.0) VIS(FINDEX+2) =
     *               -1.0 * VIS(FINDEX+2)
  125             CONTINUE
               GO TO 200
               END IF
C                                       Shift it!
            ISB = CSBAND(IFNO)
            IF (IANT1.EQ.IANT2) THEN
               ISMTH = 1
               CALL TPSHFT (VISTMP, NFREQ, DELTOT, WORK, ISMTH)
            ELSE
               ISMTH = 0
               CALL XCSHFT (VISTMP, ISB, NFREQ, DELTOT, WORK, ISMTH)
               END IF
C                                       Shift weights
            CALL WTSHFT (TMPWT, NFREQ, DELTOT)
C                                       Copy data back to vis
C                                       array
            DO 140 IFRQ = 1, NFREQ
               FINDEX = INDEX + (IFRQ-1) * INCFU
               IF (IANT1.EQ.IANT2) THEN
                  VIS(FINDEX) = SQRT (VISTMP(1,IFRQ)*VISTMP(1,IFRQ) +
     *               VISTMP(2,IFRQ)*VISTMP(2,IFRQ))
                  VIS(FINDEX+1) = 0.0
               ELSE
                  VIS(FINDEX) = VISTMP(1,IFRQ)
                  VIS(FINDEX+1) = VISTMP(2,IFRQ)
                  END IF
               IF (TMPWT(IFRQ).LE.0.0) VIS(FINDEX+2) = AVWT
               IF (TMPWT(IFRQ).GT.0.0) VIS(FINDEX+2) = TMPWT(IFRQ)
  140          CONTINUE
  200       CONTINUE
  300    CONTINUE
C
      GO TO 999
C                                       Write Error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DATSHF: ERROR',I4,' DETERMINING FREQ./VEL. PARMS')
 1010 FORMAT ('DATSHF: ERROR',I4,' DETERMINING FULL SKY FREQ.')
 1020 FORMAT ('Warning: Time = ',I4,'/',3I3.2,' Channel shift = ',F15.3)
 1025 FORMAT ('For Source: ',A16,' Antennas: ',I3,'-',I3,' IF# ',I3)
 1050 FORMAT ('DATSHF: ERROR',I3,' GETTING FREQ. INFO. WITH CHNDAT')
 1060 FORMAT ('DATSHF ERROR',I3,' OBTAINING ANTENNA INFORMATION')
 1070 FORMAT ('DATSHF: ERROR',I3,' DETERMINING FREQ. POINTER')
 1080 FORMAT ('DATSHF: ERROR',I3,' DETERMINING IF POINTER')
      END
      SUBROUTINE BPSHFT (DISKIN, CNOIN, UT, IANT, IFNO, TREAL, TIMAG,
     *   WORK, CVLSOU, REFANT, ACONLY, VISNUM, FRQSEL, CATINP,
     *   IRET)
C-----------------------------------------------------------------------
C  BPSHFT performs the calculations and shifts of the BP solution
C  necessary to line up VLBA bandpasses in frequency.
C
C  Input/Output:
C     DISKIN         I         Volume number
C     CNOIN          I         File catalogue number
C     UT             R         UT time in days
C     IANT           I         Antenna number as it's seen at AN table
C     TREAL(*)       R         The real part of the visibility
C     TIMAG(*)       R         The imag part of the visibility
C                              On output will contain the shifted
C                              data.
C     WORK(*)        R         Work buffer (>= 8192)
C     CVLSOU         I         Source # being shifted
C     REFANT         I         The reference antenna number
C     ACONLY         L         If true, only auto correlation is used
C     VISNUM         I         The number. 1 => some things need
C                              to be opened.
C     FRQSEL         I         Freq ID working on
C     CATINP(256)    I         Catalogue block from original input file
C
C  Output:
C     IRET           I         Error code, = 0 => OK
C
C  L. Kogan,  Jan 2000
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, CVLSOU, REFANT, VISNUM, FRQSEL,
     *   CATINP(256), IRET
      REAL      TREAL(*), TIMAG(*), WORK(*)
      LOGICAL  ACONLY
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IANT, I, IFNO, IFRQ, ISB, NFREQ, IA, OLDSOU, ITMP,
     *   CVER, ICHLUN, SUBARR, GAMMA, IDUM1, IDUM2, CATT(256),
     *   MLOCIF, MLOCF, ISMTH, SOUOLD
      DOUBLE PRECISION DELTOT, RATE
      HOLLERITH CATH(256)
      REAL      VISTMP(2,MAXCHA), CATR(256), UT, CVPARM(10)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (CATT, CATR, CATH)
      SAVE MLOCIF, MLOCF, SOUOLD, NFREQ, OLDSOU
C-----------------------------------------------------------------------
      CALL COPY (256, CATINP, CATT)
C
      IF (VISNUM.EQ.1) THEN
C                                       Determine pointers
         CALL AXEFND (8, 'FREQ    ', CATT(KIDIM), CATH(KHCTP), MLOCF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1070) IRET
            GO TO 990
            END IF
         CALL AXEFND (8, 'IF      ', CATT(KIDIM), CATH(KHCTP), MLOCIF,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1080) IRET
            GO TO 990
            END IF
C                                       Initialize stuff
         SOUOLD = -1
C                                       Do we use PSAP FFT
         NFREQ = CATINP(KINAX+MLOCF)
         POWRTO = .FALSE.
         DO 10 GAMMA = 1, 15
            IF ((2**GAMMA).EQ.NFREQ) THEN
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
C                                       the default size
            IDUM1 = 5120
            CALL QINIT (IDUM1, IDUM2, APNUM)
            IF ((APNUM.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               MSGTXT = 'BPSHFT: DID NOT GET REQUESTED AP MEMORY'
               IRET = 10
               GO TO 990
               END IF
            APBEG = 0
            APTYPE = 2
            NROLL = -1
            NBYTES = 0
            END IF
C
         ITMP = MAXANT*MAXIF
         CALL RFILL (ITMP, -1000.0, AVDELI)
         CALL FILL (ITMP, 0, NDELIN)
C                                       Get frequency info.
         OLDSOU = -1
         CVER = 1
         ICHLUN = 44
         CALL CHNDAT ('READ', WORK, DISKIN, CNOIN, CVER, CATINP, ICHLUN,
     *      CNNIF, CFOFF, CSBAND, CFINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) IRET
            GO TO 990
            END IF
C                                       Fill AN information
C                                       into common in D/CANS.INC
         SUBARR = 1
         CALL GETANT (DISKIN, CNOIN, SUBARR, CATINP, WORK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET
            GO TO 990
            END IF
C                                       Correct station positions for
C                                       centre array offset if non-zero
         DO 60 I = 1, NSTNS
            IA = TELNO(I)
            ANTX(IA) = CNTRX + STNX(IA)
            ANTY(IA) = CNTRY + STNY(IA)
            ANTZ(IA) = CNTRZ + STNZ(IA)
   60       CONTINUE
         END IF
C                                       Get basic freq. parms and the
C                                       source coordinates
      I = MSGKIL
      MSGKIL = 32005
      CALL RFILL (10, 0.0, CVPARM)
      CALL GETFRQ (DISKIN, CNOIN, CVPARM, OLDSOU, CVLSOU, FRQSEL,
     *   CATINP, IRET)
      MSGKIL = I
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      SOUOLD = CVLSOU
C                                       Determine shifts
      CALL DETRAT (UT, RAEPO, DECEPO, ANTX(IANT),
     *      ANTY(IANT), ANTZ(IANT), REFFRQ(IFNO), RATE)
      DELTOT = RATE / CFINC(IFNO)
      DELTOT = -DELTOT
      AVDELI(IANT,IFNO) = ABS(DELTOT)
C                                       store the real and imag part of
C                                       the BP at the temporal array
      DO 120 IFRQ = 1, NFREQ
         VISTMP(1, IFRQ) = TREAL(IFRQ)
         VISTMP(2, IFRQ) = TIMAG(IFRQ)
  120    CONTINUE

C                                       Shift it!
      ISB = CSBAND(IFNO)
      IF ((IANT .EQ. REFANT) .OR. ACONLY) THEN
         ISMTH = 1
         CALL TPSHFT (VISTMP, NFREQ, DELTOT, WORK, ISMTH)
      ELSE
         ISMTH = 0
         CALL XCSHFT (VISTMP, ISB, NFREQ, DELTOT, WORK, ISMTH)
         END IF

C                                       Copy data back to vis
C                                       array
      DO 140 IFRQ = 1, NFREQ
         IF (IANT .EQ. REFANT) THEN
            TREAL(IFRQ) = SQRT (VISTMP(1,IFRQ)*VISTMP(1,IFRQ) +
     *               VISTMP(2,IFRQ)*VISTMP(2,IFRQ))
            TIMAG(IFRQ) = 0.0
         ELSE
            TREAL(IFRQ) = VISTMP(1,IFRQ)
            TIMAG(IFRQ) = VISTMP(2,IFRQ)
            END IF
  140    CONTINUE
C
      GO TO 999
C                                       Write Error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPSHFT: ERROR',I4,' DETERMINING FREQ./VEL. PARMS')
 1050 FORMAT ('BPSHFT: ERROR',I3,' GETTING FREQ. INFO. WITH CHNDAT')
 1060 FORMAT ('BPSHFT  ERROR',I3,' OBTAINING ANTENNA INFORMATION')
 1070 FORMAT ('BPSHFT: ERROR',I3,' DETERMINING FREQ. POINTER')
 1080 FORMAT ('BPSHFT: ERROR',I3,' DETERMINING IF POINTER')
      END
