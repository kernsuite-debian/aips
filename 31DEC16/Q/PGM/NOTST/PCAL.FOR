LOCAL INCLUDE 'PPCAL.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXDAT, MAXCAL, MAXSOU
C                                       MAXDAT = Number of vis in
C                                       arrays.
      PARAMETER (MAXDAT=50000)
C                                       MAXCAL = Number of calibrators
C                                       possible.
      PARAMETER (MAXCAL=50)
      PARAMETER (MAXSOU=500)
LOCAL INCLUDE 'PCAL.INC'
C                                                         Include PCAL
C                                       Local include for PCAL
      INCLUDE 'PPCAL.INC'
      INTEGER   CATIN(256), SEQIN, SEQ2, DISKIN, DISK2, CNOIN, CNOIN2,
     *   CCTVER, VISDSK, VISCNO, VMDSK, VMCNO, JBUFSZ, ANTS(2,MAXDAT),
     *   VSOU(MAXDAT), NVCAL, VCALID(MAXCAL), NCOMP(MAXFLD), NUMDAT,
     *   IONSCR, FREQID, CHNSEL(3,20,MAXIF), VSOUID(MAXSOU), PSMRAD,
     *   ECHANT, CPVER, CSIDNO(MAXCAL), TMPCNT, IBUFF1(UVBFSS),
     *   IBUFF2(UVBFSS)
      LOGICAL   SINGLE, DOMODL, CLNMOD, VDOFIT(MAXCAL), ISXY, AVGIF,
     *   IMODEL, DOLINE, DOINTP, CMODEL
      REAL   XSI, XDI, XQUAL, XTIME(8), XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XANTS(50), XUVRA(2), XSUBA, XDOCAL, XGUSE, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH(3), XCHNS(4,20), XS2, XD2, XVER,
     *   XNCOMP(MAXAFL), XFLUX, XNMAP, XDOMOD, XSMOD(7), XSPECT(4,20),
     *   DOSCAL, XSOLIN, DOSPEC, XINTP(3), XPRTLV, XREF, BPARM(10),
     *   CPARM(10), DPARM(10), XBADD(10),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), TIME(MAXDAT), PARAN(2,MAXDAT),
     *   VMOD(2,4,MAXDAT), VWT(4,MAXDAT), VOBS(2,4,MAXDAT),
     *   VFLUX(4,MAXCAL,MAXIF), VFINC(MAXIF), REFPIX, CVFLUX(4,MAXCAL),
     *   PSMTAB(1024), PHDIFF(MAXCIF), RSPECT(4,MAXCAL),
     *   SSPECT(5,4,MAXCAL)
      HOLLERITH XNAMEI(3), XCLAIN(2), XXCALC, XXSOUR(4,30), XNAME2(3),
     *   XCLAS2(2), XXSOLT, XCMETH, XCMOD
      DOUBLE PRECISION FREQSO(MAXIF,MAXCAL)
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, NAME2*12, CLAS2*6,
     *   XSOLTY*4, CMETH*4, CMOD*4, CSNAME(MAXCAL)*16, XCALCO*4
      COMPLEX   CVMOD(4,MAXDAT), CVOBS(4,MAXDAT)
      EQUIVALENCE (CVOBS, VOBS), (CVMOD, VMOD),
     *   (BUFF1, IBUFF1), (BUFF2, IBUFF2)
      COMMON /CINFO/ CATIN, FREQSO, NCOMP, SINGLE, DOMODL, CLNMOD,
     *   CNOIN, CNOIN2, CCTVER, VISDSK, VISCNO, VMDSK, VMCNO, IONSCR,
     *   DISKIN, DISK2, SEQIN, SEQ2, FREQID, IMODEL, CHNSEL, DOLINE,
     *   PSMRAD, PSMTAB, ECHANT, DOINTP, CPVER, CSIDNO, CMODEL, TMPCNT,
     *   RSPECT, SSPECT
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /XINPUT/ XNAMEI, XCLAIN, XSI, XDI, XXSOUR, XQUAL, XXCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XBIF, XEIF, XANTS, XUVRA, XSUBA,
     *   XDOCAL, XGUSE, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XCHNS,
     *   XNAME2, XCLAS2, XS2, XD2, XVER, XNCOMP, XFLUX, XNMAP, XCMETH,
     *   XCMOD, XDOMOD, XSMOD, XSPECT, DOSCAL, XSOLIN, XXSOLT, DOSPEC,
     *   XINTP, XPRTLV, XREF, BPARM, CPARM, DPARM, XBADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, NAME2, CLAS2, XSOLTY, CMETH,
     *   CMOD, CSNAME, XCALCO
      COMMON /VDATA/ TIME, PARAN, VMOD, VWT, VOBS, VFLUX, VDOFIT, ISXY,
     *   AVGIF, NUMDAT, ANTS, VSOU, NVCAL, VCALID, VSOUID, VFINC,
     *   REFPIX, CVFLUX, PHDIFF
C                                                          End PCAL
LOCAL END
      PROGRAM PCAL
C-----------------------------------------------------------------------
C! Determines Antenna polarization characteristics
C# UV Calibration AP-appl EXT-appl polarization
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2003-2004, 2006-2016
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
C   PCAL computes polarization parameters and enters them into the AN
C   table  also determines calibrator polarizations and enters them in
C   the SU table.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NC, NI, NP, NA, NWORDS
      REAL      DTERMS(2), STERMS(2)
      LONGINT   PDTERM, PSTERM
      INCLUDE 'PCAL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      DATA PRGM /'PCAL  '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL PCLIN (PRGM, NC, NI, NP, NA, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get memory
      IF (DOLINE) THEN
         NWORDS = (2 * NC * NI * NP * NA - 1) / 1024 + 3
         CALL ZMEMRY ('GET ', PRGM, NWORDS, DTERMS, PDTERM, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'FAILED TO GET REQUESTED MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         NWORDS = (4 * NC * NI * 30 - 1) / 1024 + 3
         CALL ZMEMRY ('GET ', PRGM, NWORDS, STERMS, PSTERM, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'FAILED TO GET REQUESTED MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         END IF
C                                       Determine poln. parameters.
      CALL PCLUV (NC, NI, NP, NA, DTERMS(1+PDTERM), STERMS(1+PSTERM),
     *   IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL PCLHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE PCLIN (PRGN, NC, NI, NP, NA, IRET)
C-----------------------------------------------------------------------
C   PCLIN gets input parameters for PCAL.
C   Inputs:
C      PRGN    C*6     Program name
C      NC      I     Number spectral channels
C      NI      I     Number IFs
C      NP      I     Number polarizations (2 always I suspect)
C      NA      I     Max antenna number
C   Output:
C      IRET    I         Error code: 0 => ok
C                          1 => too few frequency channels.
C                          5 => catalog troubles
C                          8 => can't start
C   Commons: /XINPUT/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in PCAL for more details.
C-----------------------------------------------------------------------
      INTEGER   NC, NI, NP, NA, IRET
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   IERR, NPARM, I, MXFLD, NUMSUB, IROUND, LUN1, NUMAN(513),
     *   J, K, K1, K2, LUNTMP, BUFFER(512), LUN2, NTERM
      LOGICAL   T, TABLE, EXIST, FITASC, MATCH
      REAL      CATR(256), CATINR(256), DUM, X, Y, CCEN, TEMP(6)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'PCAL.INC'
      INTEGER   NW(MAXIF), ISBAND(MAXIF)
      DOUBLE PRECISION REFFRQ, FOFF(MAXIF), FRATIO
      CHARACTER BNDCOD(MAXIF)*8, DATE*8
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (NUMAN, BUFF2)
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      EQUIVALENCE (CATIN, CATINR)
      DATA LUN1 /58/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
      NSOUWD = 1
C                                       Get input parameters.
C                                       Max # fields is or was 16
      MXFLD = MAXAFL
      NPARM = 433 + MXFLD
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
      DISKIN = IROUND (XDI)
      DISK2 = IROUND (XD2)
      CCTVER = IROUND (XVER)
      AVGIF = CPARM(1) .GT. 0.001
      DOLINE = DOSPEC.GT.0.0
      IF (DOLINE) AVGIF = .FALSE.
      DOINTP = CPARM(3).LE.0.0
      CALL RFILL (4*MAXCAL, 0.0, RSPECT)
      CALL RFILL (20*MAXCAL, 0.0, SSPECT)
      CALL RCOPY (80, XSPECT, RSPECT)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (4, 1, XXSOLT, XSOLTY)
      CALL H2CHR (4, 1, XCMETH, CMETH)
      CALL H2CHR (4, 1, XCMOD, CMOD)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
      CALL H2CHR (4, 1, XXCALC, XCALCO)
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
C                                       Init. scratch file info.
      VISDSK = DISKIN
      VISCNO = CNOIN
      VMDSK = 0
      VMCNO = 0
      IONSCR = 0
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      DO 30 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 30      CONTINUE
C                                       Linearly polarized feeds?
C                                       linear does not work right
C                                       default to pretend circular
      ISXY = (CATD(KDCRV+JLOCS).LT.-4.0D0) .AND. (CPARM(5).GT.0.0)
C                                       Check sort order, must be T*
      IF (ISORT(1:1).NE.'T') THEN
         IRET = 4
         WRITE (MSGTXT,1070) ISORT, 'T*'
         GO TO 990
         END IF
C                                       Save input header.
      CALL COPY (256, CATBLK, CATIN)
C                                       See if a multiple source file
      LUNS(1) = 29
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNS, BUFF1, TABLE, EXIST,
     *   FITASC, IERR)
      SINGLE = (.NOT.EXIST) .OR. (IERR.NE.0) .OR. (ILOCSU.LT.0)
      CPVER = IROUND (CPARM(4))
      CALL FNDEXT ('CP', CATBLK, I)
      IF ((CPVER.LE.0) .OR. (CPVER.GT.I)) CPVER = I
      IF (XDOMOD.LT.1.5) CPVER = 0
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 40 I = 1,30
         SOURCS(I) = XSOUR(I)
 40      CONTINUE
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
      CALL RCOPY (8, XTIME, TIMRNG)
      DXTIME = 1.0/86400.0
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      STOKES = 'FULL'
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
      ECHANT = ECHAN
      NC = ECHAN
      IF (JLOCIF.GE.0) THEN
         NI = CATBLK(KINAX+JLOCIF)
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, NI))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = NI
         EIF = MAX (1, MIN (EIF, NI))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      XBIF = BIF
      XEIF = EIF
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
            CHNSEL(1,1,K) = 1
            CHNSEL(2,1,K) = ECHAN
            CHNSEL(3,1,K) = 1
         ELSE
            DO 70 I = 1,NW(K)
               CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), J))
               IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K)) CHNSEL(2,I,K) = J
               CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), J))
 70            CONTINUE
            END IF
 75      CONTINUE
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN1, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1060)
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
      FREQID = FRQSEL
C                                       Antennas
      DO 85 I = 1,50
         ANTENS(I) = IROUND (XANTS(I))
 85      CONTINUE
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      FGVER = IROUND (XFLAG)
      CLVER = 0
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Default SI = 10 min.
      IF (XSOLIN.LT.1.0E-5) XSOLIN = 5.0
C                                       See if do divide by model
      DOMODL = XDOMOD.GT.0.0
      CMODEL = (NAME2.NE.' ') .OR. (CLAS2.NE.' ')
      IF (CMODEL) CALL RFILL (7, 0.0, XSMOD)
      IMODEL = XSMOD(1).GT.0.0
      IF (.NOT.IMODEL) CALL RFILL (7, 0.0, XSMOD)
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
      IF (NUMSUB.LT.SUBARR) THEN
         WRITE (MSGTXT,1085) SUBARR, NUMSUB
         GO TO 990
         END IF
C                                       Find number of antennas.
      CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN1, BUFF1, NUMAN, IRET)
      IF (IRET.NE.0) GO TO 999
      NUMANT = NUMAN(2)
      IF (SUBARR.LE.NUMAN(1)) NUMANT = NUMAN(1+SUBARR)
C                                       Number of IFs
      NUMIF = 1
      IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Number of polarizations
      NUMPOL = 1
      IF (CATBLK(KINAX+JLOCS) .GT. 1) NUMPOL = 2
      NP = NUMPOL
C                                       Get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      NA = NSTNS
C                                       check UV data set exists,
C                                       set variables for PCLSOU
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVGET ('CLOS', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Init. cal. fluxes etc.
      K = MAXCAL
      CALL FILL (K, 0, VCALID)
      CALL LFILL (K, T, VDOFIT)
      K = 4 * MAXCAL * NUMIF
      CALL RFILL (K, 0.0, VFLUX)
C                                       single source
      IF (SINGLE) THEN
         CALL RCOPY (4, XSMOD, VFLUX)
         IF ((.NOT.IMODEL) .AND. (.NOT.CMODEL)) THEN
            IRET = 8
            MSGTXT = 'YOU MUST PROVIDE A PMODEL(1) FOR SINGLE-SOURCE'
     *         // ' FILES'
            GO TO 990
            END IF
         NVCAL = 1
         VCALID(1) = 1
         CALL H2CHR (8, 1, CATUV(KHOBJ), CSNAME(1))
         VSOUID(1) = 1
         CSIDNO(1) = 1
         CALL DFILL (NUMIF, 0.0D0, FREQSO(1,1))
C                                       multi-source
      ELSE
         CALL PCLSOU (IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET, 'GETTING CALSOURCE PARAMETERS'
            GO TO 990
            END IF
         END IF
      CALL PCLNDX (IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, 'GETTING SCAN PARAMETERS'
         GO TO 990
         END IF
C                                       get full frequencies
      K1 = 1
      LUN2 = LUNTMP (1)
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, K1, CATIN, LUN2,
     *   NUMIF, FOFF, ISBAND, VFINC, BNDCOD, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, 'GETTING FREQUENCY PARAMETERS'
         GO TO 990
         END IF
      REFFRQ = CATD(KDCRV+JLOCF)
      REFPIX = CATR(KRCRP+JLOCF)
      CCEN = (1.0 + CATBLK(KINAX+JLOCF)) / 2.0
      DO 110 I = 1,NVCAL
         DO 100 J = 1,NUMIF
            FREQSO(J,I) = FREQSO(J,I) + REFFRQ + FOFF(J) +
     *         (CCEN-REFPIX) * VFINC(I)
            IF ((SINGLE) .OR. ((DOMODL) .AND. (IMODEL))) THEN
               FRATIO = FREQSO(J,I) / FREQSO(1,I)
               DO 95 K = 1,4
                  VFLUX(K,I,J) = XSMOD(K) * (FRATIO**RSPECT(K,I))
 95               CONTINUE
               END IF
 100        CONTINUE
 110     CONTINUE
      REFPIX = CCEN
C                                       solve for spectral index
      IF (DOSCAL.GE.0.0) THEN
         K1 = 0
         CALL H2CHR (8, 1, CATH(KHDOB), DATE)
         DO 150 I = 1,NVCAL
            DO 140 K = 1,4
               SSPECT(2,K,I) = RSPECT(K,I)
               IF ((RSPECT(K,I).EQ.0.0) .AND. (K.EQ.1)) THEN
                  NTERM = 1
                  IF (DOSCAL.GT.1.5) NTERM = 2
                  CALL PSOSPX (CSNAME(I), DATE, FREQSO(1,I), NTERM,
     *               TEMP, IERR)
                  IF (IERR.NE.0) CALL PNDSPX (MAXCAL, K, I, NUMIF,
     *               VFLUX, FREQSO(1,I), NTERM, TEMP, IERR)
                  IF (IERR.EQ.0) THEN
                     SSPECT(1,K,I) = TEMP(1)
                     SSPECT(2,K,I) = TEMP(2)
                     IF (NTERM.GE.2) SSPECT(3,K,I) = TEMP(3)
                     IF (NTERM.GE.3) SSPECT(4,K,I) = TEMP(4)
                     IF (NTERM.GE.4) SSPECT(5,K,I) = TEMP(5)
                     END IF
                  IF ((RSPECT(K,I).NE.0.0) .AND. (K.EQ.1)) THEN
                     K1 = K1 + 1
                     DO 135 J = 1,NUMIF
                        IF (VFLUX(K,I,J).EQ.0.0) THEN
                           X = LOG10 (FREQSO(J,I)) - 9
                           Y = SSPECT(2,K,I)*X + SSPECT(3,K,I)*X*X
                           VFLUX(K,I,J) = SSPECT(1,K,I) * (10.0**Y)
                           END IF
 135                    CONTINUE
                     END IF
               ELSE IF (RSPECT(K,I).EQ.0) THEN
                  SSPECT(2,K,I) = SSPECT(2,1,I)
                  SSPECT(3,K,I) = SSPECT(3,1,I)
                  END IF
               RSPECT(K,I) = SSPECT(2,K,I)
 140           CONTINUE
 150        CONTINUE
         IF (K1.GT.0) THEN
            MSGTXT = 'Solved for spectral indices - will use:'
            CALL MSGWRT (3)
            WRITE (MSGTXT,1150)
            CALL MSGWRT (3)
            DO 160 I = 1,NVCAL
               WRITE (MSGTXT,1151) I, CSNAME(I), (SSPECT(K,1,I),
     *            K = 1,5)
               CALL MSGWRT (3)
 160           CONTINUE
            END IF
         END IF
C                                       smoothing functions
      IF (DOLINE) CALL PCLSMI
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1050 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1070 FORMAT ('SORT ORDER IS ',A2,' NOT ',A2,' AS REQUIRED')
 1085 FORMAT ('SPECIFIED SUBARRAY ',I4,' > MAX. OF ',I4)
 1100 FORMAT ('PCLIN: ERROR',I4,1X,A)
 1150 FORMAT ('Num  Name',12X,'Flux@1GHz',' Sp index',' Curv(1)',
     *   ' Curv(2)',' Curv(3)')
 1151 FORMAT (I3,2X,A,F9.3,4F9.4)
      END
      SUBROUTINE PCLSOU (IRET)
C-----------------------------------------------------------------------
C   PCLSOU collects calibrator polarized flux density information.
C   Input from common:
C      NSOUWD   I       Number of sources included or excluded; if 0
C                       all sources are included.
C      DOSWNT   L       If .TRUE. then sources in SOUWAN are included
C                       If .FALSE. then excluded.
C      SOUWAN   I(*)    The source numbers of sources included or
C                       excluded.
C      DISKIN   I       Disk number of the input multisource data file
C                       whose SU table is to be used.
C      CNOIN    I       Catalog slot number for SU file.
C   Output to Common:
C      NVCAL    I       Number of calibrator sources
C      VCALID   I(*)    Calibrator ID numbers
C      VFLUX    R(4,*,if)   Polarized flux densities (I,Q,U,V) (Jy)
C      VDOFIT   L(*)    If true fit for this calibrator polarization.
C   Output:
C      IRET     I       Return code, 0 => OK, otherwise abort.
C   Note: also uses buffer NXBUFF
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PCAL.INC'
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
      DATA SULUN /47/
C-----------------------------------------------------------------------
      NVCAL = 0
C                                       Open source (SU) table
      CALL SOUINI ('READ', NXBUFF, DISKIN, CNOIN, 1, CATUV,
     *   SULUN, NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Check FREQID compatibility.
      IF ((SUFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *   (SUFQID.NE.FREQID)) THEN
         MSGTXT = 'WARNING - POTENTIALLY FATAL ERROR'
         CALL MSGWRT (6)
         WRITE (MSGTXT,1040) SUFQID
         CALL MSGWRT (6)
         WRITE (MSGTXT,1050) FREQID
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
         CALL TABSOU ('READ', NXBUFF, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *      PMRA, PMDEC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Save calibrator fluxes
C                                       See if wanted
         IF (NSOUWD.LE.0) GO TO 30
            DO 20 I = 1,NSOUWD
               IF (IDSOU.EQ.SOUWAN(I)) GO TO 30
 20            CONTINUE
C                                       Not wanted
            GO TO 50
C                                       Wanted
 30      IF (NVCAL.LT.MAXCAL) THEN
            NVCAL = NVCAL + 1
            VCALID(NVCAL) = IDSOU
            DO 35 I = 1,NUMIF
               VFLUX(1,NVCAL,I) = FLUX(1,I)
               VFLUX(2,NVCAL,I) = FLUX(2,I)
               VFLUX(3,NVCAL,I) = FLUX(3,I)
               VFLUX(4,NVCAL,I) = FLUX(4,I)
               FREQSO(I,NVCAL) = FREQO(I)
 35            CONTINUE
            VDOFIT(NVCAL) = .NOT.DOMODL
            CSNAME(NVCAL) = SOUNAM
            VSOUID(IDSOU) = NVCAL
            CSIDNO(NVCAL) = IDSOU
C                                       Too many calibrators
         ELSE
            IRET = 10
            WRITE (MSGTXT,1035) MAXCAL
            GO TO 990
            END IF
 50      CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ISURNO, BUFF1, NXBUFF, IRET)
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
 1000 FORMAT ('PCLSOU: ERROR',I3,' OPENING SOURCE TABLE')
 1010 FORMAT ('PCLSOU: ERROR',I3,' READING SOURCE TABLE')
 1040 FORMAT ('   Your calibrators have their fluxes set for FQID ',I3)
 1050 FORMAT ('   You are using them to calibrate FQID ',I3)
 1035 FORMAT ('PCLSOU: ERROR: TOO MANY CALIBRATORS, >',I4)
 1070 FORMAT ('PCLSOU: ERROR',I3,' CLOSING SOURCE TABLE')
      END
      SUBROUTINE PCLNDX (IRET)
C-----------------------------------------------------------------------
C   PCLNDX reads the NX table and finds the number of actual cal scans
C   If it is only 1 it sets SOLINT to half the length of that scan
C   Outputs:
C      IRET   I   > 0 => bad error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PCAL.INC'
      INTEGER   MSGSAV, IREC, NREC, SLAST, NXLUN, IDSOUR, SUBA, LUNTMP,
     *   VST, VEN, FQ, NSCAN, I
      REAL      TT, DTIME, SOLINT
      DOUBLE PRECISION TB, TE, TS, TN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C                                       open - allow non-existant
      MSGSAV = MSGSUP
      MSGSUP = 32000
      NXLUN = LUNTMP (1)
      CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, 1, CATUV, NXLUN,
     *   INXRNO, NXKOLS, NXNUMV, IRET)
      MSGSUP = MSGSAV
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
      ELSE IF (IRET.GT.0) THEN
         GO TO 999
         END IF
C                                       read for cal sources
      NSCAN = 0
      SLAST = -1
      TS = -1000.0D0
      TN = TS
      SOLINT = XSOLIN / (24.0 * 60.0)
      NREC = NXBUFF(5)
      DO 100 IREC = 1,NREC
         CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, TT,
     *      DTIME, IDSOUR, SUBARR, SUBA, VST, VEN, FQ, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       is it a calibrator?
         DO 10 I = 1,NVCAL
            IF (IDSOUR.EQ.VCALID(I)) GO TO 20
 10         CONTINUE
         GO TO 100
C                                       it is a calibrator
 20      TB = TT - DTIME/2.0D0
         TE = TT + DTIME/2.0D0
C                                       same as last time
         IF (IDSOUR.EQ.SLAST) THEN
            IF (TE.LT.TS+SOLINT) THEN
               TN = TE
            ELSE IF (TB.LT.TS+SOLINT) THEN
               TS = TS + SOLINT
               TN = TE
               NSCAN = NSCAN + 1
            ELSE
               NSCAN = NSCAN + 1
               IF (DTIME.LE.SOLINT) THEN
                  TS = TB
                  TN = TE
               ELSE
                  TS = TB
 25               NSCAN = NSCAN + 1
                     TS = TS + SOLINT
                     IF (TE.GT.TS+SOLINT) GO TO 25
                  TN = TE
                  END IF
               END IF
C                                       new source
         ELSE
            SLAST = IDSOUR
            TS = TB
            TN = TE
 30         NSCAN = NSCAN + 1
            IF (TE.GT.TS+SOLINT) THEN
               TS = TS + SOLINT
               GO TO 30
               END IF
            END IF
 100     CONTINUE
C                                       reset SOLINT for one scan case
      IF (NSCAN.EQ.1) THEN
         TB = ((TN - TS) / 3.0D0) * (24.0D0 * 60.0D0)
         WRITE (MSGTXT,1100) XSOLIN, TB
         CALL MSGWRT (5)
         XSOLIN = TB
         END IF
      CALL TABNDX ('CLOS', NXBUFF, INXRNO, NXKOLS, NXNUMV, TT, DTIME,
     *   IDSOUR, SUBARR, SUBA, VST, VEN, FQ, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('PCLNDX: changing SOLINT from',F7.3,' to',F7.3,' minutes')
      END
      SUBROUTINE PNDSPX (NSRC, IPOL, ISRC, NUMIF, VFLUX, VFREQ, NTERM,
     *   SPIX, IERR)
C-----------------------------------------------------------------------
C   PNDSPC fits the spectral index of the specified source to the fluxes
C   provided.
C   Inputs:
C      NSRC     I      Max number sources
C      IPOL     I      Current polarization
C      ISRC     I      Current source
C      VFLUX    R(*)   Fluxes (4 pol, NSRC, NUMIF)
C      VFREQ    D(*)   Frequencies Hz
C      NTERM    I      =2 => return curvature in SPIX(3)
C   Outputs:
C      SPIX     R(3)   Spectral index: (1) Flux at 1 GHz
C                         (2) spectral index
C                         (3) curvature (wrt 1 GHz) or zero
C      IERR     I      Error code: 0 okay, 1 no answer, 2 IO troubles,
C                      3 input troubles
C-----------------------------------------------------------------------
      INTEGER   NSRC, IPOL, ISRC, NUMIF, NTERM, IERR
      REAL      VFLUX(4,NSRC,*), SPIX(*)
      DOUBLE PRECISION VFREQ(*)
C
      INCLUDE 'INCS:DCHND.INC'
      INTEGER   JIF, NIF, I
      DOUBLE PRECISION X(MAXIF), Y(MAXIF), W(MAXIF), BFIT(6),
     *   SX, SY, SXX, SXY, SX3, SX4, SXXY, DEN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      SPIX(1) = 0.0
      SPIX(2) = 0.0
      IF (NTERM.EQ.2) SPIX(3) = 0.0
C                                       count valid fluxes
      NIF = 0
      DO 10 JIF = 1,NUMIF
         IF ((VFLUX(IPOL,ISRC,JIF).GT.0.0) .AND.
     *      (VFLUX(IPOL,ISRC,JIF).NE.FBLANK)) THEN
            NIF = NIF + 1
            END IF
 10      CONTINUE
      JIF = 0
      SX = 0.0D0
      SY = 0.0D0
      SXX = 0.0D0
      SXY = 0.0D0
      SX3 = 0.0D0
      SX4 = 0.0D0
      SXXY = 0.0D0
      DO 50 I = 1,NIF
         IF ((VFLUX(IPOL,ISRC,I).NE.0.0) .AND.
     *      (VFLUX(IPOL,ISRC,I).NE.FBLANK)) THEN
            JIF = JIF + 1
            Y(JIF) = LOG10 (VFLUX(IPOL,ISRC,I))
            X(JIF) = LOG10 (VFREQ(I)) - 9.0D0
            W(JIF) = 1.0D0
            SX  = SX  + X(JIF)
            SXX = SXX + X(JIF)*X(JIF)
            SY  = SY  + Y(JIF)
            SXY = SXY + X(JIF)*Y(JIF)
            SX3 = SX3 + X(JIF)*X(JIF)*X(JIF)
            SX4 = SX4 + X(JIF)*X(JIF)*X(JIF)*X(JIF)
            SXXY = SXXY + X(JIF)*X(JIF)*Y(JIF)
            END IF
 50      CONTINUE
C                                       curvature
      IF (NTERM.EQ.2) THEN
         DEN = SX * SX - JIF * SXX
         IF ((JIF.EQ.2) .AND. (DEN.NE.0.0D0)) THEN
            BFIT(1) = (SX * SXY - SXX * SY) / DEN
            BFIT(2) = (SX * SY - JIF * SXY) / DEN
            BFIT(3) = 0.0D0
         ELSE IF (JIF.GE.3) THEN
            DEN = (SX*SXX-JIF*SX3) * (SXX*SX3-SX*SX4) -
     *         (SXX*SXX-JIF*SX4) * (SXX*SXX-SX*SX3)
            BFIT(1) = ((SXX*SXY-SX3*SY) * (SXX*SX3-SX*SX4) -
     *         (SXX*SXXY-SX4*SY) * (SXX*SXX-SX*SX3)) / DEN
            BFIT(2) = ((SXX*SXXY-SX4*SY) * (SX*SXX-JIF*SX3) -
     *         (SXX*SXY-SX3*SY) * (SXX*SXX-JIF*SX4)) / DEN
            BFIT(3) = ((SX*SY-JIF*SXY) * (SXX*SX-JIF*SX3) -
     *         (SXX*SY-JIF*SXXY) * (SX*SX-JIF*SXX)) /
     *         ((SX*SXX-JIF*SX3) * (SXX*SX-JIF*SX3) -
     *         (SX*SX-JIF*SXX) * (SXX*SXX-JIF*SX4))
         ELSE
            GO TO 990
            END IF
         IERR = 0
         SPIX(1) = 10.D0 ** BFIT(1)
         SPIX(2) = BFIT(2)
         SPIX(3) = BFIT(3)
         WRITE (MSGTXT,1050) SPIX(1), BFIT(2), BFIT(3), ISRC
         CALL MSGWRT (3)
C                                       simple spectral index
      ELSE
         DEN = SX * SX - JIF * SXX
         IF ((JIF.GE.2) .AND. (DEN.NE.0.0D0)) THEN
            BFIT(1) = (SX * SXY - SXX * SY) / DEN
            BFIT(2) = (SX * SY - JIF * SXY) / DEN
         ELSE
            GO TO 990
            END IF
         IERR = 0
         SPIX(1) = 10.0D0 ** BFIT(1)
         SPIX(2) = BFIT(2)
         WRITE (MSGTXT,1060) SPIX(1), BFIT(2), ISRC
         CALL MSGWRT (3)
         END IF
      GO TO 999
C
 990  MSGTXT = 'TOO LITTLE DATA FOR SPECTRAL FIT OR SINGULAR'
      IERR = 1
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('PNDSPX: F1 spix spcurv',F8.3,2F8.4,' for source',I5)
 1060 FORMAT ('PNDSPX finds flux & spectral index',2F7.3,
     *   ' for source',I5)
      END
      SUBROUTINE PSOSPX (SNAME, DATE, FREQ, NTERM, SPXPRM, IERR)
C-----------------------------------------------------------------------
C   PSOSPX looks for standard source spectral parameters
C   Input:
C      SNAME    C*(*)   Source name
C      DATE     C*8     Date obs
C      FREQ     D       A frequency Hz
C   Output:
C      NTERM    I      set to 4 if standard source
C      SPXPRM   R(5)   Flux at 1 GHz, spix, curve(3)
C      IERR     I      0 -> standard source, -1 not standard source
C-----------------------------------------------------------------------
      CHARACTER SNAME*(*), DATE*8
      INTEGER   NTERM, IERR
      REAL      SPXPRM(5)
      DOUBLE PRECISION FREQ
C
      INTEGER   XSOUR, NDATES, LXSOUR
      PARAMETER (XSOUR=5, NDATES=17, LXSOUR=6)
C
      INTEGER   I, J, ISRC, ID(3), IDNUM, LSRC, ICTYPE, JTRIM
      REAL      TCOEFF(4,XSOUR), DATES(NDATES), DCOEFF(4,NDATES,3), DD,
     *   W1, W2, LCOEFF(5,LXSOUR), SCOEFF(4,3)
      CHARACTER KNOSOU(4,XSOUR)*16, LNOSOU(4,LXSOUR)*16,
     *   SNOSOU(3,3)*16
      DOUBLE PRECISION DT
      INCLUDE 'INCS:DMSG.INC'
C                                       steady sources Perley 2013
C                                       3C123
      DATA SCOEFF / 1.8077, -0.8018, -0.1157, 0.0,
C                                       3C196
     *   1.2969, -0.8690, -0.1788, 0.0305,
C                                       3C295
     *   1.4866, -0.7871, -0.3440, 0.0749/
C                                       Perley 2013 coefficients
C                                       same units as RCOEFF
C                                       3C286
      DATA TCOEFF /1.2515,  -0.4605,  -0.1715,   0.0336,
C                                       3C48 (2010)
     *   1.3197,  -0.7253,  -0.2023,   0.0540,
C                                       3C147 (2010)
     *   1.4428,  -0.6300,  -0.3142,   0.1032,
C                                       3C138 (2010)
     *   1.0053,  -0.4384,  -0.1855,   0.0511,
C                                       1934-638 (Reynolds, 02/Jul/94)
C    *           -30.7667,  26.4908,  -7.0977,   0.605334,
C                                       3C295
     *   1.4866,  -0.7871,  -0.3440,   0.0749 /
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
     *   64.768, -0.387, -0.420,  0.181,  0.000,
C                                       3C147
     *   66.738, -0.022, -1.012,  0.549,  0.000,
C                                       3C196
     *   83.084, -0.699,  0.110,  0.000,  0.000,
C                                       3c380
     *   77.352, -0.767,  0.000,  0.000,  0.000,
C                                       3C295
     *   97.763, -0.582, -0.298,  0.583, -0.363/
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
      IERR = 0
      ICTYPE = 1
      IF (FREQ.LT.0.75D9) ICTYPE = -1
      ISRC = 0
      IF (ICTYPE.EQ.1) THEN
         DO 10 I = 1,3
            DO 5 J = 1,3
               IF (SNAME(:JTRIM(SNOSOU(J,I))).EQ.SNOSOU(J,I))
     *            ISRC = I + XSOUR
 5             CONTINUE
 10         CONTINUE
         DO 20 I = 1,XSOUR
            DO 15 J = 1,4
               IF (SNAME(:JTRIM(KNOSOU(J,I))).EQ.KNOSOU(J,I)) ISRC = I
 15            CONTINUE
 20         CONTINUE
      ELSE
         DO 40 I = 1,LXSOUR
            DO 30 J = 1,4
               IF (SNAME(:JTRIM(LNOSOU(J,I))).EQ.LNOSOU(J,I)) ISRC = I
 30            CONTINUE
 40         CONTINUE
         END IF
C                                       non-standard source
      IF (ISRC.LE.0) THEN
         IERR = -1
C                                       low frequency
      ELSE IF (ICTYPE.EQ.-1) THEN
C                                       return wrt 1 GHz, not 150 MHz
         DT = LOG10 (1.0D3 / 150.0D0)
         SPXPRM(2) = LCOEFF(2,ISRC) + 2.D0*DT*LCOEFF(3,ISRC) +
     *      3.D0*DT*DT*LCOEFF(4,ISRC) + 4.D0*DT*DT*DT * LCOEFF(5,ISRC)
         SPXPRM(3) = LCOEFF(3,ISRC) + 3.D0*DT*LCOEFF(4,ISRC) +
     *      6.D0*DT*DT*LCOEFF(5,ISRC)
         SPXPRM(4) = LCOEFF(4,ISRC) + 4.D0*DT*LCOEFF(5,ISRC)
         SPXPRM(5) = LCOEFF(5,ISRC)
         SPXPRM(1) = -DT*SPXPRM(2) + DT*DT*SPXPRM(3) -
     *      DT*DT*DT*SPXPRM(4) + DT*DT*DT*DT*SPXPRM(5)
         SPXPRM(1) = LCOEFF(1,ISRC) * (10.0 ** SPXPRM(1))
C                                       stable ones
      ELSE IF (ISRC.GT.XSOUR) THEN
         ISRC = ISRC - XSOUR
         SPXPRM(1) = 10.0 ** SCOEFF(1,ISRC)
         SPXPRM(2) = SCOEFF(2,ISRC)
         SPXPRM(3) = SCOEFF(3,ISRC)
         SPXPRM(4) = SCOEFF(4,ISRC)
         SPXPRM(5) = 0.0
C                                       3C286, 3C295 stable
      ELSE IF ((ISRC.EQ.1) .OR. (ISRC.EQ.5)) THEN
         SPXPRM(1) = 10.0 ** TCOEFF(1,ISRC)
         SPXPRM(2) = TCOEFF(2,ISRC)
         SPXPRM(3) = TCOEFF(3,ISRC)
         SPXPRM(4) = TCOEFF(4,ISRC)
         SPXPRM(5) = 0.0
C                                       time variable
      ELSE IF (ISRC.GT.0) THEN
         CALL DATEST (DATE, ID)
         CALL DAYNUM (ID(1), ID(3), ID(2), IDNUM)
         DD = ID(1) + IDNUM/365.25
         LSRC = ISRC - 1
         IF ((DD.LE.DATES(1)) .OR. (DD.GE.DATES(NDATES))) THEN
            I = NDATES
            IF (DD.LE.DATES(1)) I = 1
            SPXPRM(1) = 10.0 ** DCOEFF(1,I,LSRC)
            SPXPRM(2) = DCOEFF(2,I,LSRC)
            SPXPRM(3) = DCOEFF(3,I,LSRC)
            SPXPRM(4) = DCOEFF(4,I,LSRC)
            SPXPRM(5) = 0.0
C                                       interpolate
         ELSE
            DO 50 I = 2,NDATES
               IF (DD.LT.DATES(I)) THEN
                  W1 = (DATES(I) - DD) / (DATES(I) - DATES(I-1))
                  W2 = 1.0 - W1
                  SPXPRM(1) = W2 * DCOEFF(1,I,LSRC) +
     *               W1 * DCOEFF(1,I-1,LSRC)
                  SPXPRM(1) = 10.0 ** SPXPRM(1)
                  SPXPRM(2) = W2 * DCOEFF(2,I,LSRC) +
     *               W1 * DCOEFF(2,I-1,LSRC)
                  SPXPRM(3) = W2 * DCOEFF(3,I,LSRC) +
     *               W1 * DCOEFF(3,I-1,LSRC)
                  SPXPRM(4) = W2 * DCOEFF(4,I,LSRC) +
     *               W1 * DCOEFF(4,I-1,LSRC)
                  SPXPRM(5) = 0.0
                  GO TO 900
                  END IF
 50            CONTINUE
            END IF
         END IF
C
 900  IF (IERR.EQ.0) THEN
         NTERM = 4
         WRITE (MSGTXT,1900) SNAME
         CALL MSGWRT (3)
         WRITE (MSGTXT,1901) SPXPRM(1), SPXPRM(2), SPXPRM(3), SPXPRM(4)
         CALL MSGWRT (3)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('PSOSPX: Spectral index for standard source ',A)
 1901 FORMAT ('        F1GHz spix curv',F8.2,F8.3,2F8.4)
      END
      SUBROUTINE PCLSMI
C-----------------------------------------------------------------------
C   PCLSMI inits the spectral smoothing done for spectral-dependent
C   solutions
C   Outputs in common:
C      XINTP    R(3)   Smoothing parameters - defaults filled in
C
C-----------------------------------------------------------------------
      INTEGER   SMOMAX
      PARAMETER (SMOMAX=1024)
C
      INCLUDE 'PCAL.INC'
      INTEGER   IT, I, N
      REAL      FX, X, W, WIDTHS(4), SUPS(4)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA WIDTHS /4.0, 2.0, 2.0, 3.0/
      DATA SUPS /1.0, 3.0, 1.0, 4.0/
C-----------------------------------------------------------------------
C                                       type and defaults
      IT = XINTP(1) + 0.1
      IT = MAX (0, IT)
      IF (IT.GT.4) IT = 1
      XINTP(1) = IT
      IF (IT.EQ.0) THEN
         XINTP(2) = 0.0
         XINTP(3) = 0.0
         PSMRAD = 0
      ELSE
         N = CATBLK(KINAX+JLOCF)
         IF ((XINTP(2).LT.0.5) .OR. (XINTP(2).GT.N/3.0)) XINTP(2) =
     *      WIDTHS(IT)
         IF ((XINTP(3).GT.4.0*SUPS(IT)*XINTP(2)) .OR.
     *      (XINTP(3).LT.XINTP(2))) XINTP(3) = XINTP(2) * SUPS(IT)
         PSMRAD = XINTP(3) / 2.0 + 0.1
         IF (PSMRAD.GT.SMOMAX) THEN
            PSMRAD = SMOMAX - 1
            XINTP(3) = 2.0* PSMRAD
            XINTP(2) = XINTP(3) / SUPS(IT)
            END IF
         END IF
      N = PSMRAD + 1
      FX = 2.0 / XINTP(2)
      W = 1.0
      PSMTAB(1) = 1.0
C                                       Hanning smooth
      IF (IT.EQ.1) THEN
         DO 20 I = 2,N
            X = I - 1.0
            PSMTAB(I) = MAX (0.0, 1.0-FX*X)
            W = W + 2 * PSMTAB(I)
 20         CONTINUE
C                                       Gaussian smooth
      ELSE IF (IT.EQ.2) THEN
         FX = -LOG(2.0) * FX * FX
         DO 30 I = 2,N
            X = I - 1.0
            PSMTAB(I) = EXP (FX * X * X)
            W = W + 2 * PSMTAB(I)
 30         CONTINUE
C                                       Boxcar smooth
      ELSE IF (IT.EQ.3) THEN
         FX = 1.0 / FX
         DO 40 I = 2,N
            X = I - 1.0
            IF (X.LT.FX) THEN
               PSMTAB(I) = 1.0
            ELSE IF (X.EQ.FX) THEN
               PSMTAB(I) = 0.5
               END IF
            W = W + 2 * PSMTAB(I)
 40         CONTINUE
C                                      Sinc smooth
      ELSE IF (IT.EQ.4) THEN
         FX = 3.14159 * FX
         DO 50 I = 2,N
            X = (I - 1.0) * FX
            PSMTAB(I) = SIN(X) / X
            W = W + 2 * PSMTAB(I)
 50         CONTINUE
         END IF
C                                       Normalize integral
      IF (W.LE.0.0) W = 1.0
      DO 70 I = 1,N
         PSMTAB(I) = PSMTAB(I) / W
 70      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PCLUV (NC, NI, NP, NA, DTERMS, STERMS, IRET)
C-----------------------------------------------------------------------
C   PCLUV fits model parameters and enters them into the AN table.
C   Inputs:
C      NC       I      Number spectral channels
C      NI       I      Number IFs
C      NP       I      Number polarizations (2 always I suspect)
C      NA       I      Max antenna number
C   Output:
C      DTERMS   R(*)   big array to hold it all: (2,NC,NI,NP,NA)
C      STERMS   R(*)   source fluxes (3,NC,NI,source#)
C      IRET     I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NC, NI, NP, NA, IRET
      REAL      DTERMS(2,NC,NI,NP,*), STERMS(4,NC,NI,*)
C
      INCLUDE 'PCAL.INC'
      HOLLERITH CATINH(256)
      INTEGER   VERTMP, LUNNEW, LUNOLD, I, BIFT, EIFT, MSGSAV, IFNO,
     *   LOOPA, LOOPC, PRTLV, IREF, IROUND, SOLTYP, MXNCAL, NCH, LOOPIF,
     *   LIMIF1, LIMIF2, NIF, IPD, MAXIT, SAVEIF, SAVBIF, J, ICHAN,
     *   IFP(2,MAXANT), IFT(2,MAXANT), IFVST(4,MAXCAL), ITEMP, INNSCR,
     *   LRET, KRET, TMPDSK, TMPCNO
      LOGICAL   ALLFIT, FIXMOD, FITANT, SLCTD, GOTONE
      REAL      PD, RFCTOL, XCTOL, VERR(4,MAXCAL), PHI(2,MAXANT),
     *   THETA(2,MAXANT), ORI(2,MAXIF,MAXANT), ELP(2,MAXIF,MAXANT), X
      COMPLEX   DDD(2,MAXANT)
      DOUBLE PRECISION FRATIO
      CHARACTER PDSOLT*8
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATIN, CATINH)
      DATA LUNNEW, LUNOLD /57,58/
C-----------------------------------------------------------------------
      IF (JLOCIF.GE.0) THEN
         NIF = CATBLK(KINAX+JLOCIF)
      ELSE
         NIF = 1
         END IF
      SAVBIF = BIF
      SAVEIF = EIF
      GOTONE = .FALSE.
C                                       get previous soultions: line
      IF (CPARM(7).GT.0.0) THEN
         IF (DOLINE) THEN
            CALL GETPDS (DISKIN, CNOIN, CATIN, SUBARR, FRQSEL, NC, NI,
     *         NP, NA, DTERMS, PHDIFF, PDSOLT, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       get old full model
            CALL GETCPS (DISKIN, CNOIN, CATIN, CPVER, FRQSEL, NC, NI,
     *         NVCAL, CSIDNO, CSNAME, STERMS, IRET)
            IF (IRET.NE.0) GO TO 999
            DO 5 I = 1,NVCAL
               VSOUID(CSIDNO(I)) = I
 5             CONTINUE
C                                       Save solutions: continuum
         ELSE
            DO 20 LOOPA = 1,MAXANT
               DO 10 LOOPIF = 1,MAXIF
                  ORI(1,LOOPIF,LOOPA) = STNORI(1,LOOPIF,LOOPA)
                  ORI(2,LOOPIF,LOOPA) = STNORI(2,LOOPIF,LOOPA)
                  ELP(1,LOOPIF,LOOPA) = STNELP(1,LOOPIF,LOOPA)
                  ELP(2,LOOPIF,LOOPA) = STNELP(2,LOOPIF,LOOPA)
 10               CONTINUE
 20            CONTINUE
C                                       Get old phase differences from
C                                       AN table
            CALL PDRGET (DISKIN, CNOIN, SUBARR, LUNOLD, CATIN, NIF,
     *         ITEMP, PHDIFF, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
      ELSE
         IF (DOLINE) THEN
            LOOPA = 2 * NC * NI * NP * NA
            CALL RFILL (LOOPA, 0.0, DTERMS)
            LOOPA = 4 * NC * NI * 30
            CALL RFILL (LOOPA, 0.0, STERMS)
         ELSE
            LOOPA = 2 * MAXIF * MAXANT
            CALL RFILL (LOOPA, 0.0, ORI)
            CALL RFILL (LOOPA, 0.0, ELP)
            CALL RFILL (MAXIF, 0.0, PHDIFF)
            END IF
         END IF
      IF (EIF.LE.0) EIF = NIF
      BIFT = BIF
      EIFT = EIF
      IF (AVGIF) EIFT = BIFT
      VISDSK = 0
      VISCNO = 0
C                                       Set up for common IF solution.
C                                       Loop over IF
      IREF = IROUND (XREF)
      IF (DOLINE) THEN
         NCH = NC
      ELSE
         NCH = 1
         END IF
      LRET = 0
      KRET = 0
      DO 500 IFNO = BIFT,EIFT
         IF (DOLINE) THEN
            TMPDSK = 0
            TMPCNO = 0
         ELSE
            TMPDSK = -1
            TMPCNO = -1
            END IF
         INNSCR = NSCR
         DO 490 ICHAN = 1,NCH
            IF (AVGIF) THEN
               LIMIF1 = BIF
               LIMIF2 = EIF
            ELSE
               LIMIF1 = IFNO
               LIMIF2 = IFNO
               END IF
C                                       Antenna list
            DO 30 LOOPA = 1,50
               ANTENS(LOOPA) = IROUND (XANTS(LOOPA))
 30            CONTINUE
C                                       adjust ref fluxes
            IF (DOLINE) THEN
               DO 40 I = 1,NVCAL
                  IF ((CPVER.GT.0) .AND. (STERMS(1,ICHAN,IFNO,I).GT.0.))
     *               THEN
                     CALL RCOPY (4, STERMS(1,ICHAN,IFNO,I), CVFLUX(1,I))
                  ELSE
                     CALL RCOPY (4, VFLUX(1,I,IFNO), CVFLUX(1,I))
                     IF (DOSCAL.GE.0.0) THEN
                        FRATIO = FREQSO(IFNO,I) + (ICHAN-REFPIX) *
     *                     VFINC(IFNO)
                        FRATIO = FRATIO / 1.D9
                        X = LOG10 (FRATIO)
                        FRATIO = X*SSPECT(2,1,I) + X*X*SSPECT(3,1,I) +
     *                     X*X*X*SSPECT(4,1,I) + X*X*X*X*SSPECT(5,1,I)
                        X = 10.0 ** FRATIO
                        CVFLUX(1,I) = SSPECT(1,1,I) * X
                        END IF
                     END IF
 40               CONTINUE
            ELSE
               I = 4 * NVCAL
               CALL RCOPY (I, VFLUX(1,1,IFNO), CVFLUX)
               END IF
C                                       get the data
            CALL PCLSEL (TMPDSK, TMPCNO, IFNO, ICHAN, IRET)
C                                       Check if any data:
            IF (IRET.EQ.-1) GO TO 480
            IF (IRET.NE.0) GO TO 999
            PRTLV = XPRTLV + 0.1
            GOTONE = .TRUE.
C                                       Fit for parameters
            IF (XSOLTY.EQ.'ORI-') THEN
C                                       Setup for ori-elp:
               ALLFIT = .NOT.CLNMOD
               DO 100 LOOPC = 1,NVCAL
                  ALLFIT = ALLFIT .AND. VDOFIT(LOOPC)
                  IFVST(1,LOOPC) = 0
                  IFVST(2,LOOPC) = 0
                  IFVST(3,LOOPC) = 0
                  IFVST(4,LOOPC) = 0
                  IF (ABS (VFLUX(1,LOOPC,IFNO)).GT.1.0E-30)
     *               IFVST(1,LOOPC) = 1
                  IF (.NOT.VDOFIT(LOOPC)) IFVST(2,LOOPC) = 1
                  IF (.NOT.VDOFIT(LOOPC)) IFVST(3,LOOPC) = 1
C                                       Fix V pol
                  IFVST(4,LOOPC) = 1
 100              CONTINUE
C                                       Set flags for feed parms
               DO 110 LOOPA = 1,NUMANT
                  IFP(1,LOOPA) = 0
                  IFP(2,LOOPA) = 0
                  IFT(1,LOOPA) = 0
                  IFT(2,LOOPA) = 0
C                                       Initial feed parameters
                  IF (DOLINE) THEN
                     THETA(1,LOOPA) = DTERMS(1,ICHAN,IFNO,1,LOOPA)
                     THETA(2,LOOPA) = DTERMS(1,ICHAN,IFNO,2,LOOPA)
                     PHI(1,LOOPA) = DTERMS(2,ICHAN,IFNO,1,LOOPA)
                     PHI(2,LOOPA) = DTERMS(2,ICHAN,IFNO,2,LOOPA)
                  ELSE
                     PHI(1,LOOPA) = ORI(1,IFNO,LOOPA)
                     PHI(2,LOOPA) = ORI(2,IFNO,LOOPA)
                     THETA(1,LOOPA) = ELP(1,IFNO,LOOPA)
                     THETA(2,LOOPA) = ELP(2,IFNO,LOOPA)
                     END IF
C                                       Default R and L
C                                       BPARM(1)>0 => use default.
                  IF ((BPARM(1).GT.0.0) .OR.
     *               ((ABS (THETA(1,LOOPA)).LT.0.0001) .AND.
     *               (ABS (THETA(1,LOOPA)).LT.0.0001))) THEN
                     THETA(1,LOOPA) = 0.785398164
                     THETA(2,LOOPA) = THETA(1,LOOPA) - 1.570796327
                     END IF
 110              CONTINUE
C                                       Reference ant. - if any.
               IF ((IREF.GT.0) .AND. (ALLFIT)) IFP(1,IREF) = 1
C                                     BPARM(3) - "R-L phase diff."
               IF (BPARM(3).GT.0.) THEN
                  IPD = 0
                  PD = BPARM(4)
               ELSE
                  IPD = 1
                  END IF
C                                     BPARM(5)=0 => solve for V.
               IF (BPARM(5).GT.0.) THEN
               ELSE
C                                       Fix I and Vpol
                  IF (NVCAL.GT.0) THEN
                     DO 125 I = 1,NVCAL
                        IFVST(1,I) = 1
                        IFVST(4,I) = 1
 125                    CONTINUE
                     END IF
                  END IF
C                                       BPARM(6) >0 => fix ref antenna
C                                       orientation 1st poln
               IF (IREF.LE.0) BPARM(6) = 0.
               IF (BPARM(6).GT.0.) THEN
                  IFP(1,IREF) = 1
                  PHI(1,IREF) = 0.
                  END IF
C                                       BPARM(7) >0 => fix ref antenna
C                                       orientation 2nd poln
               IF (IREF.LE.0) BPARM(7) = 0.
               IF (BPARM(7).GT.0.) THEN
                  IFP(2,IREF) = 1
                  PHI(2,IREF) = 0.
                  END IF
C                                       BPARM(8) =>Hold all orientations
C                                       fixed?
               IF (BPARM(8).GT.0.) THEN
                  DO 130 I = 1,NUMANT
                     IFP(1,I) = 1
                     IFP(2,I) = 1
 130                 CONTINUE
                  END IF
C                                       BPARM(9) => Hold all
C                                       ellipticities fixed?
               IF (BPARM(9).GT.0.) THEN
                  DO 135 I = 1,NUMANT
                     IFT(1,I) = 1
                     IFT(2,I) = 1
 135                 CONTINUE
                  END IF
C                                        Given PMODEL?
               FIXMOD = IMODEL
               IF (FIXMOD) THEN
C                                       spectral index
                  IF (DOLINE) THEN
                     FRATIO = FREQSO(IFNO,1) + (ICHAN-REFPIX)
     *                  * VFINC(IFNO)
                     FRATIO = FRATIO / FREQSO(1,1)
                  ELSE
                     FRATIO = FREQSO(IFNO,1) / FREQSO(1,1)
                     END IF
                  CVFLUX(1,1) = XSMOD(1) * (FRATIO**RSPECT(1,1))
                  CVFLUX(2,1) = XSMOD(2) * (FRATIO**RSPECT(2,1))
                  CVFLUX(3,1) = XSMOD(3) * (FRATIO**RSPECT(3,1))
                  CVFLUX(4,1) = XSMOD(4) * (FRATIO**RSPECT(4,1))
                  END IF
C                                       BPARM(10) => Hold all
C                                       source parameters fixed?
               IF (BPARM(10).GT.0.) THEN
                  FIXMOD = .TRUE.
                  DO 140 I = 1,NVCAL
                     IFVST(1,I) = 1
                     IFVST(2,I) = 1
                     IFVST(3,I) = 1
                     IFVST(4,I) = 1
 140              CONTINUE
                  END IF
C                                       CPARM(8) = max no. iterations
               IF (CPARM(8).GT.0.) THEN
                  MAXIT = CPARM(8)
               ELSE
                  MAXIT = 250
                  END IF
C                                       CPARM(9) [0,.01) then
               IF ((CPARM(9).GT.0.0).AND.(CPARM(9).LE.1E-2)) THEN
                  RFCTOL = CPARM(9)
               ELSE
                  RFCTOL = 1.0E-4
                  END IF
C                                       CPARM(10) [0,.01) then
               IF ((CPARM(10).GT.0.0).AND.(CPARM(10).LE.1.0E-2)) THEN
                  XCTOL = CPARM(10)
               ELSE
                  XCTOL = 1E-4
                  END IF
C                                        Do solution
               SOLTYP = 2
               CALL IPCALC (NUMDAT, CVOBS, VWT, PARAN, ANTS, NUMANT,
     *            PHI, THETA, IFP, IFT, CVMOD, NVCAL, CVFLUX, IFVST,
     *            VSOU, IREF, VERR, PD, IPD, PRTLV, MAXIT, RFCTOL,
     *            XCTOL, FIXMOD, BPARM(2), IRET)
               IF (IRET.GT.1) GO TO 999
               IF (IRET.EQ.1) THEN
                  LRET = LRET + 1
                  KRET = KRET + 1
                  IF (LRET.EQ.256) GO TO 999
                  I = 4 * NVCAL
                  CALL RFILL (I, FBLANK, CVFLUX)
                  I = 2 * NUMANT
                  CALL RFILL (I, FBLANK, THETA)
                  CALL RFILL (I, FBLANK, PHI)
                  IRET = 0
               ELSE
                  LRET = 0
                  END IF
C                                       save flux
               IF (.NOT.DOMODL) THEN
                  IF (DOLINE) THEN
                     DO 145 I = 1,NVCAL
                        STERMS(1,ICHAN,IFNO,I) = CVFLUX(1,I)
                        STERMS(2,ICHAN,IFNO,I) = CVFLUX(2,I)
                        STERMS(3,ICHAN,IFNO,I) = CVFLUX(3,I)
                        STERMS(4,ICHAN,IFNO,I) = CVFLUX(4,I)
 145                    CONTINUE
                  ELSE
                     I = 4 * NVCAL
                     CALL RCOPY (I, CVFLUX, VFLUX(1,1,IFNO))
                     END IF
                  END IF
C                                       Save Feed parameters
               DO 155 LOOPA = 1,NUMANT
                  FITANT = SLCTD (LOOPA, ANTENS, NANTSL, DOAWNT)
                  IF ((DOLINE) .AND. (FITANT)) THEN
                     DTERMS(1,ICHAN,IFNO,1,LOOPA) = THETA(1,LOOPA)
                     DTERMS(1,ICHAN,IFNO,2,LOOPA) = THETA(2,LOOPA)
                     DTERMS(2,ICHAN,IFNO,1,LOOPA) = PHI(1,LOOPA)
                     DTERMS(2,ICHAN,IFNO,2,LOOPA) = PHI(2,LOOPA)
                  ELSE IF (.NOT.DOLINE) THEN
                     DO 150 LOOPIF = LIMIF1,LIMIF2
                        ELP(1,LOOPIF,LOOPA) = THETA(1,LOOPA)
                        ELP(2,LOOPIF,LOOPA) = THETA(2,LOOPA)
                        ORI(1,LOOPIF,LOOPA) = PHI(1,LOOPA)
                        ORI(2,LOOPIF,LOOPA) = PHI(2,LOOPA)
 150                    CONTINUE
                     END IF
 155              CONTINUE
               IF (DOLINE) THEN
                  I = ICHAN + (IFNO-1) * NCH
               ELSE
                  I = IFNO
                  END IF
               PHDIFF(I) = PD
C                                       Linear approximation for feeds:
            ELSE
               ITEMP = NVCAL
               IF (DOMODL) ITEMP = 0
C                                       X-Y Feeds
               IF (ISXY) THEN
                  SOLTYP = 3
                  CALL XYCALC (NUMDAT, CVOBS, VWT, PARAN, ANTS, NUMANT,
     *               DDD, CVMOD, ITEMP, CVFLUX, VSOU, IREF, CSNAME,
     *               IRET)
C                                       Circularly polarized feeds
C                                       Resolved source variant
               ELSE IF (XSOLTY.EQ.'RAPR') THEN
                  SOLTYP = 4
                  CALL RPCALC (NUMDAT, CVOBS, VWT, PARAN, ANTS, NUMANT,
     *               DDD, CVMOD, ITEMP, CVFLUX, VSOU, IREF, PRTLV,
     *               CSNAME, IRET)
C                                       Point source variant
               ELSE
                  SOLTYP = 1
                  CALL LPCALC (NUMDAT, CVOBS, VWT, PARAN, ANTS, NUMANT,
     *               DDD, CVMOD, ITEMP, CVFLUX, VSOU, IREF, CSNAME,
     *               IRET)
                  END IF
               IF (IRET.GT.1) GO TO 999
               IF (IRET.EQ.1) THEN
                  LRET = LRET + 1
                  KRET = KRET + 1
                  IF (LRET.EQ.256) GO TO 999
                  I = 4 * NVCAL
                  CALL RFILL (I, FBLANK, CVFLUX)
                  DO 165 LOOPA = 1,NUMANT
                     DDD(1,LOOPA) = CMPLX (FBLANK, FBLANK)
                     DDD(2,LOOPA) = CMPLX (FBLANK, FBLANK)
 165                 CONTINUE
                  IRET = 0
               ELSE
                  LRET = 0
                  END IF
C                                       save flux
               IF (.NOT.DOMODL) THEN
                  IF (DOLINE) THEN
                     DO 170 I = 1,NVCAL
                        STERMS(1,ICHAN,IFNO,I) = CVFLUX(1,I)
                        STERMS(2,ICHAN,IFNO,I) = CVFLUX(2,I)
                        STERMS(3,ICHAN,IFNO,I) = CVFLUX(3,I)
                        STERMS(4,ICHAN,IFNO,I) = CVFLUX(4,I)
 170                    CONTINUE
                  ELSE
                     I = 4 * NVCAL
                     CALL RCOPY (I, CVFLUX, VFLUX(1,1,IFNO))
                     END IF
                  END IF
C                                       Save Feed parameters
               DO 200 LOOPA = 1,NUMANT
                  FITANT = SLCTD (LOOPA, ANTENS, NANTSL, DOAWNT)
                  IF ((DOLINE) .AND. (FITANT)) THEN
                     DTERMS(1,ICHAN,IFNO,1,LOOPA) = REAL (DDD(1,LOOPA))
                     DTERMS(1,ICHAN,IFNO,2,LOOPA) = REAL (DDD(2,LOOPA))
                     DTERMS(2,ICHAN,IFNO,1,LOOPA) = AIMAG (DDD(1,LOOPA))
                     DTERMS(2,ICHAN,IFNO,2,LOOPA) = AIMAG (DDD(2,LOOPA))
                  ELSE IF (.NOT.DOLINE) THEN
                     DO 190 LOOPIF = LIMIF1,LIMIF2
                        ELP(1,LOOPIF,LOOPA) = REAL (DDD(1,LOOPA))
                        ELP(2,LOOPIF,LOOPA) = REAL (DDD(2,LOOPA))
                        ORI(1,LOOPIF,LOOPA) = AIMAG (DDD(1,LOOPA))
                        ORI(2,LOOPIF,LOOPA) = AIMAG (DDD(2,LOOPA))
 190                    CONTINUE
                     END IF
 200              CONTINUE
               END IF
C                                       Fitted fluxes', phase differences
            IF (AVGIF) THEN
               DO 201 LOOPIF = LIMIF1,LIMIF2
                  PHDIFF(LOOPIF) = PHDIFF(IFNO)
 201              CONTINUE
               DO 202 LOOPC = 1,NVCAL
                  DO 204 LOOPIF = LIMIF1,LIMIF2
                     VFLUX(2,LOOPC,LOOPIF) = VFLUX(2,LOOPC,IFNO)
                     VFLUX(3,LOOPC,LOOPIF) = VFLUX(3,LOOPC,IFNO)
                     VFLUX(4,LOOPC,LOOPIF) = VFLUX(4,LOOPC,IFNO)
 204                 CONTINUE
 202              CONTINUE
               END IF
            IF ((PRTLV.GT.0) .AND. (.NOT.DOLINE)) THEN
               DO 210 LOOPC = 1,NVCAL
                  WRITE (MSGTXT,1200) CSNAME(LOOPC),
     *               (VFLUX(J,LOOPC,IFNO), J = 1,4)
                  CALL MSGWRT (4)
C                                       Errors when available
 210              CONTINUE
               END IF
C                                       clean up
 480        J = NSCR
            DO 485 I = J,INNSCR+1,-1
               LOOPIF = 1
               LOOPC = 2
               IF ((ICHAN.EQ.NCH) .OR. (SCRVOL(I).NE.TMPDSK) .OR.
     *            (SCRCNO(I).NE.TMPCNO)) THEN
                  IF ((I.NE.IONSCR) .OR. (ICHAN.EQ.NCH)) THEN
                     CALL MAPCLR (LOOPIF, SCRVOL(I), SCRCNO(I), LOOPC,
     *                  BUFF2)
                     IF (I.EQ.NSCR) NSCR = NSCR - 1
                     SCRVOL(I) = 0
                     SCRCNO(I) = 0
                     IF (I.EQ.IONSCR) IONSCR = 0
                     END IF
                  END IF
 485           CONTINUE
 490        CONTINUE
 500     CONTINUE
      IF (KRET.GT.0) THEN
         WRITE (MSGTXT,1500) KRET
         CALL MSGWRT (7)
         END IF
      IF (.NOT.GOTONE) THEN
         MSGTXT = 'DID NOT FIND ANY DATA'
         IRET = 8
         GO TO 990
C                                       save ref ant for history
      ELSE IF (.NOT.DOLINE) THEN
         XREF = IREF
C                                       Put calibrator fluxes in SU
C                                       table
         IF ((CPARM(2).GT.0.0) .AND. (.NOT.DOMODL) .AND. (.NOT.SINGLE))
     *      THEN
            MSGTXT = 'Updating source models in SU table'
            CALL MSGWRT (2)
            MXNCAL = MAXCAL
            CALL PUTSOU (NVCAL, VFLUX, VCALID, MXNCAL, SAVBIF, SAVEIF,
     *         DISKIN, CNOIN, CATIN, LUNOLD, FREQID, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Rewrite An table
C                                       Find number of AN tables:
         CALL FNDEXT ('AN', CATIN, VERTMP)
         IF (VERTMP.GT.0) VERTMP = VERTMP + 1
C                                        Save solutions
         DO 630 LOOPA = 1,MAXANT
C                                        Don't update antennas not fitted.
            FITANT = SLCTD (LOOPA, ANTENS, NANTSL, DOAWNT)
            IF (FITANT) THEN
               DO 620 LOOPIF = 1,MAXIF
                  STNORI(1,LOOPIF,LOOPA) = ORI(1,LOOPIF,LOOPA)
                  STNORI(2,LOOPIF,LOOPA) = ORI(2,LOOPIF,LOOPA)
                  STNELP(1,LOOPIF,LOOPA) = ELP(1,LOOPIF,LOOPA)
                  STNELP(2,LOOPIF,LOOPA) = ELP(2,LOOPIF,LOOPA)
 620              CONTINUE
               END IF
 630        CONTINUE
C                                       Copy to a temporary AN table.
C                                       Solutions to AN table
         CALL PUTANT (DISKIN, CNOIN, SUBARR, VERTMP, SAVBIF, SAVEIF,
     *      NIF, SOLTYP, CATIN, IBUFF1, IBUFF2, FREQID, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Put phase differences to
C                                       AN table
         CALL PDRSET (DISKIN, CNOIN, VERTMP, LUNOLD, CATIN, NIF, IREF,
     *      PHDIFF, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Delete old
         CALL RMEXT (DISKIN, CNOIN, 'AN', SUBARR, CATIN, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy back.
         MSGSAV = MSGSUP
         MSGSUP = 31999
         CALL TABCOP ('AN', VERTMP, SUBARR, LUNOLD, LUNNEW, DISKIN,
     *      DISKIN, CNOIN, CNOIN, CATIN, BUFF1, BUFF2, IRET)
         MSGSUP = MSGSAV
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1610) IRET
            GO TO 990
            END IF
C                                       Delete temporary
         CALL RMEXT (DISKIN, CNOIN, 'AN', VERTMP, CATIN, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         GO TO 999
C                                       line solution
      ELSE
         J = NVCAL
         IF (DOMODL) J = 0
         IF (DOINTP) CALL PCLINT (NC, NI, NP, NA, J, IREF, DTERMS,
     *      STERMS)
C                                       save dterms
         CALL SAVPDS (DISKIN, CNOIN, CATIN, SUBARR, FRQSEL, IREF, NC,
     *      NI, NP, NA, DTERMS, PHDIFF, SOLTYP, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       save source model
         IF (.NOT.DOMODL) THEN
            CALL SAVCPS (DISKIN, CNOIN, CATIN, FRQSEL, NC, NI, NVCAL,
     *         CSIDNO, CSNAME, STERMS, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT (A16,' I,Q,U,V=',F9.4,3F9.5,' Jy')
 1500 FORMAT (I7,' SOLUTIONS WERE INDETERMINATE')
 1610 FORMAT ('TABCOP ERROR ',I3,' UPDATING AN TABLE')
      END
      SUBROUTINE PCLSEL (TMPDSK, TMPCNO, IFNO, ICHAN, IRET)
C-----------------------------------------------------------------------
C   PCLSEL will read a multi source data set into a common arrays.
C   Editing and calibration may be applied.
C   In/Out:
C      TMPDSK       I    If 0 and DOLINE, copy this IF to new SC file
C                        with all channels, then return another SC file
C                        with ICHAN in it.  > 0 => use existing SC file
C      TMPCNO       I    The CNO of the larger SC file
C   Input:
C      IFNO         I    IF number (only 1 at a time)
C      ICHAN        I    Desired spectral channel (only 1 at a time)
C   Input via common:
C      XSOLIN       R    Solution interval (min)
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
C      ANTENS(50)   I    List of antennas selected, 0=>all,
C                        any negative => all except those specified
C      FGVER        I    FLAG file version number, if .le. 0 then
C                        NO flagging is applied.
C      CLUSE        I    Cal file version number to apply.
C   Output in common:
C      NUMDAT       I    Number of visibilities in arrays:
C      TIME(*)      R    Times (days) of the visibilities
C      ANTS(2,*)    I    Antenna numbers
C      PARAN(2,*)   R    Parallactic angles (radians)
C      VMOD(2,4,*)  R    Complex polarization model (I,Q,U,V) (Jy)
C      VOBS(2,4,*)  R    Complex observed values (RR,RL,LR,LL)  or
C                        (XX,XY,YX,YY) (Jy).
C      VWT(4,*)     R    "Weights" of the observations.
C      NVCAL        I    Number of calibrator sources
C      VCALID(*)    I    Calibrator ID numbers
C      CVFLUX(4,*   R    Polarized flux densities (I,Q,U,V) (Jy)
C      VDOFIT(*)    L    If true, fit for this calibrator polarization.
C   Output:
C      IRET         I    Error code: 0 => OK,
C                        -1 => end of data
C                        >0 => failed, abort process.
C-----------------------------------------------------------------------
      INTEGER   TMPDSK, TMPCNO, IFNO, ICHAN, IRET
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER BLANK*12
      INTEGER   IA1, IA2, LUN1, LUN2, LSOU, SAVNRP, SAVLRC, NUMBL, IVIS,
     *   CNTBL, IND, ILENBU, KBIND, IPTRI, INIO, IOFF, SAVCR0, I,
     *   CSOUID, IONIND, WIN(4), IONBSZ, IONBO, IONBND, BC, EC
      LOGICAL   T, F, ALLFIT
      INTEGER   MXDAT, LOOP, INDEX, BLNDX, NEXT, VO, BO, FIRST, LIMIT,
     *   LOPCNT, GATNDX(8), GNDX, LOOP2, IIVER, OOVER
      REAL     DUM, LTIMEP, XNORM, IONBUF(UVBFSS), CTIME, TSCEND,
     *   SAVSTK, SOLINT, CATR(256)
      DOUBLE PRECISION CATD(128)
      CHARACTER IFILE*48
      PARAMETER (IONBSZ = 2 * UVBFSS)
      INCLUDE 'PCAL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   SUMCNT(MXBASE), SUMPNT(MXBASE)
      REAL      PANGLE(MAXANT), WT
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSCD.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA BLANK /' '/
      DATA T, F /.TRUE.,.FALSE./
      DATA VO, BO /0,1/
      DATA IONBO /1/
      DATA LUN1,LUN2 /67,68/
      DATA MXDAT /MAXDAT/
      DATA WIN /4*0/
      DATA GATNDX /1,7,10,4, 13,16,19,22/
C-----------------------------------------------------------------------
C                                       See if all sources fitted
C                                       Not fit if model given
      ALLFIT = ((NAME2.EQ.BLANK(1:12)) .AND. (CLAS2.EQ.BLANK(1:6)))
     *   .AND. (.NOT.DOMODL)
      IF (ALLFIT) THEN
         DO 10 LOOP = 1,NVCAL
            VDOFIT(LOOP) = T
10          CONTINUE
         END IF
C                                       Setup
      SOLINT = XSOLIN / (24.0 * 60.0)
      IF (IFNO.LE.0) IFNO = 1
      IF (AVGIF) THEN
         BIF = XBIF + 0.5
         EIF = XEIF + 0.5
      ELSE
         BIF = IFNO
         EIF = IFNO
        END IF
      BCHAN = 1
      ECHAN = ECHANT
      IF (DOLINE) THEN
         BC = MAX (1, ICHAN-PSMRAD)
         EC = MIN (ECHANT, ICHAN+PSMRAD)
      ELSE
         BC = 1
         EC = ECHANT
         END IF
C                                       Message about IF number(s)
      IF (AVGIF) THEN
         WRITE (MSGTXT,1000) BIF, EIF
      ELSE IF (DOLINE) THEN
         WRITE (MSGTXT,1001) IFNO, ICHAN
      ELSE
         WRITE (MSGTXT,1002) IFNO
         END IF
      CALL MSGWRT (2)
C                                       Get data
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy to scratch file.
      IF ((TMPDSK.GT.0) .AND. (TMPCNO.GT.0)) NVIS = TMPCNT
      IF (NVIS.LE.0) THEN
         CALL UVGET ('CLOS', DUM, DUM, IRET)
      ELSE
         CALL PCLCOP (TMPDSK, TMPCNO, VISDSK, VISCNO, BC, EC, ICHAN,
     *      BUFF1, JBUFSZ, IRET)
         IF (IRET.GT.0) GO TO 999
         END IF
C                                       No data - Bail out
      IF (NVIS.LE.0) THEN
         VISDSK = 0
         VISCNO = 0
         VMCNO = 0
         IRET = -1
         WRITE (MSGTXT,1100) IFNO
         IF (DOLINE) WRITE (MSGTXT,1101) IFNO, ICHAN
         GO TO 990
         END IF
C                                       Fitting all poln, no. models
      IF (ALLFIT) THEN
         VMCNO = VISCNO
         SAVNRP = NRPARM
         SAVLRC = LREC
         SAVCR0 = ICOR0
         SAVSTK = CATR(KRCIC+JLOCS)
C                                       Model polarizations
C                                       Create new file with room for
C                                       model.
      ELSE
         CALL COPY (256, CATBLK, SCRCAT)
         SCLREC = LREC
         SCRPRM = NRPARM
         COMPDT = CATBLK(KINAX).EQ.1
         DATDIV = .TRUE.
         IF (COMPDT) THEN
            CALL AXEFND (8, 'WEIGHT  ', SCRCAT(KIPCN), SCRHOL(KHPTP),
     *         WTLOC, IRET)
C                                       Must have this one
            IF ((IRET.NE.0) .OR. (WTLOC.LT.0)) THEN
               IRET = 5
               MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED
     *            DATA'
               CALL MSGWRT (8)
               GO TO 999
               END IF
            IRET = 0
            END IF
         CALL UVDPAD (VISDSK, VISCNO, VMCNO, JBUFSZ, BUFF1, BUFF2, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy relevant portion of IF
C                                       table.
         IIVER = 1
         OOVER = 1
         CALL CHNCOP (IIVER, OOVER, LUN1, LUN2, DISKIN, SCRVOL(VMCNO),
     *      CNOIN, SCRCNO(VMCNO), CATUV, CATBLK, BIF, EIF, FRQSEL,
     *      SFREQS, BUFF1, BUFF2, UBUFF, UBUFF(2049), IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Save header values.
         SAVNRP = NRPARM
         SAVLRC = LREC
         SAVCR0 = ICOR0
         SAVSTK = CATR(KRCIC+JLOCS)
         NRPARM = LREC
         LREC = 2 * SAVLRC - SAVNRP
         ICOR0 = 1
         CATR(KRCIC+JLOCS) = ABS (CATR(KRCIC+JLOCS))
         CATD(KDCRV+JLOCS) = 1.0D0
         CALL CATIO ('UPDT', SCRVOL(VMCNO), SCRCNO(VMCNO), CATBLK,
     *      'REST', BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1102) IRET
            GO TO 990
            END IF
C                                       Model computation
         CALL PCLFLX (IFNO, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      LTIMEP = -1.0E10
      LSOU = -10
C                                       Zero accumulations
      LIMIT = MXDAT
      IF (NVIS.LT.LIMIT) LIMIT = NVIS
      LIMIT = 2 * LIMIT
      CALL FILL (LIMIT, 0, ANTS)
      CALL RFILL (LIMIT, 0.0, PARAN)
      LIMIT = 2 * LIMIT
      CALL RFILL (LIMIT, 0.0, VWT)
      LIMIT = 2 * LIMIT
      CALL RFILL (LIMIT, 0.0, VOBS)
      CALL RFILL (LIMIT, 0.0, VMOD)
C                                       Initialize counters and pointers
      NUMBL = (NUMANT * (NUMANT-1)) / 2
      CALL FILL (NUMBL, 0, SUMCNT)
      CALL FILL (NUMBL, 0, SUMPNT)
      CSOUID = 0
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('SC', SCRVOL(VMCNO), SCRCNO(VMCNO), 1, IFILE, IRET)
      CALL ZOPEN (LUN1, IND, SCRVOL(VMCNO), IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
      ILENBU = 1
      CALL UVINIT ('READ', LUN1, IND, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IPTRI = KBIND
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1121) IRET
         GO TO 990
         END IF
C                                       Open and initialize ionosphere
C                                       file for reading:
      CALL ZPHFIL ('SC', SCRVOL(IONSCR), SCRCNO(IONSCR), 1, IFILE, IRET)
      CALL ZOPEN (LUN2, IONIND, SCRVOL(IONSCR), IFILE, .TRUE., .FALSE.,
     *   .FALSE., IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT, 1122) IRET
         GO TO 990
         END IF
      CALL MINIT ('READ', LUN2, IONIND, 2, NVIS, WIN, IONBUF, IONBSZ,
     *   IONBO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 1123) IRET
         GO TO 990
         END IF
C                                       Read data
      CURSOU = SOUWAN(1)
      IF (CURSOU.LE.0) CURSOU = 1
      NEXT = 1
      FIRST = 1
      CNTBL = 0
      TSCEND = -1.0E10
      LOPCNT = 0
      DO 200 LOOP = 1,NVIS
C                                       Read vis. record.
         CALL UVDISK ('READ', LUN1, IND, BUFF2, INIO, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1124) IRET
            GO TO 990
            END IF
         IPTRI = KBIND
         IOFF = IPTRI + SAVNRP - 1
         IF (INIO.LE.0) GO TO 210
         CTIME = BUFF2(IPTRI+ILOCT)
         IF (TSCEND.LT.-100.0) TSCEND = CTIME + SOLINT
C                                       Read ionosphere record:
         CALL MDISK ('READ', LUN2, IONIND, IONBUF, IONBND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1125) IRET
            GO TO 999
            END IF
C                                       Get source info.
         IF (ILOCSU.GE.0) CURSOU = BUFF2(IPTRI+ILOCSU) + 0.1
         IF (CURSOU.NE.LSOU) THEN
            CALL GETSOU (CURSOU, DISKIN, CNOIN, CATUV, LUN1, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Find cal. number.
            CSOUID = 0
            DO 140 I = 1,NVCAL
C                                       Use source ID if being fitted
               IF ((CURSOU.EQ.VCALID(I)) .AND. VDOFIT(I)) CSOUID = I
C                                        Set source id if passing
C                                        SOLTYPE='ORI-' a model
               IF ((XSOLTY.EQ.'ORI-') .AND. (IMODEL) .AND.
     *            (CURSOU.EQ.VCALID(I))) CSOUID = I
 140           CONTINUE
            END IF
C                                       See if average finished
         IF ((CTIME.GT.TSCEND) .OR. (CURSOU.NE.LSOU)) THEN
            INDEX = FIRST
C                                       Average
            DO 150 IVIS = 1,CNTBL
               XNORM = 1.0
               IF (SUMCNT(IVIS).GT.0) XNORM = 1.0 / SUMCNT(IVIS)
               TIME(INDEX) = TIME(INDEX) * XNORM
               PARAN(1,INDEX) = PARAN(1,INDEX) * XNORM
               PARAN(2,INDEX) = PARAN(2,INDEX) * XNORM
               DO 145 LOOP2 = 1,4
C                                       valid data
                  IF (VWT(LOOP2,INDEX).GT.0.0) THEN
                     XNORM = 1.0 / VWT(LOOP2,INDEX)
                     VOBS(1,LOOP2,INDEX) = VOBS(1,LOOP2,INDEX) * XNORM
                     VOBS(2,LOOP2,INDEX) = VOBS(2,LOOP2,INDEX) * XNORM
C                                       model
                     IF (VSOU(INDEX).LE.0) THEN
                        VMOD(1,LOOP2,INDEX) = VMOD(1,LOOP2,INDEX)*XNORM
                        VMOD(2,LOOP2,INDEX) = VMOD(2,LOOP2,INDEX)*XNORM
                        END IF
                  ELSE
                     VWT(LOOP2,INDEX) = 0.0
                     END IF
 145              CONTINUE
               INDEX = INDEX + 1
 150           CONTINUE
C                                       Reset for next average.
            CNTBL = 0
            FIRST = NEXT
            CALL FILL (NUMBL, 0, SUMCNT)
            CALL FILL (NUMBL, 0, SUMPNT)
            TSCEND = CTIME + SOLINT
            IF (NEXT.GT.MXDAT) GO TO 210
            END IF
         LSOU = CURSOU
         IF (ILOCB.GE.0) THEN
            IA1 = BUFF2(IPTRI+ILOCB) / 256.0 + 0.1
            IA2 = BUFF2(IPTRI+ILOCB) - IA1*256 + 0.1
         ELSE
            IA1 = BUFF2(IPTRI+ILOCA1) + 0.1
            IA2 = BUFF2(IPTRI+ILOCA2) + 0.1
            END IF
C                                       Get baseline pointer.
         BLNDX = ((IA1-1)*NUMANT) - (((IA1+1)*IA1)/2) + IA2
         IF (SUMPNT(BLNDX).LE.0) THEN
            IF (NEXT.GT.MXDAT) GO TO 200
            SUMPNT(BLNDX) = NEXT
            NEXT = NEXT + 1
            CNTBL = CNTBL + 1
            END IF
         LOPCNT = LOPCNT + 1
         INDEX = SUMPNT(BLNDX)
         GNDX = INDEX - FIRST + 1
         SUMCNT(GNDX) = SUMCNT(GNDX) + 1
         ANTS(1,INDEX) = IA1
         ANTS(2,INDEX) = IA2
         VSOU(INDEX) = CSOUID
C                                       Parallactic angle
         IF (CTIME.GT.LTIMEP) THEN
            CALL PARANG (CTIME, PANGLE)
            LTIMEP = CTIME
            END IF
         TIME(INDEX) = TIME(INDEX) + BUFF2(IPTRI+ILOCT)
C                                       Flag data if IFR is bad
         IF ((IONBUF(IONBND) .EQ. FBLANK)
     *       .OR. (IONBUF(IONBND+1) .EQ. FBLANK)) THEN
            IF (DPARM(1).LE.0.0) THEN
               BUFF2(IOFF+3) = 0.0
               BUFF2(IOFF+6) = 0.0
               BUFF2(IOFF+9) = 0.0
               BUFF2(IOFF+12) = 0.0
               END IF
            PARAN(1,INDEX) = PARAN(1,INDEX) + PANGLE(IA1)
            PARAN(2,INDEX) = PARAN(2,INDEX) + PANGLE(IA2)
         ELSE
            PARAN(1,INDEX) = PARAN(1,INDEX) + PANGLE(IA1)
     *                       - IONBUF(IONBND)
            PARAN(2,INDEX) = PARAN(2,INDEX) + PANGLE(IA2)
     *                       - IONBUF(IONBND+1)
            END IF
C                                       Gather visibilities
         DO 170 LOOP2 = 1,4
            GNDX = GATNDX(LOOP2) + IOFF
            WT = BUFF2(GNDX+2)
            IF (WT.GT.0.0) THEN
               VOBS(1,LOOP2,INDEX) = VOBS(1,LOOP2,INDEX) +
     *            BUFF2(GNDX)*WT
               VOBS(2,LOOP2,INDEX) = VOBS(2,LOOP2,INDEX) +
     *            BUFF2(GNDX+1)*WT
               VWT(LOOP2,INDEX) = VWT(LOOP2,INDEX) + WT
C                                       and model
               IF (VSOU(INDEX).LE.0) THEN
                  GNDX = GATNDX(LOOP2+4) + IOFF
                  VMOD(1,LOOP2,INDEX) = VMOD(1,LOOP2,INDEX) +
     *               BUFF2(GNDX) * WT
                  VMOD(2,LOOP2,INDEX) = VMOD(2,LOOP2,INDEX) +
     *               BUFF2(GNDX+1) * WT
                  END IF
               END IF
 170        CONTINUE
 200     CONTINUE
C                                       Finish last accumulation
 210  INDEX = FIRST
      DO 250 IVIS = 1,CNTBL
         XNORM = 1.0
         IF (SUMCNT(IVIS).GT.0) XNORM = 1.0 / SUMCNT(IVIS)
         TIME(INDEX) = TIME(INDEX) * XNORM
         PARAN(1,INDEX) = PARAN(1,INDEX) * XNORM
         PARAN(2,INDEX) = PARAN(2,INDEX) * XNORM
         DO 240 LOOP2 = 1,4
C                                       valid data
            IF (VWT(LOOP2,INDEX).GT.0.0) THEN
               XNORM = 1.0 / VWT(LOOP2,INDEX)
               VOBS(1,LOOP2,INDEX) = VOBS(1,LOOP2,INDEX) * XNORM
               VOBS(2,LOOP2,INDEX) = VOBS(2,LOOP2,INDEX) * XNORM
C                                       model
               IF (VSOU(INDEX).LE.0) THEN
                  VMOD(1,LOOP2,INDEX) = VMOD(1,LOOP2,INDEX) * XNORM
                  VMOD(2,LOOP2,INDEX) = VMOD(2,LOOP2,INDEX) * XNORM
                  END IF
            ELSE
               VWT(LOOP2,INDEX) = 0.0
               END IF
 240        CONTINUE
         INDEX = INDEX + 1
 250     CONTINUE
C                                       Number of observations.
      NUMDAT = NEXT - 1
C                                       Couldn't fit some data:
      IF (LOPCNT.LT.NVIS) THEN
         MSGTXT = 'PCLSEL: WARNING: INTERNAL ARRAYS TOO SMALL,' //
     *      ' USING ONLY'
         CALL MSGWRT (6)
         WRITE (MSGTXT,1251) LOPCNT, NVIS
         CALL MSGWRT (6)
         MSGTXT = 'PCLSEL: USE LONGER SOLUTION INTERVAL (SOLINT)'
         CALL MSGWRT (6)
         END IF
C                                       Force new scratch files next
C                                       pass.
      VISDSK = 0
      VISCNO = 0
      VMCNO = 0
C                                       Close scratch files
      CALL ZCLOSE (LUN2, IONIND, IRET)
      IF (IRET.EQ.0) CALL ZCLOSE (LUN1, IND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1300) IRET
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Averaging IFs',I3, ' to',I3)
 1001 FORMAT ('Processing IF number',I3,' channel',I6)
 1002 FORMAT ('Processing IF number',I3)
 1100 FORMAT ('PCLSEL: NO DATA SELECTED FOR IF',I3)
 1101 FORMAT ('PCLSEL: NO DATA SELECTED FOR IF',I3,' CHAN',I6)
 1102 FORMAT ('PCLSEL: ERROR ',I5,' UPDATING SCRATCH FILE CATBLK')
 1120 FORMAT ('PCLSEL: ZOPEN ERROR ',I3,' OPENING SCRATCH FILE')
 1121 FORMAT ('PCLSEL: UVINIT ERROR ',I3,' INITIALIZING SCRATCH FILE')
 1122 FORMAT ('PCLSEL: ZOPEN ERROR ',I3,' OPENING IONOSPHERE FILE')
 1123 FORMAT ('PCLSEL: MINIT ERROR ',I3,' INITIALIZING IONOSPHERE FILE')
 1124 FORMAT ('PCLSEL: UVDISK ERROR ',I3,' READING SCRATCH FILE')
 1125 FORMAT ('PCLSEL: MDISK ERROR ',I3,' READ IONOSPHERE FILE')
 1251 FORMAT ('PCLSEL: ',I9,' OF ',I9,' VISIBILITIES')
 1300 FORMAT ('PCLSEL: ZCLOSE ERROR ',I3,' CLOSING SCRATCH FILE')
      END
      SUBROUTINE PCLFLX (IFNO, IRET)
C-----------------------------------------------------------------------
C   PCLFLX includes the CLEAN model visibilities into the data.
C   The model visibilities computed are in the form of I,Q,U and V,
C   one IF at a time in and uv data file created by PCLSEL with space
C   for both observed and model visibilities.
C   If no model is found or a point model is specified then the flux
C   densities are obtained from the Source (SU) table.
C   Computes all four Stokes parameters if available.
C   Inputs: from commons
C     XNIT      R    Number of components to be computed.
C     VMDSK     I    Input file disk number (0=scratch)
C     VMCNO     I    Input file catalog number.
C     NAME2     C*12 CLEAN model name.
C     CLASS2    C*6  Clean model class (I,Q,U,V class)
C     DISK2     I    CLEAN file disk number.
C     XNMAP     R    Number of model files.
C     CCTVER    I    CC table version number.
C     XSMOD     R    If (1) .lt. 0 use no model, if .gt. 0 use model
C     NVCAL     I    Number of calibrator sources
C     VCALID    I(*) Calibrator ID numbers
C     CVFLUX    R(4,*) Polarized flux densities (I,Q,U,V) (Jy)
C     VDOFIT    L(*) If true fit for this calibrator polarization.
C   Output:
C     CNOIN2    I    CLEAN file catalog number.
C     IRET      I    Return code, 0 => ok, otherwise not.
C-----------------------------------------------------------------------
      INTEGER   IFNO, IRET
C
      CHARACTER  CLASS*6, STAT*4, BLANK*12, CHSTOK(4)*1, UTYPE*2
      INTEGER   MODEL, METHOD, ISTOKE, DISKO, ISCR, CHAN, IERR, NCHAN,
     *   I, IROUND, SAVNCL, SAVCID, NPGOOD
      LOGICAL   DOMSG, DOSUM, F, NONAM, NOCLAS, SAVVDO, WASOME
      INCLUDE 'PCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INTEGER   BITER(MAXFLD)
      INCLUDE 'INCS:DSEL.INC'
      DATA BLANK /'            '/
      DATA DOMSG, DOSUM, F /.FALSE.,.FALSE.,.FALSE./
      DATA CHSTOK /'I','Q','U','V'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Save some values
      SAVNCL = NVCAL
      SAVVDO = VDOFIT(1)
      SAVCID = VCALID(1)
C                                       Check if multiple sources
      IF (SINGLE) NSOUWD = 1
      IF (NSOUWD.NE.1) GO TO 300
C                                       If neither a point (XSMOD) nor
C                                       clean model use source table.
      NONAM = NAME2 .EQ. BLANK(1:12)
      NOCLAS = CLAS2 .EQ. BLANK(1:6)
      IF (NONAM .AND. NOCLAS .AND. (.NOT.IMODEL)) GO TO 300
C                                       Set model and method
C                                       (Options limited by sort order.)
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
      IF (CMOD.EQ.'SUBI') MODEL = 3
      METHOD = 0
      IF (CMETH.EQ.'DFT ') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
      DOPTMD = IMODEL
      PTRAOF = XSMOD(5)
      PTDCOF = XSMOD(6)
C                                       ADD the models.
      FACGRD(1) = -1.0
      FACGRD(2) = 1.0
C                                       Can have only 1 source here with
C                                       known polarization.
      NVCAL = 1
      VDOFIT(1) = F
      VCALID(1) = SOUWAN(1)
      IF (VCALID(1).LE.0) VCALID(1) = 1
C                                        No model subtraction for 'ORI-'
C                                        and given PMODEL
      IF ((XSOLTY.EQ.'ORI-') .AND. DOPTMD) GO TO 999
C                                       Loop over four Stokes.
      NPGOOD = 0
      DO 200 ISTOKE = 1,4
C                                       pointers for model routines
         NSTOK = 1
         KSTOK = ISTOKE
         VOFF = (ISTOKE-1) * 3
C                                       Point source parameters
         IF (DOPTMD) THEN
C                                       model all adjusted for IF/chan
            PTFLX = CVFLUX(ISTOKE,1)
            IF (ISTOKE.LE.3) NPGOOD = NPGOOD + 1
            DO3DIM = .FALSE.
         ELSE
C                                       Point model only
            PARMOD(1) = 0.0
C                                       Get info on model file(s)
            LIMFLX = XFLUX
            MFIELD = IROUND (XNMAP)
            IF (MFIELD.LE.0) MFIELD = 1
            NONEG = F
            WASOME = F
            DO 30 I = 1,MFIELD
               BITER(I) = 1
               IF (I.LE.MAXAFL) THEN
                  NCOMP(I) = ABS (XNCOMP(I)) + 0.1
                  IF (XNCOMP(I).LE.-1.0) NONEG = .TRUE.
                  IF (NCOMP(I).GT.0) WASOME = .TRUE.
               ELSE
                  NCOMP(I) = 0
                  IF (WASOME) NCOMP(I) = 1000000000
                  END IF
 30            CONTINUE
            CLNMOD = .NOT.DOPTMD
C                                       Get correct class
            CLASS = CLAS2
            CLASS(1:1) = CHSTOK(ISTOKE)(1:1)
            CNOIN2 = 1
            UTYPE = 'MA'
            CALL CATDIR ('SRCH', DISK2, CNOIN2, NAME2, CLASS, SEQ2,
     *         UTYPE, NLUSER, STAT, BUFF1, IERR)
C                                       If can't find - Skip this
C                                       polarization.
            IF (IERR.NE.0) GO TO 200
            IF (ISTOKE.LE.3) NPGOOD = NPGOOD + 1
            CALL SETGDS (DISKIN, CNOIN, NAME2, CLASS, SEQ2, DISK2,
     *         MFIELD, CCTVER, NCOMP, BITER, MODEL, METHOD, BUFF1,
     *         BUFF2, ISTOKE, IRET)
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
               CALL FACSET (DISKIN, CNOIN, IFNO, SOUWAN(1), MODEL,
     *            FACGRD(1), IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            CNOIN2 = CCCNO(1)
            END IF
C                                       Compute model.
         DISKO = VMDSK
         ISCR = VMCNO
         CHAN = 1
         NUMIF = 1
         IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
         NCHAN = CATBLK(KINAX+JLOCF) * NUMIF
         CALL UVMSUB (DISKO, ISCR, DISKO, ISCR, 0, MODEL, METHOD,
     *      CHAN, NCHAN, DOSUM, DOMSG, CATBLK, JBUFSZ, FRQSEL, BUFF1,
     *      BUFF2, UBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (.NOT.DOPTMD) CALL UNSETG (BUFF2)
 200     CONTINUE
      DOMODL = NPGOOD.GE.3
      VDOFIT(1) = .NOT.DOMODL
      GO TO 999
C                                       Multiple sources, use point
C                                       source at phase center only.
C                                       May have to restore calibrator
C                                       values.
 300  NVCAL = SAVNCL
      XSMOD(1) = 0.0
      VDOFIT(1) = SAVVDO
      VCALID(1) = SAVCID
      CALL PCLPFD (IRET)
      CLNMOD = F
      DOMODL = .TRUE.
C
 999  RETURN
      END
      SUBROUTINE PCLPFD (IRET)
C-----------------------------------------------------------------------
C   PCLPFD adds source flux densities from the source table to the
C   temporary file. Only 1 IF is processed.
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
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C   Note: also uses buffers, BUFF1, BUFF2, UBUFF, NXBUFF
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PCAL.INC'
      CHARACTER IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LENVIS,
     *   ILENBU, KBIND, IBIND, I, J, IVIS, IOFF, NOVIS, CDSOU, IDSOU,
     *   BO, VO
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUNI, LUNO /16,17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Message
      MSGTXT = 'Including source flux densities to model'
      CALL MSGWRT (3)
      LENVIS = CATBLK(KINAX)
C                                       Add model
      NOVIS = (LREC - NRPARM) / LENVIS
      IDSOU = SOUWAN(1)
      IF (IDSOU.LE.0) IDSOU = 1
C                                       Open and init for write
C                                       visibility file
      CALL ZPHFIL ('SC', SCRVOL(VMCNO), SCRCNO(VMCNO), 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, SCRVOL(VMCNO), IFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 70
         WRITE (MSGTXT,1060) IRET, 'WRIT'
         GO TO 990
C                                       Init vis file for write
 70   ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IPTRO = KBIND
      IF (IRET.EQ.0) GO TO 80
         WRITE (MSGTXT,1070) IRET, 'WRIT'
         GO TO 990
C                                       Open and init for read
C                                       visibility file
 80   CALL ZOPEN (LUNI, INDI, SCRVOL(VMCNO), IFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 90
         WRITE (MSGTXT,1060) IRET, 'READ'
         GO TO 990
C                                       Init vis file for read
 90   CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.EQ.0) GO TO 100
         WRITE (MSGTXT,1070) IRET, 'READ'
         GO TO 990
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) IRET, 'READ'
            GO TO 990
 110     IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
C                                       loop thru buffer full
         DO 180 I = 1,INIO
            DO 120 J = 1,LREC
               BUFF2(IPTRO+J-1) = BUFF1(IPTRI+J-1)
 120           CONTINUE
            IF (ILOCSU.GE.0) IDSOU = BUFF2(IPTRO+ILOCSU) + 0.5
            CDSOU = VSOUID(IDSOU)
            IOFF = NRPARM
C                                       Add point flux densities
            DO 140 IVIS = 1,NOVIS
               BUFF2(IPTRO+IOFF) = CVFLUX(IVIS,CDSOU)
               BUFF2(IPTRO+IOFF+1) = 0.0
               IOFF = IOFF + LENVIS
 140           CONTINUE
            IPTRI = IPTRI + LREC
            IPTRO = IPTRO + LREC
 180        CONTINUE
C                                       Write vis. record.
         CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, INIO, KBIND, IRET)
         IF (IRET.EQ.0) GO TO 190
            WRITE (MSGTXT,1100) IRET, 'WRIT'
            GO TO 990
 190     IPTRO = KBIND
         GO TO 100
C                                       Done
C                                       Flush buffer
 200  INIO = 0
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, INIO, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 210
         WRITE (MSGTXT,1100) IRET, 'FLSH'
         GO TO 990
C                                       Close files
 210  CALL ZCLOSE (LUNI, INDI, IRET)
      IF (IRET.EQ.0) GO TO 220
         WRITE (MSGTXT,1210) IRET, 'READ'
         GO TO 990
 220  CALL ZCLOSE (LUNO, INDO, IRET)
      IF (IRET.EQ.0) GO TO 999
         WRITE (MSGTXT,1210) IRET, 'WRIT'
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('PCLPFD: ERROR',I3,' OPEN-FOR-',A4,' VIS FILE')
 1070 FORMAT ('PCLPFD: ERROR',I3,' INIT-FOR-',A4,' VIS FILE')
 1100 FORMAT ('PCLPFD: ERROR',I3,1X,A4,'ING VIS FILE')
 1210 FORMAT ('PCLPFD: ERROR',I3,'CLOSING ',A4,' VIS FILE')
      END
      SUBROUTINE PCLHIS
C-----------------------------------------------------------------------
C   PCLHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12, HILINE*72
      INTEGER   LUN, IERR, I, ITIME(3), DATE(3), K, J, IROUND
      LOGICAL   T
      INCLUDE 'PCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /88/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open old history
      CALL HIOPEN (LUN, DISKIN, CNOIN, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       file name
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Add selection/calibration
C                                       criteria:
C                                       Sources
      DO 20 I = 1,NVCAL
         WRITE (HILINE,3000) TSKNAM, I, CSNAME(I), VCALID(I)
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
 20      CONTINUE
C                                       general calib history
      BIF = XBIF + 0.1
      EIF = XEIF + 0.1
      CALL CALHIS (LUN, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Write control info.
C                                       CC tables
      IF (CLNMOD) THEN
C                                       CC File Name etc.
         CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 190
C                                        CCfile version no.
         WRITE (HILINE,2001) TSKNAM, CCTVER
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                        Number of images
         WRITE (HILINE,2002) TSKNAM, MFIELD
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Number of CLEAN components
C                                       actually used (incl FLUX)
         DO 30 I = 1,MFIELD
            NCOMP(I) = MAX (1, NSUBG(I)) - 1
            WRITE (HILINE,2003) TSKNAM, I, NCOMP(I)
            CALL HIADD (LUN, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
 30         CONTINUE
         END IF
C                                       spectral mode
      IF (DOLINE) THEN
         HILINE = TSKNAM // '/ polarization solved as spectral function'
      ELSE
         HILINE = TSKNAM // '/ polarization solved as continuum' //
     *      ' function'
         END IF
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       solve for source Q, U
      IF (DOMODL) THEN
         HILINE = TSKNAM // '/ source Q and U not fit, used model'
      ELSE
         HILINE = TSKNAM // '/ source Q and U fit, no model used'
         END IF
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Point source model
      IF ((XSMOD(1).GE.1.0E-5) .AND. (DOMODL)) THEN
         WRITE (HILINE,2020) TSKNAM, XSMOD(1), XSMOD(2), XSMOD(3),
     *      XSMOD(4)
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2021) TSKNAM, XSMOD(5), XSMOD(6)
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       spectral index
      IF (DOSCAL.GE.0.0) THEN
         DO 40 I = 1,NVCAL
            WRITE (HILINE,2030) TSKNAM, (SSPECT(J,1,I), J = 1,2),
     *         VCALID(I)
            CALL HIADD (LUN, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            WRITE (HILINE,2031) TSKNAM, (SSPECT(J,1,I), J = 3,5),
     *         VCALID(I)
            CALL HIADD (LUN, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
 40         CONTINUE
         END IF
C                                       line smoothing
      IF (DOLINE) THEN
         I = IROUND (XINTP(1))
         WRITE (HILINE,2035) TSKNAM, I, XINTP(2), XINTP(3)
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Solution interval
      WRITE (HILINE,2022) TSKNAM, XSOLIN
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Solution type
      WRITE (HILINE,2023) TSKNAM, XSOLTY
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Reference antenna
      I = XREF + 0.5
      WRITE (HILINE,2024) TSKNAM, I
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Channel selection
      DO 65 K = BIF,EIF
         DO 60 I = 1,20
            IF ((CHNSEL(1,I,K).GT.0) .AND.
     *         (CHNSEL(2,I,K).GE.CHNSEL(1,I,K))) THEN
               WRITE (HILINE,3050) TSKNAM, (CHNSEL(J,I,K), J = 1,3), K
               CALL HIADD (LUN, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
 60         CONTINUE
 65      CONTINUE
C                                       Close HI file
 190   CALL HICLOS (LUN, T, BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2001 FORMAT (A6,'INVER = ',I5,' /CC file version no.')
 2002 FORMAT (A6,'NMAPS =',I4,' /NUMBER OF CLEAN IMAGES USED')
 2003 FORMAT (A6,'NCOMP(',I3,') = ',I8,' /Number of CLEAN comps.')
 2020 FORMAT (A6,'PMODEL =',4(F9.4,','))
 2021 FORMAT (A6,'        ',2(F9.3,','),F9.3,' /Poln. model')
 2022 FORMAT (A6,'SOLINT =',F10.3,' /Solution interval (min)')
 2023 FORMAT (A6,'SOLTYPE =''',A4,''' /Poln. solution type')
 2024 FORMAT (A6,'REFANT =',I4,' / Reference antenna')
 2030 FORMAT (A6,'F1GHz=',F7.3,' Spix=',F6.3,
     *   '  / spectral index source',I3)
 2031 FORMAT (A6,'SPcurve=',3F7.3,
     *   '  / spectral index curvature source',I2)
 2035 FORMAT (A6,'INTPARM=',I2,2F6.2,'  / post-cal spectral smoothing')
 3000 FORMAT (A6,'CALSOUR(',I2,') = ''',A,''' ID=',I4,'  / cal source')
 3050 FORMAT (A6,'/ Chns used: Start, Stop, Inc ',2I5,I4,'  IF=',I3)
      END
      SUBROUTINE GETPDS (DISK, CNO, CATBLK, ISUB, IFREQ, NC, NI, NP, NA,
     *   DTERMS, PHDIFF, PDSOLT, BUFF, IRET)
C-----------------------------------------------------------------------
C   reads existing DTERMS if any from PD table
C   Inputs:
C      DISK     I      disk
C      CNO      I      catalog number
C      CATBLK   I(*)   header
C      ISUB     I      Restrict to subarray
C      IFREQ    I      Restrict to FREQID
C      NC       I      Number spectral channels
C      NI       I      Number IFs
C      NP       I      Number polarizations (2 always I suspect)
C      NA       I      Max antenna number
C   Output:
C      PHDIFF   R(*)   Phase differences (NC,NI)
C      DTERMS   R(*)   big array to hold it all: (2,NC,NI,NP,NA)
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(*), ISUB, IFREQ, NC, NI, NP, NA, IRET
      REAL      DTERMS(2,NC,NI,NP,*), PHDIFF(NC,*), BUFF(*)
      CHARACTER PDSOLT*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VER, BUFFER(512), LUNTMP, LUN, IPDRNO, PDKOLS(9),
     *   PDNUMV(9), NUMANT, NUMPOL, NUMIF, NUMFRQ, I, ANT, SUBA, FREQID,
     *   REFANT, NREC, J
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      CALL FNDEXT ('PD', CATBLK, VER)
      I = 2 * NC * NI * NP * NA
      CALL RFILL (I, 0.0, DTERMS)
      CALL RFILL (MAXCIF, 0.0, PHDIFF)
      IF (VER.GT.0) THEN
         LUN = LUNTMP (1)
         CALL PDINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *      IPDRNO, PDKOLS, PDNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *      PDSOLT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING PD TABLE TO READ'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       sanity check
         IF ((NUMANT.NE.NA) .OR. (NUMPOL.NE.NP) .OR. (NUMIF.NE.NI)
     *      .OR. (NUMFRQ.NE.NC)) THEN
            MSGTXT = 'EXISTING PD FILE DOES NOT MATCH REQUESTED ONE'
            CALL MSGWRT (7)
            WRITE (MSGTXT,1010) 'OLD', NUMANT, NUMPOL, NUMIF, NUMFRQ
            CALL MSGWRT (7)
            WRITE (MSGTXT,1010) 'NOW', NA, NP, NI, NC
            CALL MSGWRT (7)
            IRET = 10
            GO TO 900
            END IF
         NREC = BUFFER(5)
         DO 30 I = 1,NREC
            CALL TABPD ('READ', BUFFER, IPDRNO, PDKOLS, PDNUMV, NUMIF,
     *         NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, BUFF,
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING OLD PD TABLE'
               CALL MSGWRT (8)
               GO TO 900
            ELSE IF (IRET.EQ.0) THEN
               IF (((SUBA.LE.0) .OR. (SUBA.EQ.ISUB)) .AND.
     *            ((FREQID.LE.0) .OR. (IFREQ.LE.0) .OR.
     *            (IFREQ.EQ.FREQID))) THEN
                  J = 2 * NC * NI * NP
                  CALL RCOPY (J, BUFF, DTERMS(1,1,1,1,ANT))
                  END IF
               END IF
 30         CONTINUE
C                                       close it down
 900     CALL TABPD ('CLOS', BUFFER, IPDRNO, PDKOLS, PDNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, BUFF, I)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETPDS: ERROR',I4,' ON ',A)
 1010 FORMAT (A,'NUMBER ANTS',I3,' POLS',I2,' IFS',I3,' CHANS',I6)
      END
      SUBROUTINE GETCPS (DISK, CNO, CATBLK, IVER, IFREQ, NC, NI, NS,
     *   SIDS, SNAMS, STERMS, IRET)
C-----------------------------------------------------------------------
C   reads old source polarizations from a CP table
C   Inputs:
C      DISK     I      disk
C      CNO      I      catalog number
C      CATBLK   I(*)   header
C      IVER     I      CP version number
C      IFREQ    I      Restrict to FREQID
C      NC       I      Number spectral channels
C      NI       I      Number IFs
C      NS       I      Max source number in list
C      SIDS     I(*)   Actual source number to attach to 1 -> NS
C      SNAMS    C(*)*16   Source names to attach to 1 -> NS.
C   Output
C      STERMS   R(*)   big array to hold it all: (4,NC,NI,NS)
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(*), IVER, IFREQ, NC, NI, NS, SIDS(*),
     *   IRET
      CHARACTER SNAMS(*)*16
      REAL      STERMS(4,NC,NI,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VER, BUFFER(512), LUNTMP, LUN, ICPRNO, CPKOLS(6),
     *   CPNUMV(6), NUMIF, NUMFRQ, I, SID, J, K, L
      REAL      STEMP(4,MAXCIF)
      CHARACTER SN*16
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      L = 4 * NC * NI
      CALL RFILL (NS*L, 0.0, STERMS)
      IRET = 0
      IF (VER.LE.0) GO TO 999
C                                       open
      LUN = LUNTMP (1)
      NUMIF = NI
      NUMFRQ = NC
C                                       now write
      VER = IVER
      CALL CPINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, ICPRNO,
     *   CPKOLS, CPNUMV, NUMIF, NUMFRQ, IFREQ, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OLD CP TABLE TO READ'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       read loop
      J = 0
      DO 30 I = 1,NS
         CALL TABCP ('READ', BUFFER, ICPRNO, CPKOLS, CPNUMV, NUMIF,
     *      NUMFRQ, SN, SID, STEMP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING OLD CP TABLE'
            CALL MSGWRT (8)
            GO TO 900
            END IF
         DO 20 K = 1,NS
            IF ((SN.EQ.SNAMS(K)) .AND. (SID.EQ.SIDS(K))) THEN
               J = J + 1
               CALL RCOPY (L, STEMP, STERMS(1,1,1,K))
               GO TO 30
               END IF
 20         CONTINUE
 30      CONTINUE
      IF (J.LT.NS) THEN
         WRITE (MSGTXT,1030) J, NS
         CALL MSGWRT (6)
         END IF
C                                       close it down
 900  CALL TABCP ('CLOS', BUFFER, ICPRNO, CPKOLS, CPNUMV, NUMIF,
     *   NUMFRQ, SNAMS, SIDS, STERMS, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETCPS: ERROR',I4,' ON ',A)
 1030 FORMAT ('GETCPS: FOUND ONLY',I5,' SOURCES, WANTED',I5)
      END
      SUBROUTINE SAVPDS (DISK, CNO, CATBLK, ISUB, IFREQ, IREF, NC, NI,
     *   NP, NA, DTERMS, PHDIFF, SOLTYP, BUFF, IRET)
C-----------------------------------------------------------------------
C   updates existing DTERMS if any from PD table
C   Inputs:
C      DISK     I      disk
C      CNO      I      catalog number
C      CATBLK   I(*)   header
C      ISUB     I      Restrict to subarray
C      IFREQ    I      Restrict to FREQID
C      IREF     I      Reference antenna
C      NC       I      Number spectral channels
C      NI       I      Number IFs
C      NP       I      Number polarizations (2 always I suspect)
C      NA       I      Max antenna number
C      SOLTYP   I      Solution type code
C      DTERMS   R(*)   big array to hold it all: (2,NC,NI,NP,NA)
C      PHDIFF   R(*)   phase differences (NC,NI)
C   Output:
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(*), ISUB, IFREQ, IREF, NC, NI, NP, NA,
     *   SOLTYP, IRET
      REAL      DTERMS(2,NC,NI,NP,*), PHDIFF(NC,*), BUFF(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VER, BUFF1(512), LUNTMP, LUN1, IPDRNO, PDKOLS(9), NREC,
     *   PDNUMV(9), NUMANT, NUMPOL, NUMIF, NUMFRQ, I, ANT, SUBA, FREQID,
     *   REFANT, BUFF2(512), LUN2, J, PDKOLO(9), PDNUMO(9)
      LOGICAL   WROTE(MAXANT)
      CHARACTER PDSOLT*8, CHSOL(4)*8
      INCLUDE 'INCS:DMSG.INC'
      DATA CHSOL /'APPROX  ', 'ORI-ELP ','X-Y LIN ', 'VLBI'/
C-----------------------------------------------------------------------
C                                       fix file status if needed
      CALL CATFIX (DISK, CNO, 'NOTR')
      CALL LFILL (NA, .FALSE., WROTE)
      CALL FNDEXT ('PD', CATBLK, VER)
      LUN1 = LUNTMP (1)
      LUN2 = LUN1 - 1
C                                       open
      NUMANT = NA
      NUMPOL = NP
      NUMIF = NI
      NUMFRQ = NC
      BUFF1(5) = 0
      IF (VER.GT.0) THEN
         CALL PDINI ('READ', BUFF1, DISK, CNO, VER, CATBLK, LUN1,
     *      IPDRNO, PDKOLS, PDNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *      PDSOLT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING PD TABLE TO READ'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         NREC = BUFF1(5)
      ELSE
         NREC = 0
         END IF
C                                       now write
      PDSOLT = CHSOL (MAX(1, MIN(4, SOLTYP)))
      VER = VER + 1
      CALL PDINI ('WRIT', BUFF2, DISK, CNO, VER, CATBLK, LUN2, IPDRNO,
     *   PDKOLO, PDNUMO, NUMANT, NUMPOL, NUMIF, NUMFRQ, PDSOLT, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING PD TABLE TO WRITE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       update
      J = 2 * NC * NI * NP
      IPDRNO = 1
      DO 30 I = 1,NREC
         CALL TABPD ('READ', BUFF1, IPDRNO, PDKOLS, PDNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, BUFF,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING OLD PD TABLE'
            CALL MSGWRT (8)
            GO TO 900
         ELSE IF (IRET.EQ.0) THEN
            IF (((SUBA.LE.0) .OR. (SUBA.EQ.ISUB)) .AND.
     *         ((FREQID.LE.0) .OR. (IFREQ.LE.0) .OR.
     *         (IFREQ.EQ.FREQID))) THEN
               CALL RCOPY (J, DTERMS(1,1,1,1,ANT), BUFF)
               WROTE(ANT) = .TRUE.
               END IF
            IPDRNO = I
            CALL TABPD ('WRIT', BUFF2, IPDRNO, PDKOLO, PDNUMO, NUMIF,
     *         NUMFRQ, NUMPOL, ANT, SUBA, FREQID, IREF, PHDIFF, BUFF,
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING PD TABLE'
               CALL MSGWRT (8)
               GO TO 900
               END IF
            END IF
 30      CONTINUE
C                                       now write the rest
      SUBA = ISUB
      FREQID = IFREQ
      REFANT = IREF
      DO 40 ANT = 1,NA
         IF (.NOT.WROTE(ANT)) THEN
            CALL RCOPY (J, DTERMS(1,1,1,1,ANT), BUFF)
            CALL TABPD ('WRIT', BUFF2, IPDRNO, PDKOLO, PDNUMO, NUMIF,
     *         NUMFRQ, NUMPOL, ANT, SUBA, FREQID, IREF, PHDIFF, BUFF,
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING PD TABLE'
               CALL MSGWRT (8)
               GO TO 900
               END IF
            WROTE(ANT) = .TRUE.
            END IF
 40      CONTINUE
C                                       close it down
 900  CALL TABPD ('CLOS', BUFF2, IPDRNO, PDKOLO, PDNUMO, NUMIF,
     *   NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, BUFF, I)
      IF (NREC.GT.0) CALL TABPD ('CLOS', BUFF1, IPDRNO, PDKOLS, PDNUMV,
     *   NUMIF, NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, BUFF,
     *   I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SAVPDS: ERROR',I4,' ON ',A)
      END
      SUBROUTINE SAVCPS (DISK, CNO, CATBLK, IFREQ, NC, NI, NS, SIDS,
     *   SNAMS, STERMS, IRET)
C-----------------------------------------------------------------------
C   writes new source polarizations to a new CP table
C   Inputs:
C      DISK     I      disk
C      CNO      I      catalog number
C      CATBLK   I(*)   header
C      ISUB     I      Restrict to subarray
C      IFREQ    I      Restrict to FREQID
C      NC       I      Number spectral channels
C      NI       I      Number IFs
C      NS       I      Max source number in list
C      SIDS     I(*)   Actual source number to attach to 1 -> NS
C      SNAMS    C(*)*16   Source names to attach to 1 -> NS.
C      STERMS   R(*)   big array to hold it all: (4,NC,NI,NS)
C   Output
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(*), IFREQ, NC, NI, NS, SIDS(*), IRET
      CHARACTER SNAMS(*)*16
      REAL      STERMS(4,NC,NI,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VER, BUFFER(512), LUNTMP, LUN, ICPRNO, CPKOLS(6),
     *   CPNUMV(6), NUMIF, NUMFRQ, I
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       fix file status if needed
      CALL CATFIX (DISK, CNO, 'NOTR')
      LUN = LUNTMP (1)
C                                       open
      NUMIF = NI
      NUMFRQ = NC
C                                       now write
      VER = 0
      CALL CPINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN, ICPRNO,
     *   CPKOLS, CPNUMV, NUMIF, NUMFRQ, IFREQ, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING CP TABLE TO WRITE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       update
      DO 30 I = 1,NS
         CALL TABCP ('WRIT', BUFFER, ICPRNO, CPKOLS, CPNUMV, NUMIF,
     *      NUMFRQ, SNAMS(I), SIDS(I), STERMS(1,1,1,I), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING CP TABLE'
            CALL MSGWRT (8)
            GO TO 900
            END IF
 30      CONTINUE
C                                       close it down
 900  CALL TABCP ('CLOS', BUFFER, ICPRNO, CPKOLS, CPNUMV, NUMIF,
     *   NUMFRQ, SNAMS, SIDS, STERMS, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SAVCPS: ERROR',I4,' ON ',A)
      END
      SUBROUTINE PCLINT (NC, NI, NP, NA, NS, IREF, DTERMS, STERMS)
C-----------------------------------------------------------------------
C   updates DTERMS and STERMS by interpolating across blanked or 0.0
C   also sets ref ant R pol to 0.0 if it is < 1.e-5
C   Inputs:
C      NC       I      Number spectral channels
C      NI       I      Number IFs
C      NP       I      Number polarizations (2 always I suspect)
C      NA       I      Max antenna number
C      NS       I      Number sources
C      IREF     I      Reference antenna
C   In/Output:
C      STERMS   R(*)   big array to hold it all: (4,NC,NI,NS)
C      DTERMS   R(*)   big array to hold it all: (2,NC,NI,NP,NA)
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   NC, NI, NP, NA, NS, IREF
      REAL      DTERMS(2,NC,NI,NP,*), STERMS(4,NC,NI,*)
C
      INTEGER   LC, LI, LP, LA, LS, LT, II, JJ
      REAL      AMP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       reference antenna
      IF ((IREF.GT.0) .AND. (IREF.LE.NA)) THEN
         II = 0
         DO 30 LI = 1,NI
            DO 10 LC = 1,NC
               IF (DTERMS(1,LC,LI,1,IREF).NE.FBLANK) THEN
                  AMP = DTERMS(1,LC,LI,1,IREF)**2 +
     *               DTERMS(2,LC,LI,1,IREF)**2
                  IF (AMP.GT.1.E-11) GO TO 30
                  END IF
 10            CONTINUE
C                                       fill with 0.0
            II = II + 1
            DO 20 LC = 1,NC
               IF (DTERMS(1,LC,LI,1,IREF).NE.FBLANK) THEN
                  DTERMS(1,LC,LI,1,IREF) = 0.0
                  DTERMS(2,LC,LI,1,IREF) = 0.0
                  END IF
 20            CONTINUE
 30         CONTINUE
         IF (II.GT.0) THEN
            WRITE (MSGTXT,1030) II, NI
            CALL MSGWRT (3)
         ELSE
            WRITE (MSGTXT,1031)
            CALL MSGWRT (3)
            END IF
         END IF
C                                       DTERMS
      JJ = 0
      DO 140 LA = 1,NA
         DO 130 LP = 1,NP
            DO 120 LI = 1,NI
               II = 0
               DO 110 LC = 1,NC
                  IF ((DTERMS(1,LC,LI,LP,LA).EQ.0.0) .AND.
     *               (DTERMS(2,LC,LI,LP,LA).EQ.0.0)) II = II + 1
                  IF (DTERMS(1,LC,LI,LP,LA).EQ.FBLANK) II = II + 1
 110              CONTINUE
               IF ((II.GT.0) .AND. (II.LT.NC)) THEN
                  CALL PCLDIN (NC, DTERMS(1,1,LI,LP,LA))
                  JJ = JJ + 1
                  END IF
 120           CONTINUE
 130        CONTINUE
 140     CONTINUE
      IF (JJ.GT.0) THEN
         II = NA * NP * NI
         WRITE (MSGTXT,1140) JJ, II
         CALL MSGWRT (3)
         END IF
C                                       STERMS
      JJ = 0
      DO 240 LS = 1,NS
         DO 230 LI = 1,NI
            DO 220 LT = 1,4
               II = 0
               DO 210 LC = 1,NC
                  IF ((STERMS(LT,LC,LI,LS).EQ.0.0) .OR.
     *               (STERMS(LT,LC,LI,LS).EQ.FBLANK)) II = II + 1
 210              CONTINUE
               IF ((II.GT.0) .AND. (II.LT.NC)) THEN
                  CALL PCLSIN (LT, NC, STERMS(1,1,LI,LS))
                  JJ = JJ + 1
                  END IF
 220           CONTINUE
 230        CONTINUE
 240     CONTINUE
      IF (JJ.GT.0) THEN
         II = NS * 4 * NI
         WRITE (MSGTXT,1240) JJ, II
         CALL MSGWRT (3)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('PCLINT zeroed',I3,' of',I3,
     *   ' IF''s reference antenna R solutions')
 1031 FORMAT ('PCLINT did not zero any reference antenna solutions',
     *   ' all were too large')
 1140 FORMAT ('PCLINT interpolated',I5,' of',I5,' Dterm spectra for',
     *   ' flagged data')
 1240 FORMAT ('PCLINT interpolated',I5,' of',I5,' source spectra for',
     *   ' flagged data')
      END
      SUBROUTINE PCLDIN (NC, DT)
C-----------------------------------------------------------------------
C   PCLDIN interpolates 1 complex spectrun from DTERMs
C   The need for interpolation is assumed here (checked above)
C   Inputs:
C      NC   I
C   In/Out:
C      DT   R(2,NC)   DTERM spectrum
C-----------------------------------------------------------------------
      INTEGER   NC
      REAL      DT(2,NC)
C
      INTEGER   LC, SIC, LIC, NINTP, I
      LOGICAL   FOUBLK
      REAL      WT1, WT2, V1R, V2R, V1I, V2I
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SIC = 0
      LIC = 0
 10   FOUBLK = .FALSE.
      DO 20 LC = LIC+1,NC
         IF ((DT(1,LC).EQ.FBLANK) .OR. (DT(2,LC).EQ.FBLANK) .OR.
     *      ((DT(1,LC).EQ.0.0) .AND. (DT(2,LC).EQ.0.0))) THEN
            IF (.NOT.FOUBLK) THEN
               SIC = LC - 1
               FOUBLK = .TRUE.
               END IF
         ELSE
            IF ((LC.GT.SIC) .AND. (FOUBLK)) THEN
               LIC = LC
               GO TO 30
               END IF
            END IF
 20      CONTINUE
      LIC = NC + 1
C                                       a block to do
 30   NINTP = LIC - SIC - 1
      IF ((FOUBLK) .AND. (NINTP.GT.0)) THEN
         IF (SIC.GT.0) THEN
            V1R = DT(1,SIC)
            V1I = DT(2,SIC)
         ELSE
            V1R = 0.0
            V1I = 0.0
            END IF
         IF (LIC.LE.NC) THEN
            V2R = DT(1,LIC)
            V2I = DT(2,LC)
         ELSE
            V2R = 0.0
            V2I = 0.0
            END IF
         DO 40 I = 1,NINTP
            IF (SIC.LE.0) THEN
               WT2 = 1.0
            ELSE IF (LIC.GT.NC) THEN
               WT2 = 0.0
            ELSE
               WT2 = I / (NINTP + 1.0)
               END IF
            WT1 = 1.0 - WT2
            DT(1,I+SIC) = WT1 * V1R + WT2 * V2R
            DT(2,I+SIC) = WT1 * V1I + WT2 * V2I
 40         CONTINUE
         END IF
C                                       loop back for more
      IF ((FOUBLK) .AND. (LIC.LT.NC)) GO TO 10
C
 999  RETURN
      END
      SUBROUTINE PCLSIN (LT, NC, ST)
C-----------------------------------------------------------------------
C   PCLDIN interpolates I/Q/U/V spectruM from STERMs
C   The need for interpolation is assumed here (checked above)
C   Inputs:
C      LT   I         The particular polarization
C      NC   I         The number of channels
C   In/Out:
C      ST   R(4,NC)   STERM spectrum
C-----------------------------------------------------------------------
      INTEGER   LT, NC
      REAL      ST(4,NC)
C
      INTEGER   LC, SIC, LIC, NINTP, I
      LOGICAL   FOUBLK
      REAL      WT1, WT2, V1, V2
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SIC = 0
      LIC = 0
 10   FOUBLK = .FALSE.
      DO 20 LC = LIC+1,NC
         IF ((ST(LT,LC).EQ.FBLANK) .OR. (ST(LT,LC).EQ.0.0)) THEN
            IF (.NOT.FOUBLK) THEN
               SIC = LC - 1
               FOUBLK = .TRUE.
               END IF
         ELSE
            IF ((LC.GT.SIC) .AND. (FOUBLK)) THEN
               LIC = LC
               GO TO 30
               END IF
            END IF
 20      CONTINUE
      LIC = NC + 1
C                                       a block to do
 30   NINTP = LIC - SIC - 1
      IF ((FOUBLK) .AND. (NINTP.GT.0)) THEN
         IF (SIC.GT.0) THEN
            V1 = ST(LT,SIC)
         ELSE
            V1 = 0.0
            END IF
         IF (LIC.LE.NC) THEN
            V2 = ST(LT,LIC)
         ELSE
            V2 = 0.0
            END IF
         DO 40 I = 1,NINTP
            IF (SIC.LE.0) THEN
               WT2 = 1.0
            ELSE IF (LIC.GT.NC) THEN
               WT2 = 0.0
            ELSE
               WT2 = I / (NINTP + 1.0)
               END IF
            WT1 = 1.0 - WT2
            ST(LT,I+SIC) = WT1 * V1 + WT2 * V2
 40         CONTINUE
         END IF
C                                       loop back for more
      IF ((FOUBLK) .AND. (LIC.LT.NC)) GO TO 10
C
 999  RETURN
      END
      SUBROUTINE PUTSOU (NVCAL, VFLUX, VCALID, MXNCAL, BIF, EIF, DISK,
     *   CNO, CATBLK, LUN, FREQID, IERR)
C-----------------------------------------------------------------------
C   Routine to enter calibrator flux polarization flux densities into
C   the SU table.
C   Inputs:
C    NVCAL         I    Number of calibrator sources.
C    VFLUX(4,*,if) R    Flux densities I, Q, U, V (Jy)
C    VCALID(*)     I    Source ID numbers
C    MXNCAL        I    Dimension of VFLUX and VCALID
C    BIF           I    First IF number
C    EIF           I    Highest IF number
C    DISK          I    Disk number for NX and SN tables.
C    CNO           I    Catalog slot number
C    CATBLK(256)   I    Catalog header
C    LUN           I    LUN to use. (e.g. 25)
C    FREQID        I    FREQID info being written for
C   Output:
C    IERR          I    Return code. 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   MXNCAL
      INTEGER   NVCAL, VCALID(MXNCAL), BIF, EIF, DISK, CNO, CATBLK(*),
     *    LUN, FREQID, IERR
      INCLUDE 'INCS:PUVD.INC'
      REAL      VFLUX (4,MXNCAL,*)
      INTEGER   BUFFER(512), VER, KOLS(MAXSUC), NUMV(MAXSUC),
     *   NUMIF, JERR, I, ICALID, IBIF, IEIF, MSGSAV
      INTEGER   IRNO, NUMREC, LOOP
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      JERR = 0
      IERR = 0
C                                       See if any sources
      IF (NVCAL.LE.0) GO TO 999
C                                       Find Source Info
C                                       Open SU table
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   NUMIF, VELTYP, VELDEF, SUFQID, IRNO, KOLS, NUMV, JERR)
      MSGSUP = MSGSAV
      IF (JERR.NE.0) GO TO 999
C                                       Check IF limits
      IBIF = BIF
      IEIF = EIF
      IF (IBIF.LE.0) IBIF = 1
      IF (IBIF.GT.NUMIF) IBIF = NUMIF
      IF (IEIF.LE.IBIF) IEIF = IBIF
      IF (IEIF.GT.NUMIF) IEIF = NUMIF
C                                       Get number of records
      NUMREC = BUFFER(5)
C                                       Close and reopen write.
      CALL TABIO ('CLOS', 0, IRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1090) IERR, 'SU'
         GO TO 990
 10   IF (NUMREC.LE.0) GO TO 999
      SUFQID = FREQID
      CALL SOUINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   NUMIF, VELTYP, VELDEF, SUFQID, IRNO, KOLS, NUMV, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find source
      DO 80 LOOP = 1,NUMREC
         IRNO = LOOP
         CALL TABSOU ('READ', BUFFER, IRNO, KOLS, NUMV, IDSOUR, SNAME,
     *      QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH,
     *      RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA, PMDEC,
     *      IERR)
         IF (IERR.EQ.0) GO TO 30
            WRITE (MSGTXT,1000) IERR, 'READ'
            GO TO 990
C                                       See if wanted
 30      DO 40 I = 1,NVCAL
            ICALID = I
            IF (IDSOUR .EQ. VCALID(I)) GO TO 50
 40         CONTINUE
C                                       Not wanted
         GO TO 80
C                                       Save flux densities
 50      DO 60 I = IBIF,IEIF
            FLUX(1,I) = VFLUX(1,ICALID,I)
            FLUX(2,I) = VFLUX(2,ICALID,I)
            FLUX(3,I) = VFLUX(3,ICALID,I)
            FLUX(4,I) = VFLUX(4,ICALID,I)
 60         CONTINUE
C                                       Rewrite record.
         IRNO = LOOP
         CALL TABSOU ('WRIT', BUFFER, IRNO, KOLS, NUMV, IDSOUR, SNAME,
     *      QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH,
     *      RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA, PMDEC,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRIT'
            GO TO 990
            END IF
 80      CONTINUE
      CALL TABIO ('CLOS', 0, IRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1090) IERR, 'SU'
         GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PUTSOU: TABSOU ERROR ',I3,1X,A4,'ING SU TABLE')
 1090 FORMAT ('PUTSOU: TABIO ERROR ',I3,' CLOSING ',A2,' TABLE')
      END
      SUBROUTINE PUTANT (DISK, CNO, INVER, OUTVER, BIF, EIF, NUMIF,
     *   SOLTYP, CATBLK, BUFF1, BUFF2, FREQID, IERR)
C-----------------------------------------------------------------------
C   PUTANT reads an antennas (AN) extension file and copies to an
C   output file adding antenna feed parameters.
C   Inputs:
C      DISK      I      Volume number
C      CNO       I      Catalog slot number
C      INVER     I      Input version number
C      OUTVER    I      Output version number
C      BIF       I      First IF number
C      EIF       I      Highest IF number
C      NUMIF     I      Number of IFs
C      SOLTYP    I      Feed solution type.
C                       1 = linear approximation
C                       2 = orientation, ellipticity
C                       3 = Lin. approx for X-Y feeds
C                       4 = VLBI linear approx.
C      CATBLK(*) I      Catalog header block
C      FREQID    I      FQ ID for which polzn parms being
C                       calculated
C   Input from COMMON (DANS.INC):
C      STNEPL(2,*)R    Feed real/elipticity (poln, IF)
C      STNORI(2,*)R    Feed imag/orientation (poln, IF)
C   Output:
C      BUFF1(*)  I      I/O Buffer
C      BUFF2(*)  I      I/O Buffer
C      IERR      I      Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CHPOLT*8, CHSOL(4)*8
      INTEGER   DISK, CNO, CATBLK(256), INVER, OUTVER, BUFF1(*),
     *   BUFF2(*), IERR,  BIF, EIF, NUMIF, SOLTYP, FREQID,
     *   A2NUMV(MAXANC), A2KOLS(MAXANC), IIF, LUN1, LUN2, IANT, INDEX,
     *   KEYTYP, LOCS, IBIF, IEIF, NUMREC
      HOLLERITH HSOLTY(2)
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHSOL /'APPROX  ', 'ORI-ELP ','X-Y LIN ', 'VLBI'/
      DATA CHPOLT /'POLTYPE '/
      DATA LUN1, LUN2 /77,78/
C-----------------------------------------------------------------------
C                                      Open AN extension file.
      CALL ANTINI ('READ', BUFF1, DISK, CNO, INVER, CATBLK, LUN1,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ'
         GO TO 990
         END IF
C                                       Check FREQID compatibility.
      IF ((ANFQID.GT.0) .AND. (FREQID.GT.0) .AND. (ANFQID.NE.FREQID))
     *   THEN
         MSGTXT = 'WARNING:'
         CALL MSGWRT (6)
         MSGTXT = '   The polarization information in your AN table'
         CALL MSGWRT (6)
         WRITE (MSGTXT,1050) ANFQID
         CALL MSGWRT (6)
         WRITE (MSGTXT,1060) FREQID
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C
      NUMREC = BUFF1(5)
C                                       Check IF limits
      IBIF = BIF
      IEIF = EIF
      IF (IBIF.LE.0) IBIF = 1
      IF (IBIF.GT.NUMIF) IBIF = NUMIF
      IF (IEIF.LE.IBIF) IEIF = IBIF
      IF (IEIF.GT.NUMIF) IEIF = NUMIF
C                                       Open output table
      ANTNIF = NUMIF
      NOPCAL = 2
      ANFQID = FREQID
      CALL ANTINI ('WRIT', BUFF2, DISK, CNO, OUTVER, CATBLK, LUN2,
     *   IANRNO, A2KOLS, A2NUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRIT'
         GO TO 990
         END IF
C                                       Read AN records
      DO 200 IANT = 1,NUMREC
         IANRNO = IANT
         CALL TABAN ('READ', BUFF1, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1100) IERR, 'READ'
            GO TO 990
            END IF
C                                       Feed polarizations
         INDEX = 2 * (IBIF-1) + 1
         DO 150 IIF = IBIF,IEIF
            POLCA(INDEX) = STNELP(1,IIF,NOSTA)
            POLCA(INDEX+1) = STNORI(1,IIF,NOSTA)
            POLCB(INDEX) = STNELP(2,IIF,NOSTA)
            POLCB(INDEX+1) = STNORI(2,IIF,NOSTA)
            INDEX = INDEX + 2
 150        CONTINUE
C                                       Write record
         IANRNO = IANT
         CALL TABAN ('WRIT', BUFF2, IANRNO, A2KOLS, A2NUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'WRIT'
            GO TO 990
            END IF
 200     CONTINUE
C                                       Add solution type keyword.
      LOCS = 1
      KEYTYP = 3
      CALL CHR2H (8, CHSOL(SOLTYP), 1, HSOLTY)
      CALL TABKEY ('WRIT', CHPOLT, 1, BUFF2, LOCS, HSOLTY,
     *   KEYTYP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         GO TO 990
         END IF
C                                       Close AN extension files
      CALL TABIO ('CLOS', 1, IANRNO, BUFF1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1210) IERR
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 1, IANRNO, BUFF2, BUFF2, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1210) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PUTANT: ERROR',I3,' OPEN-FOR-',A4,'ING AN FILE')
 1050 FORMAT ('   was set with FREQID ',I3,' this is being')
 1060 FORMAT ('   overwritten with parameters derived from FREQID ',I3)
 1100 FORMAT ('PUTANT: ERROR',I3,1X,A4,'ING AN FILE')
 1200 FORMAT ('PUTANT: ERROR',I3,' ADDING SOLUTION TYPE KEYWORD')
 1210 FORMAT ('PUTANT: ERROR',I3,' CLOSING AN FILE')
      END
      SUBROUTINE LPCALC (NOBS, VOBS, WT, CHI, IJS, N, D, VSTMOD,
     *   NUCAL, VST, ICAL, IREF, CSNAME, IER)
C-----------------------------------------------------------------------
C   Subroutine to determine instrumental and source polarizations.
C   Will currently handle MAXCAL (=50) calibrators.
C   Inputs:
C     NOBS       I    The number of observations, an observation
C                     consists of four visibility measuments on one
C                     baseline (RR, RL, LR, LL if using circular feeds).
C     VOBS(4,*)  CPLX Observed visibilities (RR, RL, LR, LL)
C     WT(4,*)    R    Weights of the visibilities
C     CHI(2,*)   R    Parallactic angles of the feeds for each antenna
C                     used for the observed visibilities.  For
C                     equatorial mounts use 0.
C     IJS(2,*)   I    Antenna numbers of the observations.  IJS(1,n)
C                     should be the lower number, IJS(2,n) the upper.
C     N          I    Number of antennas, actually the antenna number
C                     of the highest numbered antenna.
C     VSTMOD(4,*)CPLX Model polarizations per source (I,Q,U,V).
C                     Values only need be supplied to observations
C                     corresponding to sources which are NOT having
C                     point source polarization parameters fitted.
C     NUCAL      I    Number of unknown calibrator sources, i.e. the
C                     number of sources whose polarizations are to be
C                     determined.
C     ICAL(*)    I    Source numbers of the observations.  0 => the
C                     observations corresponds to a source of known
C                     polarization; otherwise a number between 1 and
C                     NUCAL.
C     IREF       I    Reference antenna to use. 0 => minimize RMS
C                     antenna values.
C   Input / output:
C     D(*)       CPLX Feed parameters, 1 per feed (complex).
C     VST(4,*)   R    Pt. source polarized flux densities (I,Q,U,V). On
C                     input, the initial guess, on output the fitted
C                     values.
C    Output:
C     IER        I    Return error code, 0=>OK, else failed.
C                     1 = Solution is indeterminate.
C                     9 = inadequate flux model
C
C   NOTES: 19-JAN-2001 try to get errors correct (Steven T. Myers)
C-----------------------------------------------------------------------
      INCLUDE 'PPCAL.INC'
      INTEGER   NCMAX, NVMAX, NVMAX1
C                                       Number of calibrators allowed:
      PARAMETER (NCMAX=MAXCAL)
      PARAMETER (NVMAX=2*MAXANT+NCMAX)
      PARAMETER (NVMAX1=2*MAXANT+NCMAX+1)
C
      CHARACTER CSNAME(*)*16
      INTEGER   NOBS, IER, IREF, N, NUCAL
      INTEGER   I, I1, I2, I3, J, K, KNVAR, KOBS, L, ICAL(NOBS),
     *   IJS(2,NOBS), LDA, NVAR, NVAR1, JPVT(NVMAX1), JOB, INFO,
     *   COBS(MAXANT)
      REAL   CHI(2,NOBS), E, PHASE, PHASER, RMS, RTD, SWT, SWT2, DOF,
     *   VST(4,NCMAX), W, WT(4,NOBS), AMP, AMPERR, TEMP
      COMPLEX C(3), D(2,MAXANT), OBS, VOBS(4,NOBS), VSTMOD(4,NOBS), ZZ
      COMPLEX AHA(NVMAX1,NVMAX1), AHB(NVMAX), WORK(NVMAX1)
      DOUBLE PRECISION BHB, DET(2), EPS, S
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL FILL (MAXANT, 0, COBS)
C                                       Check that necessary calibrator
C                                       model values are sent
      DO 10 I = 1,NOBS
         IF (ICAL(I).GT.0) THEN
C                                       Check if Total intensity given.
            J = ICAL(I)
            IF (VST(1,J).LT.1.0E-5) THEN
C                                       No total intensity
               IER = 9
               WRITE (MSGTXT,1000) I, NOBS
               CALL MSGWRT (8)
               GO TO 999
               END IF
C                                       Check if polarization model
C                                       given (Linear polarization may
C                                       be 0).
         ELSE
            IF (ABS (VSTMOD(1,I)).LE.1.0E-10) THEN
               IER = 9
               WRITE (MSGTXT,1005) I, NOBS
               CALL MSGWRT (8)
               IF (WT(1,I).GT.0.0) GO TO 999
               END IF
            END IF
 10      CONTINUE
C                                       Set constants
      RTD = 45.0 / ATAN(1.0)
      IER = 0
      LDA = NVMAX1
      NVAR = 2 * N + NUCAL
C                                       Set up the normal equations
C                                       (only the upper triangular
C                                       portion of AHA is filled in,
C                                       because the matrix is
C                                       conjugate-symmetric):
      DO 20 I = 1,NVAR
         AHB(I) = (0.0, 0.0)
         DO 15 J = I,NVAR
            AHA(I,J) = (0.0, 0.0)
 15         CONTINUE
 20      CONTINUE
      BHB = 0.0D0
C
C   STM: Solve weighted least squares problem
C
C     RMS^2 = SUM_K { WT(K)*( OBS(K) - MOD(K) )^2 } / SUM_K { WT(K) }
C
C   Note: this is the correct form for weighted least-squares if
C   WT(K) = 1/SIG(K)^2 as is the case for (calibrated) AIPS weights
C
      KOBS = 0
      SWT = 0.
      SWT2 = 0.
      DO 50 K = 1,NOBS
         L = ICAL(K)
         I = IJS(1,K)
         J = IJS(2,K)
         ZZ = CMPLX (COS (-CHI(1,K)-CHI(2,K)),
     *      SIN (-CHI(1,K)-CHI(2,K)))
C                                       First RL
         IF (WT(2,K).GT.0.) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
            KOBS = KOBS+1
            I1 = I
            I2 = N + J
            I3 = 2 * N + L
            OBS = VOBS(2,K)
            W = WT(2,K)
            SWT = SWT + W
            SWT2 = SWT2 + W**2
            IF (L.EQ.0) THEN
               C(1) = VSTMOD(1,K)
               C(2) = VSTMOD(1,K)
               OBS = OBS - ZZ * (VSTMOD(2,K) + (0.0, 1.0) * VSTMOD(3,K))
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
            ELSE
               C(1) = VST(1,L)
               C(2) = VST(1,L)
               C(3) = ZZ
               I3 = 2 * N + L
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I1,I3) = AHA(I1,I3) + W * CONJG (C(1)) * C(3)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHA(I2,I3) = AHA(I2,I3) + W * CONJG (C(2)) * C(3)
               AHA(I3,I3) = AHA(I3,I3) + W * CONJG (C(3)) * C(3)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               AHB(I3) = AHB(I3) + W * CONJG (C(3)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
               END IF
            END IF
C                                       Then LR
         IF (WT(3,K).GT.0.) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
            KOBS = KOBS + 1
            I2 = N + I
            I1 = J
            I3 = 2 * N + L
            OBS = CONJG (VOBS(3,K))
            W = WT(3,K)
            SWT = SWT + W
            SWT2 = SWT2 + W**2
            IF (L.EQ.0) THEN
               C(1) = VSTMOD(1,K)
               C(2) = VSTMOD(1,K)
               OBS = OBS - ZZ * (VSTMOD(2,K) + (0.0,1.0) * VSTMOD(3,K))
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
            ELSE
               C(1) = VST(1,L)
               C(2) = VST(1,L)
               C(3) = ZZ
               I3 = 2 * N + L
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I1,I3) = AHA(I1,I3) + W * CONJG (C(1)) * C(3)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHA(I2,I3) = AHA(I2,I3) + W * CONJG (C(2)) * C(3)
               AHA(I3,I3) = AHA(I3,I3) + W * CONJG (C(3)) * C(3)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               AHB(I3) = AHB(I3) + W * CONJG (C(3)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
               END IF
            END IF
 50      CONTINUE
      IF (SWT.LE.0.0) THEN
         MSGTXT = 'NO VALID DATA FOUND'
         CALL MSGWRT (6)
         DO 60 I = 1,N
            D(1,I) = (0.0 ,0.0)
            D(2,I) = (0.0 ,0.0)
 60         CONTINUE
         GO TO 999
         END IF
C                                       Find the average size of the
C                                       nonzero diagonal elements of
C                                       AHA:
      K = 0
      S = 0.0D0
      DO 100 I = 1,NVAR
         IF (REAL (AHA(I,I)).GT.0.0) THEN
            K = K + 1
            S = S + REAL (AHA(I,I))
            END IF
 100     CONTINUE
      IF (K.GT.0) S = S / K
      IF (K.EQ.0) S = 1.0D0
C                                       find ref ant
      IF ((IREF.LE.0) .OR. (IREF.GT.N)) THEN
         IREF = 0
         I1 = 0
         DO 105 I = 1,MAXANT
            IF (COBS(I).GT.I1) THEN
               I1 = COBS(I)
               IREF = I
               END IF
 105        CONTINUE
         WRITE (MSGTXT,1105) IREF
         CALL MSGWRT (4)
         END IF
C                                       For any missing antennas or
C                                       missing calibration sources,
C                                       set the corresponding diagonal
C                                       elements of the normal equations
C                                       matrix to the average of the
C                                       nonzero diagonal elements:
C                                       STM: This seems silly, should collapse
C                                       matrix or something like that
C
      KNVAR = NVAR
      DO 120 I = 1,NVAR
         IF (ABS (AHA(I,I)).EQ.0.0) THEN
            TEMP = S
            AHA(I,I) = CMPLX (TEMP, 0.0)
            KNVAR = KNVAR - 1
            END IF
 120     CONTINUE
C                                       Either constrain the solution
C                                       for the right-hand i.f. of the
C                                       iref'th antenna to zero, or
C                                       modify the normal equations
C                                       matrix so as to approximate the
C                                       least-squares solution of
C                                       minimal Euclidean norm (by
C                                       adding a small positive number
C                                       to the diagonal elements):
      IF ((IREF.GE.1) .AND. (IREF.LE.N)) THEN
         AHA(IREF,IREF) = AHA(IREF,IREF) + CMPLX (10.0 * S,0.0D0)
      ELSE
         EPS = 1.0D-5
         DO 180 I = 1,NVAR
            AHA(I,I) = CMPLX((1.0D0 + EPS), 0.0D0) * AHA(I,I)
 180        CONTINUE
         END IF
C                                       Set up an extra column of AHA so
C                                       that the r.m.s. residual can
C                                       come as a by-product of the
C                                       Cholesky decomposition (see
C                                       p. 8-3 of the  LINPACK guide):
      DO 200 I = 1,NVAR
         AHA(I,NVAR+1) = AHB(I)
 200     CONTINUE
      AHA(NVAR+1,NVAR+1) = BHB
C                                       Get the Cholesky decomposition
C                                       of AHA via a LINPACK routine:
      JOB = 0
      NVAR1 = NVAR + 1
      CALL CCHDC (AHA, LDA, NVAR1, WORK, JPVT, JOB, INFO)
      IF (INFO.LT.NVAR) THEN
         WRITE (MSGTXT,1200) INFO, NVAR
         CALL MSGWRT (7)
         IER = 1
         DO 220 I = 1,N
            D(1,I) = (0.0 ,0.0)
            D(2,I) = (0.0 ,0.0)
 220        CONTINUE
         GO TO 999
         END IF
C                                       (The weighted sum of squared
C                                       residuals = real(AHA(nvar+1,
C                                       nvar+1))**2) Get the solution,
C                                       via a LINPACK routine:
      CALL CPOSL (AHA, LDA, NVAR, AHB)
C
C NOTE: This could have better been computed by used the augmented AHA
C pulling out AHB=AHA(*,NVAR+1) column, and using CTRSL
C
C                                       Calculate the standard error
C                                       estimates, via a LINPACK routine
C                                       (the normalized covariance
C                                       matrix then is given by
C                                       AHA * RMS**2).
C                                       When the least-squares solution
C                                       of minimal Euclidean norm is
C                                       computed (i.e., when iref=0)
C                                       standard error estimates for
C                                       the antenna feed parameters are
C                                       not calculated here (they're
C                                       set to 0), but error estimates
C                                       for the calibrator fluxes can be
C                                       calculated:
C
C
      IF ((KOBS.GT.KNVAR) .AND. (SWT.GT.0.)) THEN
C                                       Old:
C         RMS = ABS (REAL (AHA(NVAR+1,NVAR+1))) *
C     *      SQRT (REAL (KOBS)/(REAL (KOBS - KNVAR) * SWT * 2.0))
C         WRITE (MSGTXT,1220) RMS
C         DOF = REAL(KOBS)
C                                       correct the number of
C                                       degrees-of-freedom for the
C                                       weighting
         DOF = SWT**2 / SWT2
         RMS = ABS (REAL (AHA(NVAR+1,NVAR+1))) *
     *      SQRT (1./( (DOF - REAL(KNVAR)) * SWT * 2.0))
         WRITE (MSGTXT,1220) RMS, DOF
         CALL MSGWRT (4)
         JOB = 1
         CALL CPODI (AHA, LDA, NVAR, DET, JOB)
C                                       This returns the Inv(A) in upper
C                                       half (and det(A) in DET for
C                                       job=11)
         IF (IREF.NE.0) AHA(IREF,IREF) = (0.0, 0.0)
         END IF
C                                       Now, print the results.
C                                       Then return:
      DO 320 I = 1,N
         D(1,I) = AHB(I)
         WRITE (MSGTXT,1300) I
         CALL MSGWRT (4)
C        E = RMS * SQRT (REAL (AHA(I,I)))
C                                       STM: AHA is now Inv(AHA) and
C                                       thus need to be scaled by SWT
         E = RMS * SQRT (REAL (AHA(I,I))*SWT)
         AMP = ABS (D(1,I))
         AMPERR = SQRT (2.0) * E
         IF (AMP .EQ. 0.0) THEN
            PHASE = 0.0
            PHASER = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(1,I)), REAL (D(1,I))) * RTD
            PHASER = E / ABS (D(1,I)) * RTD
            END IF
         WRITE (MSGTXT,1301) AMP, AMPERR, PHASE, PHASER
         CALL MSGWRT (4)
C                                       Other hand poln.
         D(2,I) = CONJG (AHB(N+I))
C        E = RMS * SQRT (REAL (AHA(N+I,N+I)))
C                                       STM: AHA is now Inv(AHA) and
C                                       thus need to be scaled by SWT
         E = RMS * SQRT (REAL (AHA(N+I,N+I))*SWT)
         AMP = ABS (D(2,I))
         AMPERR = SQRT (2.0) * E
         IF (AMP .EQ. 0.0) THEN
            PHASE = 0.0
            PHASER = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(2,I)), REAL (D(2,I))) * RTD
            PHASER = E / ABS (D(2,I)) * RTD
            END IF
         WRITE (MSGTXT,1302) AMP, AMPERR, PHASE, PHASER
         CALL MSGWRT (4)
 320     CONTINUE
      IF (NUCAL.GT.0) THEN
         DO 340 I = 1,NUCAL
            VST(2,I) = REAL (AHB(2*N+I))
            VST(3,I) = AIMAG (AHB(2*N+I))
C           E = RMS * SQRT (REAL (AHA(2*N+I,2*N+I)))
C                                       STM: AHA is now Inv(AHA) and
C                                       thus need to be scaled by SWT
            E = RMS * SQRT (REAL (AHA(2*N+I,2*N+I))*SWT)
            WRITE (MSGTXT,1320) I, CSNAME(I)
            CALL MSGWRT (4)
            WRITE (MSGTXT,1321) VST(2,I), VST(3,I), E, E
            CALL MSGWRT (4)
            AMP = ABS (AHB(2*N+I))
            AMPERR = SQRT (2.0) * E
            IF (AMP .GT. 0.0) THEN
               PHASE = 0.5 * ATAN2 (VST(3,I), VST(2,I)) * RTD
               PHASER = 0.5 * E / AMP * RTD
            ELSE
               PHASE = 0.0
               PHASER = 0.0
               END IF
            WRITE (MSGTXT,1322) AMP, AMPERR
            CALL MSGWRT (4)
            WRITE (MSGTXT,1323) PHASE, PHASER
            CALL MSGWRT (4)
 340        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TOTAL INTENSITIES MUST BE PROVIDED',I10,' OF',I10)
 1005 FORMAT ('POLARIZATION MODEL NOT PROVIDED SAMPLE',I10,' OF',I10)
 1105 FORMAT ('LPCALC: Setting reference antenna to',I3)
 1200 FORMAT ('SOLUTION IS INDETERMINATE:',I5,' <',I5)
 1220 FORMAT ('RMS residual =',1PE12.3,' DOF =',0PF10.2)
 1300 FORMAT ('Interferometer Element',I4)
 1301 FORMAT ('  R: Amp = ',F8.5,'+/-',F8.5,' Phase(deg) = ',F7.2,
     *   '+/-',F7.2)
 1302 FORMAT ('  L: Amp = ',F8.5,'+/-',F8.5,' Phase(deg) = ',F7.2,
     *   '+/-',F7.2)
 1320 FORMAT ('Calibration source ',I3,3X,A)
 1321 FORMAT ('  Q+iU=(',F10.5,',',F10.5,') +/- (',F10.6,',',F10.6,
     *   ') Jy')
 1322 FORMAT ('  Pol. inten. =',F10.5,' +/-',F10.6,' Jy')
 1323 FORMAT ('  Pol. angle  =',F7.2,' +/-', F7.3,' deg')
      END
      SUBROUTINE XYCALC (NOBS, VOBS, WT, CHI, IJS, N, D, VSTMOD,
     *   NUCAL, VST, ICAL, IREF, CSNAME, IER)
C-----------------------------------------------------------------------
C   Subroutine to determine instrumental and source polarizations from
C   linearly polarized feeds.
C   Inputs:
C      NOBS   I        The number of observations, an observation
C                      consists of four visibility measuments on one
C                      baseline (XX,YY,XY,YX)
C      VOBS   C(4,*)   Observed visibilities (XX,XY,YX,YY)
C      WT     R(4,*)   Weights of the visibilities
C      CHI    R(2,*)   Parallactic angles of the feeds for each antenna
C                      used for the observed visibilities.  For
C                      equatorial mounts use 0.
C      IJS    I(2,*)   Antenna numbers of the observations.  IJS(1,n)
C                      should be the lower number, IJS(2,n) the upper.
C      N      I        Number of antennas, actually the antenna number
C                      of the highest numbered antenna.
C      VSTMOD C(4,*)   Model polarizations per source (I,Q,U,V).
C                      Values only need be supplied to observations
C                      corresponding to sources which are NOT having
C                      point source polarization parameters fitted.
C      NUCAL  I        Number of unknown calibrator sources, i.e. the
C                      number of sources whose polarizations are to be
C                      determined.
C      ICAL   I(*)     Source numbers of the observations.  0 => the
C                      observations corresponds to a source of known
C                      polarization; otherwise a number between 1 and
C                      NUCAL.
C      IREF   I        Reference antenna to use. 0 => minimize RMS
C                      antenna values.
C    Output:
C       D     C(*)     Feed parameters, 1 per feed (complex).
C       VST   R(4,*)   Pt. source polarized flux densities (I,Q,U,V). On
C                      input, the initial guess, on output the fitted
C                      values.
C       IER   I        Return error code, 0=>OK, else failed.
C                      1 = Solution is indeterminate.
C
C  NOTES: STM 19-JAN-2001 try to get errors correct (Steven T. Myers)
C  See LPCALC for details
C-----------------------------------------------------------------------
      INTEGER   NOBS, IJS(2,*), N, NUCAL, ICAL(*), IREF, IER
      REAL      WT(4,*), CHI(2,*), VST(4,*)
      COMPLEX   D(2,*), VOBS(4,*), VSTMOD(4,*)
      CHARACTER CSNAME(*)*16
C
      INCLUDE 'PPCAL.INC'
      INTEGER   NCMAX, NVMAX, NVMAX1
C                                       Number of calibrators allowed:
      PARAMETER (NCMAX=MAXCAL)
      PARAMETER (NVMAX=2*MAXANT+NCMAX*3)
      PARAMETER (NVMAX1=2*MAXANT+NCMAX*3+1)
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   I, I1, I2, I3, I4, I5, J, K, KNVAR, KOBS, L, LDA, NVAR,
     *   NVAR1, JPVT(NVMAX1), JOB, INFO, COBS(MAXANT)
      REAL   E3, E4, E5, PHASE, PHASER, RMS, RTD, W, AMP, AMPERR,
     *   TEMP, CSUM, CDIF, SSUM, SDIF, SWT, SWT2, DOF
      COMPLEX C(5), OBS, MODL, AHA(NVMAX1,NVMAX1), AHB(NVMAX),
     *   WORK(NVMAX1)
      DOUBLE PRECISION BHB, DET(2), EPS, S
C-----------------------------------------------------------------------
      RTD = 45.0 / ATAN (1.0)
      IER = 0
      LDA = NVMAX1
      NVAR = (2 * N) + (3 * NUCAL)
C                                       Set up the normal equations
C                                       (only the upper triangular
C                                       portion of AHA is filled in,
C                                       because the matrix is
C                                       conjugate-symmetric):
      DO 20 I = 1,NVAR
         AHB(I) = CMPLX (0.0, 0.0)
         DO 10 J = I,NVAR
            AHA(I,J) = CMPLX (0.0, 0.0)
 10         CONTINUE
 20      CONTINUE
      BHB = 0.0D0
      KOBS = 0
      SWT = 0.
      SWT2 = 0.
      CALL FILL (MAXANT, 0, COBS)
C                                       Parameters 1=DX, 2=DY, 3=Q, 4=U,
C                                       5=V
C                                       Fill matrices
      DO 50 K = 1,NOBS
C                                       Cal. no. 0=> known poln.
         L = ICAL(K)
C                                       1st antenna number
         I = IJS(1,K)
C                                       2nd antenna number
         J = IJS(2,K)
C                                       Sin and cosine of sums and
C                                       differences of the antenna
C                                       parallactic angles.
         CSUM = 0.5 * COS (CHI(1,K)+CHI(2,K))
         SSUM = 0.5 * SIN (CHI(1,K)+CHI(2,K))
         CDIF = 0.5 * COS (CHI(1,K)-CHI(2,K))
         SDIF = 0.5 * SIN (CHI(1,K)-CHI(2,K))
C                                       Matrix indices
         I3 = 2 * N + L
         I4 = 2 * N + NUCAL + L
         I5 = 2 * N + 2 * NUCAL + L
C                                       Contribution from XX
         IF (WT(1,K).GT.0.0) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
            KOBS = KOBS + 1
            W = WT(1,K)
            SWT = SWT + W
            SWT2 = SWT2 + W**2
            IF (L.EQ.0) THEN
C                                       Source of known polarization:
C                                       XX has no contributions to the
C                                       matrix.
            ELSE
C                                       Source of unknown polarization
C                                       Derivatives of obs wrt
C                                       parameters C(1) = d(XX)/d(DXa):
               C(1) = CMPLX (0.0, 0.0)
C                                       C(2) = d(XX)/d(DXb*)
               C(2) = CMPLX (0.0, 0.0)
C                                       C(3) = d(XX)/d(Q)
               C(3) = CMPLX (SSUM, 0.0)
C                                       C(4) = d(XX)/d(U)
               C(4) = CMPLX (-CSUM, 0.0)
C                                       C(5) = d(XX)/d(V)
               C(5) = CMPLX (0.0, -SDIF)
C                                       Residual (= Obs)
               OBS = VOBS(1,K)
               AHA(I3,I3) = AHA(I3,I3) + W * CONJG (C(3)) * C(3)
               AHA(I3,I4) = AHA(I3,I4) + W * CONJG (C(3)) * C(4)
               AHA(I3,I5) = AHA(I3,I5) + W * CONJG (C(3)) * C(5)
               AHA(I4,I4) = AHA(I4,I4) + W * CONJG (C(4)) * C(4)
               AHA(I4,I5) = AHA(I4,I5) + W * CONJG (C(4)) * C(5)
               AHA(I5,I5) = AHA(I5,I5) + W * CONJG (C(5)) * C(5)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               AHB(I3) = AHB(I3) + W * CONJG (C(3)) * OBS
               AHB(I4) = AHB(I4) + W * CONJG (C(4)) * OBS
               AHB(I5) = AHB(I5) + W * CONJG (C(5)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
               END IF
            END IF
C                                       Contribution from XY
         IF (WT(2,K).GT.0.) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
C                                       Matrix indices
            I1 = I
            I2 = N + J
            KOBS = KOBS + 1
            W = WT(2,K)
            SWT = SWT + W
            SWT2 = SWT2 + W**2
            IF (L.EQ.0) THEN
C                                       Source of known polarization
C                                       Derivatives of obs wrt
C                                       parameters C(1) = d(XY)/d(DXa):
               C(1) = VSTMOD(1,K)
C                                       C(2) = d(XY)/d(DYb*)
               C(2) = VSTMOD(1,K)
C                                       Residual
               MODL = -VSTMOD(1,K)*SDIF - VSTMOD(2,K)*CSUM -
     *            VSTMOD(3,K)*SSUM - (CMPLX(0.0,1.0) *
     *            VSTMOD(4,K)*CDIF)
               OBS = VOBS(2,K) - MODL
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
            ELSE
C                                       Source of unknown polarization
C                                       Derivatives of obs wrt
C                                       parameters C(1) = d(XY)/d(DXa):
               C(1) = CMPLX (VST(1,L), 0.0)
C                                       C(2) = d(XY)/d(DYb*)
               C(2) = CMPLX (VST(1,L), 0.0)
C                                       C(3) = d(XY)/d(Q)
               C(3) = CMPLX (-CSUM, 0.0)
C                                       C(4) = d(XY)/d(U)
               C(4) = CMPLX (-SSUM, 0.0)
C                                       C(5) = d(XY)/d(V)
               C(5) = CMPLX (0.0, -CDIF)
C                                       Residual (= Obs)
               OBS = VOBS(2,K)
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I1,I3) = AHA(I1,I3) + W * CONJG (C(1)) * C(3)
               AHA(I1,I4) = AHA(I1,I4) + W * CONJG (C(1)) * C(4)
               AHA(I1,I5) = AHA(I1,I5) + W * CONJG (C(2)) * C(5)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHA(I2,I3) = AHA(I2,I3) + W * CONJG (C(2)) * C(3)
               AHA(I2,I4) = AHA(I2,I4) + W * CONJG (C(2)) * C(4)
               AHA(I2,I5) = AHA(I2,I5) + W * CONJG (C(2)) * C(5)
               AHA(I3,I3) = AHA(I3,I3) + W * CONJG (C(3)) * C(3)
               AHA(I3,I4) = AHA(I3,I4) + W * CONJG (C(3)) * C(4)
               AHA(I3,I5) = AHA(I3,I5) + W * CONJG (C(3)) * C(5)
               AHA(I4,I4) = AHA(I4,I4) + W * CONJG (C(4)) * C(4)
               AHA(I4,I5) = AHA(I4,I5) + W * CONJG (C(4)) * C(5)
               AHA(I5,I5) = AHA(I5,I5) + W * CONJG (C(5)) * C(5)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               AHB(I3) = AHB(I3) + W * CONJG (C(3)) * OBS
               AHB(I4) = AHB(I4) + W * CONJG (C(4)) * OBS
               AHB(I5) = AHB(I5) + W * CONJG (C(5)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
               END IF
            END IF
C                                       Contribution from YX
         IF (WT(3,K).GT.0.) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
C                                       Matrix indices
            I1 = N + I
            I2 = J
            KOBS = KOBS + 1
            W = WT(3,K)
            SWT = SWT + W
            SWT2 = SWT2 + W**2
            IF (L.EQ.0) THEN
C                                       Source of known polarization
C                                       Derivatives of obs wrt
C                                       parameters C(1) = d(YX)/d(DYa):
               C(1) = VSTMOD(1,K)
C                                       C(2) = d(YX)/d(DXb*)
               C(2) = VSTMOD(1,K)
C                                       Residual
               MODL = VSTMOD(1,K)*SDIF - VSTMOD(2,K)*CSUM -
     *            VSTMOD(3,K)*SSUM + (CMPLX (0.0,1.0) *
     *            VSTMOD(4,K)*CDIF)
               OBS = VOBS(3,K) - MODL
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
            ELSE
C                                       Source of unknown polarization
C                                       Derivatives of obs wrt
C                                       parameters C(1) = d(YX)/d(DXa):
               C(1) = CMPLX (VST(1,L), 0.0)
C                                       C(2) = d(YX)/d(DXb*)
               C(2) = CMPLX (VST(1,L), 0.0)
C                                       C(3) = d(YX)/d(Q)
               C(3) = CMPLX (-CSUM, 0.0)
C                                       C(4) = d(YX)/d(U)
               C(4) = CMPLX (-SSUM, 0.0)
C                                       C(5) = d(YX)/d(V)
               C(5) = CMPLX (0.0, CDIF)
C                                       Residual (= Obs)
               OBS = VOBS(3,K)
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I1,I3) = AHA(I1,I3) + W * CONJG (C(1)) * C(3)
               AHA(I1,I4) = AHA(I1,I4) + W * CONJG (C(1)) * C(4)
               AHA(I1,I5) = AHA(I1,I5) + W * CONJG (C(2)) * C(5)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHA(I2,I3) = AHA(I2,I3) + W * CONJG (C(2)) * C(3)
               AHA(I2,I4) = AHA(I2,I4) + W * CONJG (C(2)) * C(4)
               AHA(I2,I5) = AHA(I2,I5) + W * CONJG (C(2)) * C(5)
               AHA(I3,I3) = AHA(I3,I3) + W * CONJG (C(3)) * C(3)
               AHA(I3,I4) = AHA(I3,I4) + W * CONJG (C(3)) * C(4)
               AHA(I3,I5) = AHA(I3,I5) + W * CONJG (C(3)) * C(5)
               AHA(I4,I4) = AHA(I4,I4) + W * CONJG (C(4)) * C(4)
               AHA(I4,I5) = AHA(I4,I5) + W * CONJG (C(4)) * C(5)
               AHA(I5,I5) = AHA(I5,I5) + W * CONJG (C(5)) * C(5)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               AHB(I3) = AHB(I3) + W * CONJG (C(3)) * OBS
               AHB(I4) = AHB(I4) + W * CONJG (C(4)) * OBS
               AHB(I5) = AHB(I5) + W * CONJG (C(5)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
               END IF
            END IF
C                                       Contribution from YY
         IF (WT(4,K).GT.0.0) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
            KOBS = KOBS + 1
            W = WT(4,K)
            SWT = SWT + W
            SWT2 = SWT2 + W**2
            IF (L.EQ.0) THEN
C                                       Source of known polarization:
C                                       YY has no contributions to the
C                                       matrix.
            ELSE
C                                       Source of unknown polarization
C                                       Derivatives of obs wrt
C                                       parameters C(1) = d(YY)/d(DXa):
               C(1) = CMPLX (0.0, 0.0)
C                                       C(2) = d(YY)/d(DXb*)
               C(2) = CMPLX (0.0, 0.0)
C                                       C(3) = d(YY)/d(Q)
               C(3) = CMPLX (-SSUM, 0.0)
C                                       C(4) = d(YY)/d(U)
               C(4) = CMPLX (CSUM, 0.0)
C                                       C(5) = d(YY)/d(V)
               C(5) = CMPLX (0.0, -SDIF)
C                                       Residual (= Obs)
               OBS = VOBS(4,K)
               AHA(I3,I3) = AHA(I3,I3) + W * CONJG (C(3)) * C(3)
               AHA(I3,I4) = AHA(I3,I4) + W * CONJG (C(3)) * C(4)
               AHA(I3,I5) = AHA(I3,I5) + W * CONJG (C(3)) * C(5)
               AHA(I4,I4) = AHA(I4,I4) + W * CONJG (C(4)) * C(4)
               AHA(I4,I5) = AHA(I4,I5) + W * CONJG (C(4)) * C(5)
               AHA(I5,I5) = AHA(I5,I5) + W * CONJG (C(5)) * C(5)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               AHB(I3) = AHB(I3) + W * CONJG (C(3)) * OBS
               AHB(I4) = AHB(I4) + W * CONJG (C(4)) * OBS
               AHB(I5) = AHB(I5) + W * CONJG (C(5)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
               END IF
            END IF
 50      CONTINUE
C                                       find ref ant
      IF ((IREF.LE.0) .OR. (IREF.GT.N)) THEN
         IREF = 0
         I1 = 0
         DO 55 I = 1,MAXANT
            IF (COBS(I).GT.I1) THEN
               I1 = COBS(I)
               IREF = I
               END IF
 55         CONTINUE
         WRITE (MSGTXT,1055) IREF
         CALL MSGWRT (4)
         END IF
C                                       Find the average size of the
C                                       nonzero diagonal elements of
C                                       AHA:
      K = 0
      S = 0.0D0
      DO 100 I = 1,NVAR
         IF (REAL (AHA(I,I)).GT.0.0) THEN
            K = K + 1
            S = S + REAL (AHA(I,I))
            END IF
 100     CONTINUE
      IF (K.GT.0) S = S / K
      IF (K.EQ.0) S = 1.0D0
C                                       For any missing antennas or
C                                       missing calibration sources,
C                                       set the corresponding diagonal
C                                       elements of the normal equations
C                                       matrix to the average of the
C                                       nonzero diagonal elements:
      KNVAR = NVAR
      DO 120 I = 1,NVAR
         IF (ABS (AHA(I,I)).EQ.0.0) THEN
            TEMP = S
            AHA(I,I) = CMPLX (TEMP, 0.0)
            KNVAR = KNVAR - 1
            END IF
 120     CONTINUE
C                                       Either constrain the solution
C                                       for the right-hand i.f. of the
C                                       iref'th antenna to zero, or
C                                       modify the normal equations
C                                       matrix so as to approximate the
C                                       least-squares solution of
C                                       minimal Euclidean norm (by
C                                       adding a small positive number
C                                       to the diagonal elements):
      IF ((IREF.GE.1) .AND. (IREF.LE.N)) THEN
         AHA(IREF,IREF) = AHA(IREF,IREF) + CMPLX (10.0 * S, 0.0D0)
      ELSE
         EPS = 1.0D-5
         DO 180 I = 1,NVAR
            AHA(I,I) = CMPLX ((1.0D0 + EPS),0.0D0) * AHA(I,I)
 180        CONTINUE
         END IF
C                                       Set up an extra column of AHA so
C                                       that the r.m.s. residual can
C                                       come as a by-product of the
C                                       Cholesky decomposition (see
C                                       p. 8-3 of the  LINPACK guide):
      DO 200 I = 1,NVAR
         AHA(I,NVAR+1) = AHB(I)
 200     CONTINUE
      AHA(NVAR+1,NVAR+1) = BHB
C                                       Get the Cholesky decomposition
C                                       of AHA via a LINPACK routine:
      JOB = 0
      NVAR1 = NVAR + 1
      CALL CCHDC (AHA, LDA, NVAR1, WORK, JPVT, JOB, INFO)
      IF (INFO.LT.NVAR) THEN
         WRITE (MSGTXT,1200) INFO, NVAR
 1200 FORMAT ('SOLUTION IS INDETERMINATE:',I5,' <',I5)
         CALL MSGWRT (8)
         IER = 1
C                                       Failed - zero results
         DO 220 I = 1,N
            D(1,I) = CMPLX (0.0 ,0.0)
            D(2,I) = CMPLX (0.0 ,0.0)
 220        CONTINUE
         GO TO 999
         END IF
C                                       (The weighted sum of squared
C                                       residuals = real(AHA(nvar+1,
C                                       nvar+1))**2) Get the solution,
C                                       via a LINPACK routine:
      CALL CPOSL (AHA, LDA, NVAR, AHB)
C                                       Calculate the standard error
C                                       estimates, via a LINPACK routine
C                                       (the normalized covariance
C                                       matrix then is given by
C                                       AHA * RMS**2).
C                                       When the least-squares solution
C                                       of minimal Euclidean norm is
C                                       computed (i.e., when iref=0)
C                                       standard error estimates for
C                                       the antenna feed parameters are
C                                       not calculated here (they're
C                                       set to 0), but error estimates
C                                       for the calibrator fluxes can be
C                                       calculated:
      IF ((KOBS.GT.KNVAR) .AND. (SWT.GT.0.0)) THEN
C                                       Compute RMS residual
C
C         RMS = ABS (REAL (AHA(NVAR+1,NVAR+1))) *
C     *      SQRT (REAL (KOBS)/(REAL (KOBS - KNVAR) * SWT * 2.0))
C         WRITE (MSGTXT,1220) RMS
C                                       STM 1-19-01 (see LPCALC)
         DOF = (SWT**2) / SWT2
         RMS = ABS (REAL (AHA(NVAR+1,NVAR+1))) *
     *      SQRT (1./( (DOF - REAL (KNVAR)) * SWT * 2.0))
         WRITE (MSGTXT,1220) RMS, DOF
         CALL MSGWRT (4)
         JOB = 1
         CALL CPODI (AHA, LDA, NVAR, DET, JOB)
         IF (IREF.EQ.0) THEN
            DO 300 I = 1,2*N
               AHA(I,I) = CMPLX (0.0, 0.0)
 300           CONTINUE
         ELSE
            AHA(IREF,IREF) = CMPLX (0.0, 0.0)
            END IF
         END IF
C                                       Now, print the results.
C                                       Then return:
      DO 320 I = 1,N
         D(1,I) = AHB(I)
         WRITE (MSGTXT,1300) I
         CALL MSGWRT (4)
         E3 = RMS * SQRT (REAL (AHA(I,I))*SWT)
         AMP = ABS (D(1,I))
         AMPERR = SQRT (2.0) * E3
         IF (AMP .EQ. 0.0) THEN
            PHASE = 0.0
            PHASER = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(1,I)), REAL (D(1,I))) * RTD
            PHASER = E3 / ABS (D(1,I)) * RTD
            END IF
         WRITE (MSGTXT,1301) AMP, AMPERR, PHASE, PHASER
         CALL MSGWRT (4)
C                                       Other hand poln.
         D(2,I) = CONJG (AHB(N+I))
         E3 = RMS * SQRT (REAL (AHA(N+I,N+I))*SWT)
         AMP = ABS (D(2,I))
         AMPERR = SQRT (2.0) * E3
         IF (AMP .EQ. 0.0) THEN
            PHASE = 0.0
            PHASER = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(2,I)), REAL (D(2,I))) * RTD
            PHASER = E3 / ABS (D(2,I)) * RTD
            END IF
         WRITE (MSGTXT,1302) AMP, AMPERR, PHASE, PHASER
         CALL MSGWRT (8)
 320     CONTINUE
C                                       List calibrator polarizations
      IF (NUCAL.GT.0) THEN
         I3 = (2 * N)
         I4 = (2 * N) + NUCAL
         I5 = (2 * N) + 2 * NUCAL
         DO 340 I = 1,NUCAL
            VST(2,I) = REAL (AHB(I3+I))
            VST(3,I) = REAL (AHB(I4+I))
            VST(4,I) = REAL (AHB(I5+I))
            E3 = RMS * SQRT (REAL (AHA(I3+I,I3+I))*SWT)
            E4 = RMS * SQRT (REAL (AHA(I4+I,I4+I))*SWT)
            E5 = RMS * SQRT (REAL (AHA(I5+I,I5+I))*SWT)
            WRITE (MSGTXT,1320) I, CSNAME(I)
            CALL MSGWRT (4)
C                                       Q and U pol
            WRITE (MSGTXT,1321) VST(2,I), VST(3,I), E3, E4
            CALL MSGWRT (4)
C                                       V pol
            WRITE (MSGTXT,1322) VST(4,I), E5
            CALL MSGWRT (4)
C                                       Linear poln.
            AMP = SQRT ((VST(2,I)**2) + (VST(3,I)**2))
            AMPERR = SQRT (E3*E3 + E4*E4)
            IF (AMP .GT. 0.0) THEN
               PHASE = 0.5 * ATAN2 (VST(3,I), VST(2,I)) * RTD
               PHASER = 0.5 * E3 / AMP * RTD
            ELSE
               PHASE = 0.0
               PHASER = 0.0
               END IF
            WRITE (MSGTXT,1323) AMP, AMPERR
            CALL MSGWRT (4)
            WRITE (MSGTXT,1324) PHASE, PHASER
            CALL MSGWRT (4)
 340        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1055 FORMAT ('XYCALC: Setting reference antenna to',I3)
 1220 FORMAT ('RMS residual =',1PE12.3,' DOF =',0PF10.2)
 1300 FORMAT ('Interferometer Element',I4)
 1301 FORMAT ('  X: Amp = ',F8.5,'+/-',F8.5,' Phase(deg) = ',F7.2,
     *   '+/-',F7.2)
 1302 FORMAT ('  Y: Amp = ',F8.5,'+/-',F8.5,' Phase(deg) = ',F7.2,
     *   '+/-',F7.2)
 1320 FORMAT ('Calibration source ',I3,3X,A)
 1321 FORMAT ('  Q+iU=(',F10.5,',',F10.5,') +/- (',F10.6,',',F10.6,
     *   ') Jy')
 1322 FORMAT ('  V = ',F10.6,' +/- ',F10.7)
 1323 FORMAT ('  Pol. inten. =',F10.5,' +/-',F10.6,' Jy')
 1324 FORMAT ('  Pol. angle =',F7.2,' +/-', F7.3,' deg')
      END
      SUBROUTINE CCHDC (A, LDA, P, WORK, JPVT, JOB, INFO)
C-----------------------------------------------------------------------
      INTEGER LDA,P,JPVT(*),JOB,INFO
      COMPLEX A(LDA,*),WORK(*)
C
C     CCHDC COMPUTES THE CHOLESKY DECOMPOSITION OF A POSITIVE DEFINITE
C     MATRIX.  A PIVOTING OPTION ALLOWS THE USER TO ESTIMATE THE
C     CONDITION OF A POSITIVE DEFINITE MATRIX OR DETERMINE THE RANK
C     OF A POSITIVE SEMIDEFINITE MATRIX.
C
C     ON ENTRY
C
C         A      COMPLEX(LDA,P).
C                A CONTAINS THE MATRIX WHOSE DECOMPOSITION IS TO
C                BE COMPUTED.  ONLT THE UPPER HALF OF A NEED BE STORED.
C                THE LOWER PART OF THE ARRAY A IS NOT REFERENCED.
C
C         LDA    INTEGER.
C                LDA IS THE LEADING DIMENSION OF THE ARRAY A.
C
C         P      INTEGER.
C                P IS THE ORDER OF THE MATRIX.
C
C         WORK   COMPLEX.
C                WORK IS A WORK ARRAY.
C
C         JPVT   INTEGER(P).
C                JPVT CONTAINS INTEGERS THAT CONTROL THE SELECTION
C                OF THE PIVOT ELEMENTS, IF PIVOTING HAS BEEN REQUESTED.
C                EACH DIAGONAL ELEMENT A(K,K)
C                IS PLACED IN ONE OF THREE CLASSES ACCORDING TO THE
C                VALUE OF JPVT(K).
C
C                   IF JPVT(K) .GT. 0, THEN X(K) IS AN INITIAL
C                                      ELEMENT.
C
C                   IF JPVT(K) .EQ. 0, THEN X(K) IS A FREE ELEMENT.
C
C                   IF JPVT(K) .LT. 0, THEN X(K) IS A FINAL ELEMENT.
C
C                BEFORE THE DECOMPOSITION IS COMPUTED, INITIAL ELEMENTS
C                ARE MOVED BY SYMMETRIC ROW AND COLUMN INTERCHANGES TO
C                THE BEGINNING OF THE ARRAY A AND FINAL
C                ELEMENTS TO THE END.  BOTH INITIAL AND FINAL ELEMENTS
C                ARE FROZEN IN PLACE DURING THE COMPUTATION AND ONLY
C                FREE ELEMENTS ARE MOVED.  AT THE K-TH STAGE OF THE
C                REDUCTION, IF A(K,K) IS OCCUPIED BY A FREE ELEMENT
C                IT IS INTERCHANGED WITH THE LARGEST FREE ELEMENT
C                A(L,L) WITH L .GE. K.  JPVT IS NOT REFERENCED IF
C                JOB .EQ. 0.
C
C        JOB     INTEGER.
C                JOB IS AN INTEGER THAT INITIATES COLUMN PIVOTING.
C                IF JOB .EQ. 0, NO PIVOTING IS DONE.
C                IF JOB .NE. 0, PIVOTING IS DONE.
C
C     ON RETURN
C
C         A      A CONTAINS IN ITS UPPER HALF THE CHOLESKY FACTOR
C                OF THE MATRIX A AS IT HAS BEEN PERMUTED BY PIVOTING.
C
C         JPVT   JPVT(J) CONTAINS THE INDEX OF THE DIAGONAL ELEMENT
C                OF A THAT WAS MOVED INTO THE J-TH POSITION,
C                PROVIDED PIVOTING WAS REQUESTED.
C
C         INFO   CONTAINS THE INDEX OF THE LAST POSITIVE DIAGONAL
C                ELEMENT OF THE CHOLESKY FACTOR.
C
C     FOR POSITIVE DEFINITE MATRICES INFO = P IS THE NORMAL RETURN.
C     FOR PIVOTING WITH POSITIVE SEMIDEFINITE MATRICES INFO WILL
C     IN GENERAL BE LESS THAN P.  HOWEVER, INFO MAY BE GREATER THAN
C     THE RANK OF A, SINCE ROUNDING ERROR CAN CAUSE AN OTHERWISE ZERO
C     ELEMENT TO BE POSITIVE. INDEFINITE SYSTEMS WILL ALWAYS CAUSE
C     INFO TO BE LESS THAN P.
C
C     LINPACK. THIS VERSION DATED 03/19/79 .
C     J.J. DONGARRA AND G.W. STEWART, ARGONNE NATIONAL LABORATORY AND
C     UNIVERSITY OF MARYLAND.
C
C
C     BLAS CAXPY,CSWAP
C     FORTRAN SQRT,REAL,CONJG
C
C     INTERNAL VARIABLES
C-----------------------------------------------------------------------
      INTEGER PU,PL,PLP1,J,JP,JT,K,KB,KM1,KP1,L,MAXL
      INTEGER ITEMP, N1
      COMPLEX TEMP
      REAL MAXDIA
      LOGICAL SWAPK,NEGK
      DATA N1 /1/
C-----------------------------------------------------------------------
      PL = 1
      PU = 0
      INFO = P
      IF (JOB .EQ. 0) GO TO 160
C
C        PIVOTING HAS BEEN REQUESTED. REARRANGE THE
C        THE ELEMENTS ACCORDING TO JPVT.
C
         DO 70 K = 1, P
            SWAPK = JPVT(K) .GT. 0
            NEGK = JPVT(K) .LT. 0
            JPVT(K) = K
            IF (NEGK) JPVT(K) = -JPVT(K)
            IF (.NOT.SWAPK) GO TO 60
               IF (K .EQ. PL) GO TO 50
                  ITEMP = PL-1
                  CALL CSWAP(ITEMP,A(1,K),N1,A(1,PL),N1)
                  TEMP = A(K,K)
                  A(K,K) = A(PL,PL)
                  A(PL,PL) = TEMP
                  A(PL,K) = CONJG(A(PL,K))
                  PLP1 = PL + 1
                  IF (P .LT. PLP1) GO TO 40
                  DO 30 J = PLP1, P
                     IF (J .GE. K) GO TO 10
                        TEMP = CONJG(A(PL,J))
                        A(PL,J) = CONJG(A(J,K))
                        A(J,K) = TEMP
                     GO TO 20
   10                CONTINUE
                     IF (J .EQ. K) GO TO 20
                        TEMP = A(K,J)
                        A(K,J) = A(PL,J)
                        A(PL,J) = TEMP
   20                CONTINUE
   30             CONTINUE
   40             CONTINUE
                  JPVT(K) = JPVT(PL)
                  JPVT(PL) = K
   50          CONTINUE
               PL = PL + 1
   60       CONTINUE
   70    CONTINUE
         PU = P
         IF (P .LT. PL) GO TO 150
         DO 140 KB = PL, P
            K = P - KB + PL
            IF (JPVT(K) .GE. 0) GO TO 130
               JPVT(K) = -JPVT(K)
               IF (PU .EQ. K) GO TO 120
                  ITEMP = K-1
                  CALL CSWAP(ITEMP,A(1,K),N1,A(1,PU),N1)
                  TEMP = A(K,K)
                  A(K,K) = A(PU,PU)
                  A(PU,PU) = TEMP
                  A(K,PU) = CONJG(A(K,PU))
                  KP1 = K + 1
                  IF (P .LT. KP1) GO TO 110
                  DO 100 J = KP1, P
                     IF (J .GE. PU) GO TO 80
                        TEMP = CONJG(A(K,J))
                        A(K,J) = CONJG(A(J,PU))
                        A(J,PU) = TEMP
                     GO TO 90
   80                CONTINUE
                     IF (J .EQ. PU) GO TO 90
                        TEMP = A(K,J)
                        A(K,J) = A(PU,J)
                        A(PU,J) = TEMP
   90                CONTINUE
  100             CONTINUE
  110             CONTINUE
                  JT = JPVT(K)
                  JPVT(K) = JPVT(PU)
                  JPVT(PU) = JT
  120          CONTINUE
               PU = PU - 1
  130       CONTINUE
  140    CONTINUE
  150    CONTINUE
  160 CONTINUE
      DO 270 K = 1, P
C
C        REDUCTION LOOP.
C
         MAXDIA = REAL(A(K,K))
         KP1 = K + 1
         MAXL = K
C
C        DETERMINE THE PIVOT ELEMENT.
C
         IF (K .LT. PL .OR. K .GE. PU) GO TO 190
            DO 180 L = KP1, PU
               IF (REAL(A(L,L)) .LE. MAXDIA) GO TO 170
                  MAXDIA = REAL(A(L,L))
                  MAXL = L
  170          CONTINUE
  180       CONTINUE
  190    CONTINUE
C
C        QUIT IF THE PIVOT ELEMENT IS NOT POSITIVE.
C
         IF (MAXDIA .GT. 0.0E0) GO TO 200
            INFO = K - 1
C     ......EXIT
            GO TO 280
  200    CONTINUE
         IF (K .EQ. MAXL) GO TO 210
C
C           START THE PIVOTING AND UPDATE JPVT.
C
            KM1 = K - 1
            CALL CSWAP(KM1,A(1,K),N1,A(1,MAXL),N1)
            A(MAXL,MAXL) = A(K,K)
            A(K,K) = CMPLX(MAXDIA, 0.0E0)
            JP = JPVT(MAXL)
            JPVT(MAXL) = JPVT(K)
            JPVT(K) = JP
            A(K,MAXL) = CONJG(A(K,MAXL))
  210    CONTINUE
C
C        REDUCTION STEP. PIVOTING IS CONTAINED ACROSS THE ROWS.
C
         WORK(K) = CMPLX(SQRT(REAL(A(K,K))),0.0E0)
         A(K,K) = WORK(K)
         IF (P .LT. KP1) GO TO 260
         DO 250 J = KP1, P
            IF (K .EQ. MAXL) GO TO 240
               IF (J .GE. MAXL) GO TO 220
                  TEMP = CONJG(A(K,J))
                  A(K,J) = CONJG(A(J,MAXL))
                  A(J,MAXL) = TEMP
               GO TO 230
  220          CONTINUE
               IF (J .EQ. MAXL) GO TO 230
                  TEMP = A(K,J)
                  A(K,J) = A(MAXL,J)
                  A(MAXL,J) = TEMP
  230          CONTINUE
  240       CONTINUE
            A(K,J) = A(K,J)/WORK(K)
            WORK(J) = CONJG(A(K,J))
            TEMP = -A(K,J)
            ITEMP = J-K
            CALL CAXPY(ITEMP,TEMP,WORK(KP1),N1,A(KP1,J),N1)
  250    CONTINUE
  260    CONTINUE
  270 CONTINUE
  280 CONTINUE
      RETURN
      END
      SUBROUTINE CPOSL(A,LDA,N,B)
C-----------------------------------------------------------------------
      INTEGER LDA,N
      COMPLEX A(LDA,*),B(*)
C
C     CPOSL SOLVES THE COMPLEX HERMITIAN POSITIVE DEFINITE SYSTEM
C     A * X = B
C     USING THE FACTORS COMPUTED BY CPOCO OR CPOFA.
C
C     ON ENTRY
C
C        A       COMPLEX(LDA, N)
C                THE OUTPUT FROM CPOCO OR CPOFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        B       COMPLEX(N)
C                THE RIGHT HAND SIDE VECTOR.
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
C        A ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES
C        SINGULARITY BUT IT IS USUALLY CAUSED BY IMPROPER SUBROUTINE
C        ARGUMENTS.  IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED
C        CORRECTLY AND  INFO .EQ. 0 .
C
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
C     WITH  P  COLUMNS
C           CALL CPOCO(A,LDA,N,RCOND,Z,INFO)
C           IF (RCOND IS TOO SMALL .OR. INFO .NE. 0) GO TO ...
C           DO 10 J = 1, P
C              CALL CPOSL(A,LDA,N,C(1,J))
C        10 CONTINUE
C
C     LINPACK.  THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS CAXPY,CDOTC
C
C     INTERNAL VARIABLES
C
      COMPLEX CDOTC,T
      INTEGER K,KB
      INTEGER ITEMP, N1
      DATA N1 /1/
C-----------------------------------------------------------------------
C
C     SOLVE CTRANS(R)*Y = B
C
      DO 10 K = 1, N
         ITEMP = K-1
         T = CDOTC(ITEMP,A(1,K),N1,B(1),N1)
         B(K) = (B(K) - T)/A(K,K)
   10 CONTINUE
C
C     SOLVE R*X = Y
C
      DO 20 KB = 1, N
         K = N + 1 - KB
         B(K) = B(K)/A(K,K)
         T = -B(K)
         ITEMP = K-1
         CALL CAXPY(ITEMP,T,A(1,K),N1,B(1),N1)
   20 CONTINUE
      RETURN
      END
      SUBROUTINE CPODI (A, LDA, N, DET, JOB)
C-----------------------------------------------------------------------
      INTEGER   LDA, N, JOB
      COMPLEX   A(LDA,*)
      DOUBLE PRECISION DET(2)
C
C     CPODI COMPUTES THE DETERMINANT AND INVERSE OF A CERTAIN
C     COMPLEX HERMITIAN POSITIVE DEFINITE MATRIX (SEE BELOW)
C     USING THE FACTORS COMPUTED BY CPOCO, CPOFA OR CQRDC.
C
C     ON ENTRY
C
C        A       COMPLEX(LDA, N)
C                THE OUTPUT  A  FROM CPOCO OR CPOFA
C                OR THE OUTPUT  X  FROM CQRDC.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        JOB     INTEGER
C                = 11   BOTH DETERMINANT AND INVERSE.
C                = 01   INVERSE ONLY.
C                = 10   DETERMINANT ONLY.
C
C     ON RETURN
C
C        A       IF CPOCO OR CPOFA WAS USED TO FACTOR  A  THEN
C                CPODI PRODUCES THE UPPER HALF OF INVERSE(A) .
C                IF CQRDC WAS USED TO DECOMPOSE  X  THEN
C                CPODI PRODUCES THE UPPER HALF OF INVERSE(CTRANS(X)*X)
C                WHERE CTRANS(X) IS THE CONJUGATE TRANSPOSE.
C                ELEMENTS OF  A  BELOW THE DIAGONAL ARE UNCHANGED.
C                IF THE UNITS DIGIT OF JOB IS ZERO,  A  IS UNCHANGED.
C
C        DET     DP(2)
C                DETERMINANT OF  A  OR OF  CTRANS(X)*X  IF REQUESTED.
C                OTHERWISE NOT REFERENCED.
C                DETERMINANT = DET(1) * 10.0**DET(2)
C                WITH  1.0 .LE. DET(1) .LT. 10.0
C                OR  DET(1) .EQ. 0.0 .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
C        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
C        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
C        AND IF CPOCO OR CPOFA HAS SET INFO .EQ. 0 .
C
C     LINPACK.  THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS CAXPY,CSCAL
C     FORTRAN CONJG,MOD,REAL
C
C     INTERNAL VARIABLES
C
      COMPLEX T
      REAL S
      INTEGER J,JM1,K,KP1, I, ITEMP
C-----------------------------------------------------------------------
C
C     COMPUTE DETERMINANT
C
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0E0
         DET(2) = 0.0E0
         S = 10.0E0
         DO 50 I = 1, N
            DET(1) = REAL(A(I,I))**2*DET(1)
C        ...EXIT
            IF (DET(1) .EQ. 0.0E0) GO TO 60
   10       IF (DET(1) .GE. 1.0E0) GO TO 20
               DET(1) = S*DET(1)
               DET(2) = DET(2) - 1.0E0
            GO TO 10
   20       CONTINUE
   30       IF (DET(1) .LT. S) GO TO 40
               DET(1) = DET(1)/S
               DET(2) = DET(2) + 1.0E0
            GO TO 30
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     COMPUTE INVERSE(R)
C
      IF (MOD(JOB,10) .EQ. 0) GO TO 140
         DO 100 K = 1, N
            A(K,K) = (1.0E0,0.0E0)/A(K,K)
            T = -A(K,K)
            ITEMP = K-1
            CALL CSCAL(ITEMP,T,A(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1, N
               T = A(K,J)
               A(K,J) = (0.0E0,0.0E0)
               CALL CAXPY(K,T,A(1,K),1,A(1,J),1)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        FORM  INVERSE(R) * CTRANS(INVERSE(R))
C
         DO 130 J = 1, N
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 120
            DO 110 K = 1, JM1
               T = CONJG(A(K,J))
               CALL CAXPY(K,T,A(1,J),1,A(1,K),1)
  110       CONTINUE
  120       CONTINUE
            T = CONJG(A(J,J))
            CALL CSCAL(J,T,A(1,J),1)
  130    CONTINUE
  140 CONTINUE
      RETURN
      END
      COMPLEX FUNCTION CDOTC(N,CX,INCX,CY,INCY)
C-----------------------------------------------------------------------
C     FORMS THE DOT PRODUCT OF TWO VECTORS, CONJUGATING THE FIRST
C     VECTOR.
C     JACK DONGARRA, LINPACK,  3/11/78.
C-----------------------------------------------------------------------
      COMPLEX CX(*),CY(*),CTEMP
      INTEGER I,INCX,INCY,IX,IY,N
C-----------------------------------------------------------------------
      CTEMP = (0.0,0.0)
      CDOTC = (0.0,0.0)
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
        CTEMP = CTEMP + CONJG(CX(IX))*CY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      CDOTC = CTEMP
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 DO 30 I = 1,N
        CTEMP = CTEMP + CONJG(CX(I))*CY(I)
   30 CONTINUE
      CDOTC = CTEMP
      RETURN
      END
      SUBROUTINE PCLCOP (TMPDSK, TMPCNO, DISK, CNOSCR, BC, EC, ICHAN,
     *   BUFFER, BUFSZ, IRET)
C-----------------------------------------------------------------------
C   Routine to copy selected data from one data file to another
C   optionally applying calibration and editing information.
C   Ionospheric Faraday rotation is written to a scratch file as the
C   copy is made. The input file should have been opened with UVGET.
C   Both files will be closed on return from PCLCOP.
C     Note: UVGET returns the information necessary to catalog the
C   output file.  The output file will be compressed if necessary at
C   completion of PCLCOP.
C   Inputs:
C      TMPDSK   I       0 => make temp disk with all channels, > 0
C                       => use it, < 0 don't do.
C      TMPCNO           CNO of new temp disk
C      DISK     I       Disk number for catalogd output file.
C                       If .LE. 0 then the output file is a /CFILES/
C                       scratch file.
C      BC       I       Begin channel for desired output incl smooth
C      EC       I       End channel for desired output incl smooth
C      ICHAN    I       Desired output channel
C      BUFFER   R(*)    Work buffer for writing.
C      BUFSZ    I       Size of BUFFER in bytes.
C   Input via common:
C      LREC     I       (/UVHDR/) length of vis. record in R words.
C      NRPARM   I       (/UVHDR/) number of R random parameters.
C   In/out:
C      CNOSCR   I       Catalog slot number for if cataloged file;
C                       /CFILES/ scratch file number if a scratch file,
C                       IF DISK=CNOSCR=0 then the scratch is created.
C                       On output = Scratch file number if created.
C   In/out via common:
C      CATBLK   I(256)  Catalog header block from UVGET
C                       on output with actual no. records
C      NVIS     I       (/UVHDR/) Number of vis. records.
C      IONSCR   I       /CFILES/ scratch file number for ionospheric
C                       data.
C
C   Output:
C      IRET     I       Error code: 0 => OK,
C                          > 0 => failed, abort process.
C   Usage notes:
C   (1) UVGET with OPCODE='INIT' MUST be called before PCLCOP to setup
C       for calibration, editing and data translation.  If an output
C       cataloged file is to be created this should be done after the
C       call to UVGET.
C   (2) Uses AIPS LUNs 24 and 27 and 32
C-----------------------------------------------------------------------
      INTEGER   TMPDSK, TMPCNO, DISK, CNOSCR, BC, EC, ICHAN, BUFSZ, IRET
      REAL      BUFFER(*)
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER NAME*48
      INTEGER   VOL, LUN, IONLUN, FIND, BIND, LENBU, NIO, CNO, BO, VO,
     *   I, XCOUNT, ISIZE, IA1, IA2, IONBSZ, NAX, NPIX(2), IONSZ,
     *   IONFND, IONBO, WIN(4), IONBND, LRECO, JERR, TNIF, TNF, TNS,
     *   LOFF, LUNI, FINDI, BINDI, NVAL, IVAL, IBC, IEC, WCOUNT, NGOOD,
     *   IP, JP
      REAL      IONBUF(UVBFSS), TBUFF(UVBFSS)
      LOGICAL   DOTEMP
      PARAMETER (LUN = 24)
      PARAMETER (LUNI = 32)
      PARAMETER (IONLUN = 27)
      PARAMETER (IONBSZ = 2 * UVBFSS)
      INCLUDE 'PCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA BO, VO /1,0/
      DATA IONBO /1/
      DATA WIN /4*0/
C-----------------------------------------------------------------------
      IRET = 0
      LENBU = 1
C                                       Fix up for frequency and IF
C                                       averaging.
      LRECO = NRPARM + 4 * 3
C
      TNF = CATBLK(KINAX+JLOCF)
      IF ((JLOCIF.GE.0) .AND. (AVGIF)) THEN
         TNIF = CATBLK(KINAX+JLOCIF)
      ELSE
         TNIF = 1
         END IF
C
      CATBLK(KINAX+JLOCF) = 1
      IF (JLOCIF.GE.0) THEN
         CATBLK(KINAX+JLOCIF) = 1
         END IF
C                                       Calculate size of scratch file
C                                       for ionospheric data:
      IF (IONSCR.LE.0) THEN
         NAX = 2
         NPIX(1) = 2
         NPIX(2) = NVIS
         CALL MAPSIZ (NAX, NPIX, IONSZ)
C                                       Create scratch file for
C                                       ionospheric data:
         CALL SCREAT (IONSZ, IONBUF, IRET)
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) WRITE (MSGTXT, 1000)
            IF (IRET.GT.1) WRITE (MSGTXT, 1001) IRET
            GO TO 990
            END IF
         IONSCR = NSCR
         END IF
C                                       Open scratch file for ionspheric
C                                       data:
      CALL ZPHFIL ('SC', SCRVOL(IONSCR), SCRCNO(IONSCR), 1, NAME, IRET)
      CALL ZOPEN (IONLUN, IONFND, SCRVOL(IONSCR), NAME, .TRUE.,
     *   .FALSE., .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 1004) IRET
         GO TO 990
         END IF
C                                       Initialize ionsphere scratch
C                                       file for write:
      CALL MINIT ('WRIT', IONLUN, IONFND, 2, NVIS, WIN, IONBUF, IONBSZ,
     *   IONBO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 1006) IRET
         GO TO 990
         END IF
C                                       copy full IF to scratch
      IF ((TMPDSK.EQ.0) .AND. (TMPCNO.EQ.0)) THEN
         WRITE (MSGTXT,1010) BIF
         CALL MSGWRT (3)
         CALL UVSIZE (LREC, NVIS, ISIZE)
         CALL SCREAT (ISIZE, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) WRITE (MSGTXT,1000)
            IF (IRET.GT.1) WRITE (MSGTXT,1001) IRET
            GO TO 990
            END IF
         TMPDSK = SCRVOL(NSCR)
         TMPCNO = SCRCNO(NSCR)
C                                       Update CATBLK.
         CALL CATIO ('UPDT', TMPDSK, TMPCNO, CATBLK, 'REST', BUFFER,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1002) IRET
            END IF
         CALL ZPHFIL ('SC', TMPDSK, TMPCNO, 1, NAME, IRET)
C                                       Open output file.
         CALL ZOPEN (LUN, FIND, TMPDSK, NAME, .TRUE., .FALSE., .TRUE.,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1003) IRET
            GO TO 990
            END IF
C                                       Init vis file for write
         CALL UVINIT ('WRIT', LUN, FIND, NVIS, VO, LREC, LENBU,
     *      BUFSZ, BUFFER, BO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1005) IRET
            GO TO 990
            END IF
         XCOUNT = 0
         DO 50 I = 1,NVIS
C                                       Read old.
            CALL UVGET ('READ', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
            IF (IRET.LT.0) GO TO 60
            IF (IRET.NE.0) GO TO 999
C                                       Calculate ionospheric Faraday
C                                       rotation from LAMBDA and IFR
C                                       (maintained by UVGET) and save
C                                       it (fake it if IFR is blanked):
            CALL MDISK ('WRIT', IONLUN, IONFND, IONBUF, IONBND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT, 1007) IRET
               GO TO 990
               END IF
            IF (ILOCB.GE.0) THEN
               IA1 = BUFFER(BIND+ILOCB) / 256 + 0.1
               IA2 = BUFFER(BIND+ILOCB) - IA1 * 256 + 0.1
            ELSE
               IA1 = BUFFER(BIND+ILOCA1) + 0.1
               IA2 = BUFFER(BIND+ILOCA2) + 0.1
               END IF
            IF (IFR(IA1).NE.FBLANK) THEN
               IONBUF(IONBND) = LAMBDA(LOFF)**2 * IFR(IA1)
            ELSE
               IONBUF(IONBND) = FBLANK
               END IF
            IF (IFR(IA2) .NE. FBLANK) THEN
               IONBUF(IONBND+1) = LAMBDA(LOFF)**2 * IFR(IA2)
            ELSE
               IONBUF(IONBND+1) = FBLANK
               END IF
C                                       write the uv data
            XCOUNT = XCOUNT + 1
            NIO = 1
            CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1008) IRET
               GO TO 990
               END IF
 50         CONTINUE
 60      NIO = 0
         CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1008) IRET
            GO TO 990
            END IF
C                                       Close input
         CALL ZCLOSE (LUN, FIND, IRET)
         NVIS = XCOUNT
         TMPCNT = XCOUNT
         IF (NVIS.LE.0) THEN
            CALL UVGET ('CLOS', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
            GO TO 999
            END IF
         END IF
C                                       temp input of real input?
      DOTEMP = (TMPDSK.GT.0) .AND. (TMPCNO.GT.0)
C                                       open input temp file
      IF (DOTEMP) THEN
         NVIS = TMPCNT
C                                       close input now
         CALL UVGET ('CLOS', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
         CALL ZPHFIL ('SC', TMPDSK, TMPCNO, 1, NAME, IRET)
C                                       Open output file.
         CALL ZOPEN (LUNI, FINDI, TMPDSK, NAME, .TRUE., .FALSE., .TRUE.,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1003) IRET
            GO TO 990
            END IF
C                                       Init vis file for write
         CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC, LENBU, BUFSZ,
     *      BUFF2, BO, BINDI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1005) IRET
            GO TO 990
            END IF
         IBC = BC
         IEC = EC
      ELSE
         IBC = BCHAN
         IEC = ECHAN
         END IF
C                                       Create output file if necessary
      IF ((DISK.EQ.0) .AND. (CNOSCR.EQ.0)) THEN
C                                       Determine size.
         CALL UVSIZE (LRECO, NVIS, ISIZE)
C                                       Create scratch file.
         CALL SCREAT (ISIZE, BUFFER, IRET)
         CNOSCR = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) WRITE (MSGTXT,1000)
            IF (IRET.GT.1) WRITE (MSGTXT,1001) IRET
            GO TO 990
            END IF
C                                       Update CATBLK.
         CALL CATIO ('UPDT', SCRVOL(CNOSCR), SCRCNO(CNOSCR), CATBLK,
     *      'REST', BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1002) IRET
            END IF
         END IF
C                                       Set output file name.
      IF (DISK.GT.0) THEN
         CNO = CNOSCR
         VOL = DISK
         CALL ZPHFIL ('UV', VOL, CNOSCR, 1, NAME, IRET)
      ELSE
         CNO = SCRCNO(CNOSCR)
         VOL = SCRVOL(CNOSCR)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCR), 1, NAME, IRET)
         END IF
C                                       Open output file.
      CALL ZOPEN (LUN, FIND, VOL, NAME, .TRUE., .FALSE., .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1003) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      CALL UVINIT ('WRIT', LUN, FIND, NVIS, VO, LRECO, LENBU, BUFSZ,
     *    BUFFER, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         GO TO 990
         END IF
C                                       Copy file
C
C      IF (JLOCIF .GE. 0) THEN
C         TNIF = CATBLK(KINAX+JLOCIF)
C      ELSE
C         TNIF = 1
C         END IF
C      TNF = CATBLK(KINAX+JLOCF)
      TNS = CATBLK(KINAX+JLOCS)
      LOFF = (BIF - 1) * NLAMDA + 1
      NVAL = TNS * 3 * (EC - BC + 1)
      IVAL = NRPARM + (BC - 1) * TNS * 3
      WCOUNT = 0
      XCOUNT = 0
      DO 100 I = 1,NVIS
C                                       Read old.
         IF (DOTEMP) THEN
            CALL UVDISK ('READ', LUNI, FINDI, BUFF2, NIO, BINDI, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL RCOPY (NRPARM, BUFF2(BINDI), BUFFER(BIND))
            CALL RCOPY (NVAL, BUFF2(BINDI+IVAL), TBUFF)
            IP = BINDI + IVAL - 1
            NGOOD = 0
            DO 30 JP = 1,NVAL,3
               TBUFF(JP) = BUFF2(IP+1)
               TBUFF(JP+1) = BUFF2(IP+2)
               TBUFF(JP+2) = BUFF2(IP+3)
               IF (TBUFF(JP+2).GT.0.0) NGOOD = NGOOD + 1
               IP = IP + 3
 30            CONTINUE
            IF (NGOOD.LE.0) GO TO 100
         ELSE
            CALL UVGET ('READ', BUFFER(BIND), TBUFF, IRET)
            IF (IRET.GT.0) GO TO 999
            IF (IRET.LT.0) GO TO 110
            END IF
C                                       Average in Frequency and IF
         IF (DOLINE) THEN
            CALL PCLSMO (TNS, IBC, IEC, ICHAN, PSMTAB, CHNSEL(1,1,BIF),
     *         TBUFF, BUFFER(BIND+NRPARM), IRET)
         ELSE
            CALL PCLAVG (TNIF, TNF, TNS, CHNSEL(1,1,BIF), TBUFF,
     *         BUFFER(BIND+NRPARM), IRET)
            END IF
         IF (IRET.GT.0) GO TO 100
         XCOUNT = XCOUNT + 1
C                                       Calculate ionospheric Faraday
C                                       rotation from LAMBDA and IFR
C                                       (maintained by UVGET) and save
C                                       it (fake it if IFR is blanked):
         IF (.NOT.DOTEMP) THEN
            CALL MDISK ('WRIT', IONLUN, IONFND, IONBUF, IONBND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT, 1007) IRET
               GO TO 990
               END IF
            IF (ILOCB.GE.0) THEN
               IA1 = BUFFER(BIND+ILOCB) / 256 + 0.1
               IA2 = BUFFER(BIND+ILOCB) - IA1 * 256 + 0.1
            ELSE
               IA1 = BUFFER(BIND+ILOCA1) + 0.1
               IA2 = BUFFER(BIND+ILOCA2) + 0.1
               END IF
            IF (IFR(IA1).NE.FBLANK) THEN
               IONBUF(IONBND) = LAMBDA(LOFF)**2 * IFR(IA1)
            ELSE
               IONBUF(IONBND) = FBLANK
               END IF
            IF (IFR(IA2) .NE. FBLANK) THEN
               IONBUF(IONBND+1) = LAMBDA(LOFF)**2 * IFR(IA2)
            ELSE
               IONBUF(IONBND+1) = FBLANK
               END IF
            END IF
C                                       Write new
         NIO = 1
         CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1008) IRET
            GO TO 990
            END IF
 100     CONTINUE
C                                       Flush output
 110  CALL MDISK ('FINI', IONLUN, IONFND, IONBUF, IONBND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 1007) IRET
         GO TO 990
         END IF
      NIO = 0
      CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1008) IRET
         GO TO 990
         END IF
C                                       Close input
      IF (DOTEMP) THEN
         CALL ZCLOSE (LUNI, FINDI, IRET)
      ELSE
         CALL UVGET ('CLOS', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, VOL, CNO, LUN, CATBLK, IRET)
C                                      Put vis. count in CATBLK
      CATBLK(KIGCN) = NVIS
C                                       Update CATBLK.
      CALL CATIO ('UPDT', VOL, CNO, CATBLK, 'REST', BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1002) IRET
         END IF
      CALL UVPGET (JERR)
C                                       Close output
      CALL ZCLOSE (IONLUN, IONFND, IRET)
      CALL ZCLOSE (LUN, FIND, IRET)
      IRET = 0
      IF (NVIS.LE.0) IRET = -1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCLCOP: TOO LITTLE DISK SPACE FOR SCRATCH FILE')
 1001 FORMAT ('PCLCOP: ERROR ',I5,' CREATING SCRATCH FILE')
 1002 FORMAT ('PCLCOP: ERROR ',I5,' UPDATING SCRATCH FILE CATBLK')
 1003 FORMAT ('PCLCOP: ERROR ',I5,' OPENING OUTPUT FILE')
 1004 FORMAT ('PCLCOP: ERROR ',I5,' OPENING IONOSPHERE FILE')
 1005 FORMAT ('PCLCOP: ERROR ',I5,' INIT. OUTPUT FILE')
 1006 FORMAT ('PCLCOP: ERROR ',I5,' INIT. IONOSPHERE FILE')
 1007 FORMAT ('PCLCOP: ERROR ',I5,' WRITING IONOSPHERE FILE')
 1008 FORMAT ('PCLCOP: ERROR ',I5,' WRITING OUTPUT FILE')
 1010 FORMAT ('PCLCOP: Copying data for IF',I3,' to work file')
      END
      SUBROUTINE PCLSMO (TNS, BCHAN, ECHAN, ICHAN, PSMTAB, CHNSEL,
     *   INBUF, OUTBUF, IRET)
C-----------------------------------------------------------------------
C   Routine to average visibilities in frequency and IF to produce a
C   single average visibility with 4 polarizations.
C   Inputs:
C      TNS      I           Number of stokes
C      BCHAN    I           begin channel number
C      ECHAN    I           end channel number
C      ICHAN    I           channel we are computing
C      CHNSEL   I(3,20,*)   Ranges of channel where * is IF starting
C                           at current BIF
C      INBUF    R(3,*)      Input visibility (re, im, wt)
C   Inputs from common (DUVH.INC)
C      JLOCIF   I           IF pointer
C      JLOCF    I           frequency pointer
C      JLOCS    I           Stokes pointer
C      INCIF    I           IF increment
C      INCF     I           frequency increment
C      INCS     I           Stokes increment
C   Outputs:
C      OUTBUF   R(3,4)      Output visibility
C      IRET     I           0 all are valid, else number not valid
C-----------------------------------------------------------------------
      INTEGER   TNS, BCHAN, ECHAN, ICHAN, CHNSEL(3,20), IRET
      REAL      PSMTAB(*), INBUF(*), OUTBUF(12)
C
      INTEGER   LOOPS, LOOPF, INDEX, OUTDEX, LC
      REAL      SUMWT, SUMRE, SUMIM, WT, XNORM, CHWT
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Loop over Stokes'
      IRET = 0
      DO 40 LOOPS = 1,TNS
         OUTDEX = (LOOPS-1) * 3 + 1
         SUMWT = 0.0
         SUMRE = 0.0
         SUMIM = 0.0
C                                       Sum over freq and IF
         INDEX = 1 + (LOOPS-1)*INCS
         DO 20 LOOPF = BCHAN,ECHAN
            LC = ABS (LOOPF-ICHAN) + 1
            CHWT = PSMTAB(LC)
            CALL WANTCH (CHNSEL, LOOPF, CHWT)
            WT = MAX (0.0, INBUF(INDEX+2)) * CHWT
            SUMRE = SUMRE + INBUF(INDEX)*WT
            SUMIM = SUMIM + INBUF(INDEX+1)*WT
            SUMWT = SUMWT + WT
            INDEX = INDEX + INCF
 20         CONTINUE
         IF (SUMWT.GT.1.0E-10) THEN
            XNORM = 1.0 / SUMWT
         ELSE
            IRET = IRET + 1
            XNORM = 0.0
            END IF
         OUTBUF(OUTDEX)   = SUMRE * XNORM
         OUTBUF(OUTDEX+1) = SUMIM * XNORM
         OUTBUF(OUTDEX+2) = SUMWT
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PCLAVG (TNIF, TNF, TNS, CHNSEL, INBUF, OUTBUF, IRET)
C-----------------------------------------------------------------------
C   Routine to average visibilities in frequency and IF to produce a
C   single average visibility with 4 polarizations.
C   Inputs:
C      TNIF     I           Number of IFs to average
C      TNF      I           Number of frequencies to average
C      TNS      I           Number of stokes
C      CHNSEL   I(3,20,*)   Ranges of channel where * is IF starting
C                           at current BIF
C      INBUF    R(3,*)      Input visibility (re, im, wt)
C   Inputs from common (DUVH.INC)
C      JLOCIF   I           IF pointer
C      JLOCF    I           frequency pointer
C      JLOCS    I           Stokes pointer
C      INCIF    I           IF increment
C      INCF     I           frequency increment
C      INCS     I           Stokes increment
C   Outputs:
C      OUTBUF   R(3,4)      Output visibility
C      IRET     I           0 all are valid, else number not valid
C-----------------------------------------------------------------------
      INTEGER   TNIF, TNF, TNS, CHNSEL(3,20,*), IRET
      REAL      INBUF(*), OUTBUF(12)
C
      INTEGER   LOOPS, LOOPIF, LOOPF, INDEX, OUTDEX
      REAL      SUMWT, SUMRE, SUMIM, WT, XNORM, CHWT
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Loop over Stokes'
      IRET = 0
      DO 40 LOOPS = 1,TNS
         OUTDEX = (LOOPS-1) * 3 + 1
         SUMWT = 0.0
         SUMRE = 0.0
         SUMIM = 0.0
C                                       Sum over freq and IF
         DO 30 LOOPIF = 1,TNIF
            INDEX = 1 + (LOOPS-1)*INCS + (LOOPIF-1)*INCIF
            DO 20 LOOPF = 1,TNF
               CHWT = 1.0
               CALL WANTCH (CHNSEL(1,1,LOOPIF), LOOPF, CHWT)
               WT = MAX (0.0, INBUF(INDEX+2)) * CHWT
               SUMRE = SUMRE + INBUF(INDEX)*WT
               SUMIM = SUMIM + INBUF(INDEX+1)*WT
               SUMWT = SUMWT + WT
               INDEX = INDEX + INCF
 20            CONTINUE
 30         CONTINUE
         IF (SUMWT.GT.1.0E-10) THEN
            XNORM = 1.0 / SUMWT
         ELSE
            XNORM = 0.0
            IRET = IRET + 1
            END IF
         OUTBUF(OUTDEX)   = SUMRE * XNORM
         OUTBUF(OUTDEX+1) = SUMIM * XNORM
         OUTBUF(OUTDEX+2) = SUMWT
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RPCALC (NOBS, VOBS, WT, CHI, IJS, N, D, VSTMOD,
     *   NUCAL, VST, ICAL, IREF, PRTLV, CSNAME, IER)
C-----------------------------------------------------------------------
C   Subroutine to determine instrumental and source polarizations.
C   This routine assumes that the sources have constant polarization
C   angle and the same, but scaled, distribution as the total intensity.
C   Will currently handle 20 calibrators.
C   Fits model:
C     RL/II       = ((Q+iU)/II) + DRa*exp(-i*2*chia) +
C                   conj(DLb)*exp(-i*2*chib)
C     conj(LR/II) = ((Q+iU)/conj(II))+ DRb*exp(-i*2*chib) +
C                   conj(DLa)*exp(-i*2*chia)
C
C     where II = 0.5*(RR+LL), chia, chib are the parallactic angles of
C     antennas a and b and DLx and DRx are the instrumental parameters.
C
C   Inputs:
C     NOBS       I    The number of observations, an observation
C                     consists of four visibility measuments on one
C                     baseline (RR, RL, LR, LL if using circular feeds).
C     VOBS(4,*)  CPLX Observed visibilities (RR, RL, LR, LL)
C     WT(4,*)    R    Weights of the visibilities
C     CHI(2,*)   R    Parallactic angles of the feeds for each antenna
C                     used for the observed visibilities.  For
C                     equatorial mounts use 0.
C     IJS(2,*)   I    Antenna numbers of the observations.  IJS(1,n)
C                     should be the lower number, IJS(2,n) the upper.
C     N          I    Number of antennas, actually the antenna number
C                     of the highest numbered antenna.
C     VSTMOD(4,*)CPLX Model polarizations per observation (I,Q,U,V).
C                     Values only need be supplied to observations
C                     corresponding to sources which are NOT having
C                     source polarization parameters fitted.
C     NUCAL      I    Number of unknown calibrator sources, i.e. the
C                     number of sources whose polarizations are to be
C                     determined.
C     ICAL(*)    I    Source numbers of the observations.  0 => the
C                     observations corresponds to a source of known
C                     polarization; otherwise a number between 1 and
C                     NUCAL.
C     IREF       I    Reference antenna to use. 0 => minimize RMS
C                     antenna values.
C     PRTLV      I    If .ge. 2 then print residuals to fit.
C   Input / output:
C     D(*)       CPLX Feed parameters, 1 per feed (complex).
C     VST(4,*)   R    Pt. source polarized flux densities (I,Q,U,V). On
C                     input, the initial guess, on output the fitted
C                     values.
C    Output:
C     IER        I    Return error code, 0=>OK, else failed.
C                     1 = Solution is indeterminate.
C                     9 = inadequate flux model
C
C  NOTES: STM 19-JAN-2001 try to get errors correct (Steven T. Myers)
C  See LPCALC for details
C-----------------------------------------------------------------------
      INCLUDE 'PPCAL.INC'
      INTEGER   NCMAX, NVMAX, NVMAX1
C                                       Number of calibrators allowed:
      PARAMETER (NCMAX=MAXCAL)
      PARAMETER (NVMAX=2*MAXANT+NCMAX)
      PARAMETER (NVMAX1=2*MAXANT+NCMAX+1)
C
      CHARACTER CSNAME(*)*16
      INTEGER   NOBS, IER, IREF, N, NUCAL, PRTLV
      INTEGER   I, I1, I2, I3, J, K, KNVAR, KOBS, L, ICAL(NOBS),
     *   IJS(2,NOBS),  LDA, NVAR, NVAR1, JPVT(NVMAX1), JOB, INFO, IRLOI,
     *   IRLOR, IRLRR, IRLRI, ILROI, ILROR, ILRRI, ILRRR, ICHIA, ICHIB,
     *   IROUND, COBS(MAXANT)
      REAL   CHI(2,NOBS), E, PHASE, PHASER, RMS, RTD, SWT, SWT2, IWT,
     *   VST(4,NCMAX), W, WT(4,NOBS), AMP, AMPERR, TEMP, RLR, RLI, LRR,
     *   LRI, SUM2, COUNT2, DOF
      COMPLEX C(3), D(2,MAXANT), OBS, VOBS(4,NOBS), VSTMOD(4,NOBS),
     *   ZZA, ZZB, IPOLI, MODEL, RESID, T1, T2, T3
      COMPLEX AHA(NVMAX1,NVMAX1), AHB(NVMAX), WORK(NVMAX1), POLI
      DOUBLE PRECISION BHB, DET(2), EPS, S
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check that necessary calibrator
C                                       model values are sent
      DO 10 I = 1,NOBS
         IF (ICAL(I).GT.0) THEN
C                                       Check if Total intensity given.
            J = ICAL(I)
            IF (VST(1,J).LT.1.0E-5) THEN
C                                       No total intensity
               IER = 9
               WRITE (MSGTXT,1000) I, NOBS
               CALL MSGWRT (8)
               GO TO 999
               END IF
         ELSE
C                                       Check if polarization model
C                                       given (Linear polarization may
C                                       be 0).
            IF (ABS (VSTMOD(1,I)).LE.1.0E-10) THEN
               IER = 9
               WRITE (MSGTXT,1005) I, NOBS
               CALL MSGWRT (8)
               IF (WT(1,K).GT.0.0) GO TO 999
               END IF
            END IF
 10      CONTINUE
C                                       Set constants
      RTD = 45.0 / ATAN(1.0)
      IER = 0
      LDA = NVMAX1
      NVAR = 2 * N + NUCAL
C                                       Set up the normal equations
C                                       (only the upper triangular
C                                       portion of AHA is filled in,
C                                       because the matrix is
C                                       conjugate-symmetric):
      DO 20 I = 1,NVAR
         AHB(I) = (0.0, 0.0)
         DO 15 J = I,NVAR
            AHA(I,J) = (0.0, 0.0)
 15         CONTINUE
 20      CONTINUE
      BHB = 0.0D0
      KOBS = 0
      SWT = 0.
      SWT2 = 0.
C                                       Parameters 1=DR, 2=DL, 3=(Q+IU)
C                                       Fill matrices
      CALL FILL (MAXANT, 0, COBS)
      DO 50 K = 1,NOBS
C                                       Cal. no. 0=> known poln.
         L = ICAL(K)
C                                       1st antenna number
         I = IJS(1,K)
C                                       2nd antenna number
         J = IJS(2,K)
C                                       Complex numbers with phase equal
C                                       to twice the parallactic
C                                       angles.
         ZZA = CMPLX (COS (2.0*(CHI(1,K))), SIN (2.0*(CHI(1,K))))
         ZZB = CMPLX (COS (2.0*(CHI(2,K))), SIN (2.0*(CHI(2,K))))
C                                       Total intensity
         IF ((WT(1,K).GT.0.0) .OR. (WT(4,K).GT.0.0)) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
            POLI = (VOBS(1,K)*WT(1,K) + VOBS(4,K)*WT(4,K)) /
     *         (WT(1,K) + WT(4,K))
            IWT = (REAL (POLI)**2 + AIMAG (POLI)**2)
            IF (L.GT.0) THEN
               IF (IWT.LT.(1.0E-2*VST(1,L))) GO TO 50
            ELSE
               IF (IWT.LT.(1.0E-2*ABS (VSTMOD(1,K)))) GO TO 50
               END IF
         ELSE
            GO TO 50
            END IF
C                                       inverse of POLI
         IPOLI = CMPLX(1.0, 0.0) / POLI
C                                       Contribution from RL
         IF (WT(2,K).GT.0.) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
            KOBS = KOBS+1
C                                       Matrix indices
            I1 = I
            I2 = N + J
            I3 = 2 * N + L
            OBS = VOBS(2,K) * IPOLI
            W = WT(2,K) * IWT
            SWT = SWT + W
            SWT2 = SWT2 + W**2
            IF (L.EQ.0) THEN
C                                       Source of known polarization:
C                                       C(1) = d(RL)/d(DRa):
               C(1) = ZZA
C                                       C(2) = d(RL)/d(DLb*)
               C(2) = ZZB
               OBS = OBS - (VSTMOD(2,K) + (0.0, 1.0) * VSTMOD(3,K)) /
     *            VSTMOD(1,K)
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
            ELSE
C                                       Source of unknown polarization
C                                       Derivatives of obs wrt
C                                       parameters C(1) = d(RL)/d(DRa):
               C(1) = ZZA
C                                       C(2) = d(RL)/d(DLb*)
               C(2) = ZZB
C                                       C(3) = d(RL)/d(Q+iU)
               C(3) = IPOLI
               I3 = 2 * N + L
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I1,I3) = AHA(I1,I3) + W * CONJG (C(1)) * C(3)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHA(I2,I3) = AHA(I2,I3) + W * CONJG (C(2)) * C(3)
               AHA(I3,I3) = AHA(I3,I3) + W * CONJG (C(3)) * C(3)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               AHB(I3) = AHB(I3) + W * CONJG (C(3)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
               END IF
            END IF
C                                       Contribution from LR
         IF (WT(3,K).GT.0.) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
            KOBS = KOBS + 1
C                                       Matrix indices
            I1 = N + I
            I2 = J
            I3 = 2 * N + L
            OBS = CONJG (VOBS(3,K) * IPOLI)
            W = WT(3,K) * IWT
            SWT = SWT + W
            SWT2 = SWT2 + W**2
            IF (L.EQ.0) THEN
C                                       Source of known polarization
C                                       Derivatives of obs wrt
C                                       parameters
C                                       C(1) = d(LR)/d(DLa*):
               C(1) = ZZA
C                                       C(2) = d(LR)/d(DRb)
               C(2) = ZZB
C                                       Residual
               OBS = OBS - (VSTMOD(2,K) + (0.0, 1.0) * VSTMOD(3,K)) /
     *            CONJG (VSTMOD(1,K))
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
            ELSE
C                                       Source of unknown polarization
C                                       Derivatives of obs wrt
C                                       parameters
C                                       C(1) = d(LR)/d(DLa*):
               C(1) = ZZA
C                                       C(2) = d(LR)/d(DRb)
               C(2) = ZZB
C                                       C(3) = d(LR)/d(Q+iU)
               C(3) = CONJG (IPOLI)
               I3 = 2 * N + L
               AHA(I1,I1) = AHA(I1,I1) + W * CONJG (C(1)) * C(1)
               AHA(I1,I2) = AHA(I1,I2) + W * CONJG (C(1)) * C(2)
               AHA(I1,I3) = AHA(I1,I3) + W * CONJG (C(1)) * C(3)
               AHA(I2,I2) = AHA(I2,I2) + W * CONJG (C(2)) * C(2)
               AHA(I2,I3) = AHA(I2,I3) + W * CONJG (C(2)) * C(3)
               AHA(I3,I3) = AHA(I3,I3) + W * CONJG (C(3)) * C(3)
               AHB(I1) = AHB(I1) + W * CONJG (C(1)) * OBS
               AHB(I2) = AHB(I2) + W * CONJG (C(2)) * OBS
               AHB(I3) = AHB(I3) + W * CONJG (C(3)) * OBS
               BHB = BHB + W * (REAL (OBS)**2 + AIMAG (OBS)**2)
               END IF
            END IF
 50      CONTINUE
C                                       find ref ant
      IF ((IREF.LE.0) .OR. (IREF.GT.N)) THEN
         IREF = 0
         I1 = 0
         DO 55 I = 1,MAXANT
            IF (COBS(I).GT.I1) THEN
               I1 = COBS(I)
               IREF = I
               END IF
 55         CONTINUE
         WRITE (MSGTXT,1055) IREF
         CALL MSGWRT (4)
         END IF
C                                       Find the average size of the
C                                       nonzero diagonal elements of
C                                       AHA:
      K = 0
      S = 0.0D0
      DO 100 I = 1,NVAR
         IF (REAL (AHA(I,I)).GT.0.0) THEN
            K = K + 1
            S = S + REAL (AHA(I,I))
            END IF
 100     CONTINUE
      IF (K.GT.0) S = S / K
      IF (K.EQ.0) S = 1.0D0
C                                       For any missing antennas or
C                                       missing calibration sources,
C                                       set the corresponding diagonal
C                                       elements of the normal equations
C                                       matrix to the average of the
C                                       nonzero diagonal elements:
      KNVAR = NVAR
      DO 120 I = 1,NVAR
         IF (ABS (AHA(I,I)).EQ.0.0) THEN
            TEMP = S
            AHA(I,I) = CMPLX (TEMP, 0.0)
            KNVAR = KNVAR - 1
            END IF
 120     CONTINUE
C                                       Either constrain the solution
C                                       for the right-hand i.f. of the
C                                       iref'th antenna to zero, or
C                                       modify the normal equations
C                                       matrix so as to approximate the
C                                       least-squares solution of
C                                       minimal Euclidean norm (by
C                                       adding a small positive number
C                                       to the diagonal elements):
      IF (IREF.GE.1.AND.IREF.LE.N) THEN
         AHA(IREF,IREF) = AHA(IREF,IREF) + CMPLX (10.0 * S, 0.0D0)
      ELSE
         EPS = 1.0D-5
         DO 180 I = 1,NVAR
            AHA(I,I) = CMPLX((1.0D0 + EPS), 0.0D0) * AHA(I,I)
 180        CONTINUE
         END IF
C                                       Set up an extra column of AHA so
C                                       that the r.m.s. residual can
C                                       come as a by-product of the
C                                       Cholesky decomposition (see
C                                       p. 8-3 of the  LINPACK guide):
      DO 200 I = 1,NVAR
         AHA(I,NVAR+1) = AHB(I)
 200     CONTINUE
      AHA(NVAR+1,NVAR+1) = BHB
C                                       Get the Cholesky decomposition
C                                       of AHA via a LINPACK routine:
      JOB = 0
      NVAR1 = NVAR + 1
      CALL CCHDC (AHA, LDA, NVAR1, WORK, JPVT, JOB, INFO)
      IF (INFO.LT.NVAR) THEN
         WRITE (MSGTXT,1200) INFO, NVAR
 1200 FORMAT ('SOLUTION IS INDETERMINATE:',I5,' <',I5)
         CALL MSGWRT (8)
         IER = 1
         DO 220 I = 1,N
            D(1,I) = (0.0 ,0.0)
            D(2,I) = (0.0 ,0.0)
 220        CONTINUE
         GO TO 999
         END IF
C                                       (The weighted sum of squared
C                                       residuals = real(AHA(nvar+1,
C                                       nvar+1))**2) Get the solution,
C                                       via a LINPACK routine:
      CALL CPOSL (AHA, LDA, NVAR, AHB)
C                                       Calculate the standard error
C                                       estimates, via a LINPACK routine
C                                       (the normalized covariance
C                                       matrix then is given by
C                                       AHA * RMS**2).
C                                       When the least-squares solution
C                                       of minimal Euclidean norm is
C                                       computed (i.e., when iref=0)
C                                       standard error estimates for
C                                       the antenna feed parameters are
C                                       not calculated here (they're
C                                       set to 0), but error estimates
C                                       for the calibrator fluxes can be
C                                       calculated:
      IF ((KOBS.GT.KNVAR) .AND. (SWT.GT.0.)) THEN
C        RMS = ABS (REAL (AHA(NVAR+1,NVAR+1))) *
C    *      SQRT (REAL (KOBS)/(REAL (KOBS - KNVAR) * SWT * 2.0))
C        WRITE (MSGTXT,1220) RMS
         DOF = (SWT**2) / SWT2
         RMS = ABS (REAL (AHA(NVAR+1,NVAR+1))) *
     *      SQRT (1./( (DOF - REAL(KNVAR)) * SWT * 2.0))
         WRITE (MSGTXT,1220) RMS, DOF
         CALL MSGWRT (4)
         JOB = 1
         CALL CPODI (AHA, LDA, NVAR, DET, JOB)
         IF (IREF.EQ.0) THEN
            DO 300 I = 1,2*N
               AHA(I,I) = (0.0, 0.0)
 300           CONTINUE
         ELSE
            AHA(IREF,IREF) = (0.0, 0.0)
            END IF
         END IF
C                                       Now, print the results.
C                                       Then return:
      DO 320 I = 1,N
         D(1,I) = AHB(I)
         WRITE (MSGTXT,1300) I
         CALL MSGWRT (4)
         E = RMS * SQRT (REAL (AHA(I,I))*SWT)
         AMP = ABS (D(1,I))
         AMPERR = SQRT (2.0) * E
         IF (AMP .EQ. 0.0) THEN
            PHASE = 0.0
            PHASER = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(1,I)), REAL (D(1,I))) * RTD
            PHASER = E / ABS (D(1,I)) * RTD
            END IF
         WRITE (MSGTXT,1301) AMP, AMPERR, PHASE, PHASER
         CALL MSGWRT (4)
C                                       Other hand poln.
         D(2,I) = CONJG (AHB(N+I))
         E = RMS * SQRT (REAL (AHA(N+I,N+I))*SWT)
         AMP = ABS (D(2,I))
         AMPERR = SQRT (2.0) * E
         IF (AMP .EQ. 0.0) THEN
            PHASE = 0.0
            PHASER = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(2,I)), REAL (D(2,I))) * RTD
            PHASER = E / ABS (D(2,I)) * RTD
            END IF
         WRITE (MSGTXT,1302) AMP, AMPERR, PHASE, PHASER
         CALL MSGWRT (4)
 320     CONTINUE
      IF (NUCAL.GT.0) THEN
         DO 340 I = 1,NUCAL
C                                       Fitted polarization
            VST(2,I) = REAL (AHB(2*N+I))
            VST(3,I) = AIMAG (AHB(2*N+I))
            E = RMS * SQRT (REAL (AHA(2*N+I,2*N+I))*SWT)
            WRITE (MSGTXT,1320) I, CSNAME(I)
            CALL MSGWRT (4)
            WRITE (MSGTXT,1321) VST(2,I), VST(3,I), E, E
            CALL MSGWRT (4)
            AMP = SQRT (VST(2,I)*VST(2,I) + VST(3,I)*VST(3,I))
            AMPERR = SQRT (2.0) * E
            IF (AMP .GT. 0.0) THEN
               PHASE = 0.5 * ATAN2 (VST(3,I), VST(2,I)) * RTD
               PHASER = 0.5 * E / AMP * RTD
            ELSE
               PHASE = 0.0
               PHASER = 0.0
               END IF
            WRITE (MSGTXT,1322) AMP, AMPERR, PHASE, PHASER
            CALL MSGWRT (4)
 340        CONTINUE
         END IF
C                                       Compute and print residuals.
      IF (PRTLV.GE.2) THEN
         SUM2 = 0.0
         COUNT2 = 0.0
         WRITE (MSGTXT, 1499)
         CALL  MSGWRT (5)
         DO 500 K = 1,NOBS
C                                       Cal. no. 0=> known poln.
            L = ICAL(K)
C                                       1st antenna number
            I = IJS(1,K)
C                                       2nd antenna number
            J = IJS(2,K)
C                                       Complex numbers with phase equal
C                                       to twice the parallactic
C                                       angles.
            ZZA = CMPLX (COS (2.0*(CHI(1,K))), SIN (2.0*(CHI(1,K))))
            ZZB = CMPLX (COS (2.0*(CHI(2,K))), SIN (2.0*(CHI(2,K))))
C                                       Total intensity
            IF ((WT(1,K).GT.0.0) .OR. (WT(4,K).GT.0.0)) THEN
               POLI = (VOBS(1,K)*WT(1,K) + VOBS(4,K)*WT(4,K)) /
     *            (WT(1,K) + WT(4,K))
            ELSE
               GO TO 500
               END IF
C                                       inverse of POLI
            IPOLI = CMPLX(1.0, 0.0) / POLI
C                                       Contribution from RL
            RLR = 0.0
            RLI = 0.0
            IF (WT(2,K).GT.0.) THEN
               OBS = VOBS(2,K) * IPOLI
               T2 = CONJG (D(2,J)) * ZZB
               T3 = D(1,I) * ZZA
               IF (L.EQ.0) THEN
C                                        Source of known polarization:
                  T1 = (VSTMOD(2,K) + (0.0, 1.0) * VSTMOD(3,K)) /
     *               VSTMOD(1,K)
               ELSE
C                                       Source of unknown polarization:
                  T1 = IPOLI * (VST(2,L) + (0.0, 1.0) * VST(3,L))
                  END IF
               MODEL = T1 + T2 + T3
               RESID = OBS - MODEL
               RESID = RESID * POLI
               RLR = REAL (RESID)
               RLI = AIMAG (RESID)
               COUNT2 = COUNT2 + 1.0
               SUM2 = SUM2 + RLR*RLR + RLI*RLI
               END IF
C                                       Contribution from LR
            LRR = 0.0
            LRI = 0.0
            IF (WT(3,K).GT.0.) THEN
               OBS = CONJG (VOBS(3,K) * IPOLI)
               T2 = D(1,J) * ZZB
               T3 = CONJG (D(2,I)) * ZZA
               IF (L.EQ.0) THEN
C                                        Source of known polarization:
                  T1 = (VSTMOD(2,K) + (0.0, 1.0) * VSTMOD(3,K)) /
     *               CONJG (VSTMOD(1,K))
               ELSE
C                                        Source of unknown polarization:
                  T1 = CONJG (IPOLI) * (VST(2,L) + (0.0, 1.0) *
     *               VST(3,L))
                  END IF
               MODEL = T1 + T2 + T3
               RESID = OBS - MODEL
               RESID = CONJG (RESID * POLI)
               LRR = REAL (RESID)
               LRI = AIMAG (RESID)
               COUNT2 = COUNT2 + 1.0
               SUM2 = SUM2 + LRR*LRR + LRI*LRI
               END IF
C                                        Print
            IRLOR = IROUND (1000.0 * REAL (VOBS(2,K)))
            IRLOI = IROUND (1000.0 * AIMAG (VOBS(2,K)))
            ILROR = IROUND (1000.0 * REAL (VOBS(3,K)))
            ILROI = IROUND (1000.0 * AIMAG (VOBS(3,K)))
            IRLRR = IROUND (1000.0 * RLR)
            IRLRI = IROUND (1000.0 * RLI)
            ILRRR = IROUND (1000.0 * LRR)
            ILRRI = IROUND (1000.0 * LRI)
            ICHIA = IROUND (57.296 * CHI(1,K))
            ICHIB = IROUND (57.296 * CHI(2,K))
            WRITE (MSGTXT,1500) K, I, J, L, ICHIA, ICHIB, IRLOR, IRLOI,
     *         ILROR, ILROI, IRLRR, IRLRI, ILRRR, ILRRI
            CALL MSGWRT (5)
 500        CONTINUE
C                                       RMS residual
         IF (COUNT2.GT.2) THEN
            SUM2 = SQRT (SUM2 / COUNT2)
            WRITE (MSGTXT,1501) SUM2
            CALL MSGWRT (5)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TOTAL INTENSITIES MUST BE PROVIDED',I10,' OF',I10)
 1005 FORMAT ('POLARIZATION MODEL NOT PROVIDED SAMPLE',I10,' OF',I10)
 1055 FORMAT ('RPCALC: Setting reference antenna to',I3)
 1220 FORMAT ('RMS residual =',1PE12.3,' DOF =',0PF10.2)
 1300 FORMAT ('Interferometer Element',I4)
 1301 FORMAT ('  R: Amp = ',F8.5,'+/-',F8.5,' Phase(deg) = ',F7.2,
     *   '+/-',F7.2)
 1302 FORMAT ('  L: Amp = ',F8.5,'+/-',F8.5,' Phase(deg) = ',F7.2,
     *   '+/-',F7.2)
 1320 FORMAT ('Calibration source ',I3,3X,A)
 1321 FORMAT ('  Q+iU=(',F10.5,',',F10.5,') +/- (',F10.6,',',F10.6,
     *   ') Jy')
 1322 FORMAT ('  Pol. inten. =',F10.5,' +/-',F10.6,' Jy, angle =',
     *   F7.2,' +/-', F7.3,' deg')
 1499 FORMAT ('Obs A1 A2 C    ChiA ChiB   RL Obs     LR Obs     RL res',
     *  '     LR res')
 1500 FORMAT (I5, 3I3, I5,I4, 4(I6,I5))
 1501 FORMAT ('True RMS residual =',1PE12.3,' Jy')
      END
      SUBROUTINE IPCALC (NOBS, VOBS, WT, CHI, IJS, N, PHI, THETA, IFP,
     *   IFT, VSTMOD, NUCAL, VST, IFVST, ICAL, IREF, ERRVST, PD, IPD,
     *   PRTLV, MAXIT, RFCTOL, XCTOL, FIXMOD, NOERR, IERR)
C-----------------------------------------------------------------------
C   Subroutine to determine instrumental and source polarizations.
C   Uses general orientation/ellipticity model for instrumental model.
C   This routine assumes that the sources have constant polarization
C   angle and the same, but scaled, distribution as the total intensity.
C   Will currently handle 20 calibrators.
C   Fits model:
C     F = {RR[(cos t1 + sin t1) exp(-i(e1-a1))] *
C            [(cos t2 + sin t2) exp( i(e2-a2))]
C       +  RL[(cos t1 + sin t1) exp(-i(e1-a1))] *
C            [(cos t2 - sin t2) exp(-i(e2-a2))]   +
C          LR[(cos t1 - sin t1) exp( i(e1-a1))] *
C            [(cos t2 + sin t2) exp( i(e2-a2))]   +
C          LL[(cos t1 - sin t1) exp( i(e1-a1))] *
C            [(cos t2 - sin t2) exp(-i(e2-a2))]}
C        * g1 * conjg (g2)
C
C     where 1 and 2 represent the two ends of the interferometer and can
C                    be R or L,
C           t = feed ellipticity  (tL = tR + pi/2), (THETA)
C           e = feed orientation  (eL = eR + pi/2), (PHI)
C           a = parallactic angle,
C           gR = exp(-i(a - (eR - eRref))) (the effect of phase
C                    calibration),
C           gL = exp( i(a - (eL - eLref) + PD))
C           II = 0.5*(RR+LL), PD = R-L phase difference
C   Notes:
C      1) This model assumes that an individual feed produces two
C      orthogonal polarizations.
C      2) The effects of the phase calibration are included in the g
C      terms.  This formulation assumes that the parallactic angle
C      corrections were made before phase calibration and that the R-L
C      phase differences were all referred to the same antenna (IREF)
C      before phase calibration.
C      3) Model visibilities are computed assuming the polarized
C      visibilities are a scaled version of the total intensity vis.
C      4) The weights of the parallel hand data are reduced by a factor
C      of 10 to allow the cross hand residuals to dominate the result.
C
C   Inputs:
C     NOBS       I    The number of observations, an observation
C                     consists of four visibility measuments on one
C                     baseline (RR, RL, LR, LL if using circular feeds).
C     VOBS(4,*)  Cx   Observed visibilities (RR, RL, LR, LL)
C     WT(4,*)    R    Weights of the visibilities
C     CHI(2,*)   R    Parallactic angles of the feeds for each antenna
C                     used for the observed visibilities.  For
C                     equatorial mounts use 0.
C     IJS(2,*)   I    Antenna numbers of the observations.  IJS(1,n)
C                     should be the lower number, IJS(2,n) the upper.
C     N          I    Number of antennas, actually the antenna number
C                     of the highest numbered antenna.
C     IFP(2,N)   I    Flag for fixing PHI, 1 per feed per antenna
C                     0 = fit, 1 = fix. No other values allowed.
C     IFT(2,N)   I    Flag for fixing THETA, 1 per feed per antenna
C                     0 = fit, 1 = fix. No other values allowed.
C     VSTMOD(4,*)Cx   Model polarizations per source (I,Q,U,V).
C                     Values only need be supplied to observations
C                     corresponding to sources which are NOT having
C                     point source polarization parameters fitted.
C     NUCAL      I    Number of unknown calibrator sources, i.e. the
C                     number of sources whose polarizations are to be
C                     determined.
C     IFVST(4,*) I    Flags for fixing source polarizations. (I,Q,U,V)
C                     0 => fit, 1 => fix.  NO other values are allowed.
C     ICAL(*)    I    Source numbers of the observations.  0 => the
C                     observations corresponds to a source of known
C                     polarization; otherwise a number between 1 and
C                     NUCAL.
C     IREF       I    Reference antenna number.
C     PD         R    "R-L" phase difference, needs source of known poln.
C     IPD        I    fit/fix flag for PD
C     PRTLV      I    Print level 0=>none, 1=some, 2=lots.
C     RFCTOL     R    Convergence tolerence, 0=> use default.
C     XCTOL      R    ???  use 0.0 for default.
C     FIXMOD     L    If true then use the model given in VST.
C   Input / output:
C     PHI(2,*)   R    Feed orientation, 2 feeds per antenna;
C                     on input the initial guess, on output the solution
C     THETA(2,*) R    Feed ellipticity, 2 feeds per antenna;
C                     on input the initial guess, on output the solution
C     VST(4,*)   R    Pt. source polarized flux densities (I,Q,U,V). On
C                     input, the initial guess, on output the fitted
C                     values.
C   Output:
C     ERRVST(4,*)R    Flux errors (Jy).
C     IERR       I    Return error code, 0=>OK, 1=> no data 2,3 inputs
C   NOTE: currently will handle only 20 calibrators
C-----------------------------------------------------------------------
      INCLUDE 'PPCAL.INC'
      INTEGER   NOBS, IJS(2,*), N, IFP(2,*), IFT(2,*), NUCAL,
     *   IFVST(4,*), ICAL(*), IREF, IPD, PRTLV, MAXIT, IERR
      COMPLEX VOBS(4,*), VSTMOD(4,*)
      REAL   WT(4,*), CHI(2,*), PHI(2,*), THETA(2,*), VST(4,*),
     *   ERRVST(4,*), PD, RFCTOL, XCTOL, NOERR
      LOGICAL   FIXMOD
C
      INTEGER   NCMAX, NVMAX, LV, LIV, LLIV, LLV
C                                       Max number of sources
      PARAMETER (NCMAX = MAXCAL)
      PARAMETER (NVMAX = 4*(MAXANT+NCMAX)+1)
      PARAMETER (LV = 71 + NVMAX * (NVMAX+13)  /  2)
      PARAMETER (LIV = 60)
      INTEGER   IV(LIV), NVAR, IFX(NVMAX), IAP(MAXANT), I, J, K, L, NFP,
     *   COBS(MAXANT)
      REAL      ERRP(2,MAXANT), ERRT(2,MAXANT), ERRPD, SWT, ISWT, SWT2,
     *   DOF, RMSRES, RNOBS, P, T, EP, ET
      DOUBLE PRECISION G(NVMAX), V(LV), FOPTT, X(NVMAX), ERR(NVMAX),
     *   FOPT, DET(2)
      COMPLEX   POLI
      INCLUDE 'INCS:DMSG.INC'
      DATA LLIV, LLV /LIV,LV/
C-----------------------------------------------------------------------
      IERR = 0
      NVAR = 4 * (N+NUCAL)+1
      NFP = 0
C                                       Check number of calibrators.
      IF (NUCAL .GT. NCMAX) THEN
         WRITE (MSGTXT,1000) NUCAL, NCMAX
         IERR = 3
         GO TO 990
         END IF
C                                       Initialize model parameters
C                                       Feed parameters
      DO 20 I = 1,N
         X(I) = PHI(1,I)
         X(N+I) = PHI(2,I)
         X(2*N+I) = THETA(1,I)
         X(3*N+I) = THETA(2,I)
         IFX(I) = IFP(1,I)
         IFX(N+I) = IFP(2,I)
         IFX(2*N+I) = IFT(1,I)
         IFX(3*N+I) = IFT(2,I)
         NFP = NFP+IFP(1,I)+IFP(2,I)+IFT(1,I)+IFT(2,I)
 20      CONTINUE
C                                       Source (fractional) polarization
      IF (NUCAL.GT.0) THEN
         DO 40 I = 1,NUCAL
C                                        Must give IPOL
            IF (VST(1,I).LE.1.0E-20) THEN
               MSGTXT = 'MUST PROVIDE TOTAL INTENSITIES'
               IERR = 2
               GO TO 990
               END IF
C                                       Don't fit for IPOL flux
            IFVST(1,I) = 1
            DO 30 J = 1,4
               X(4*(N+I-1)+J) = VST(J,I) / VST(1,I)
               IFX(4*(N+I-1)+J) = IFVST(J,I)
               NFP = NFP+IFVST(J,I)
 30            CONTINUE
 40         CONTINUE
         END IF
C                                       "R-L" phase difference
      X(NVAR) = PD
      IFX(NVAR) = IPD
      NFP = NFP+IPD
C                                       Normalize weights
      SWT = 0.0
      CALL FILL (MAXANT, 0, COBS)
      DO 50 K = 1,NOBS
         I = IJS(1,K)
         J = IJS(2,K)
         IF (WT(2,K).GT.0.) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
            END IF
         IF (WT(3,K).GT.0.) THEN
            COBS(I) = COBS(I) + 1
            COBS(J) = COBS(J) + 1
            END IF
C                                       Reduce weights of parallel hand
C                                       data.
         WT(1,K) = WT(1,K) * 0.1
         WT(4,K) = WT(4,K) * 0.1
         SWT = SWT + MAX (0.0, WT(1,K))
         SWT = SWT + MAX (0.0, WT(2,K))
         SWT = SWT + MAX (0.0, WT(3,K))
         SWT = SWT + MAX (0.0, WT(4,K))
C                                        Change sign for parallactic angle
         CHI(1,K) = -CHI(1,K)
         CHI(2,K) = -CHI(2,K)
 50      CONTINUE
      IF (SWT.LE.1.0E-20) THEN
         MSGTXT = 'IPCALC: NO VALID DATA'
         CALL MSGWRT (6)
         DO 51 I = 1,NUCAL
            VST(2,I) = 0.0
            VST(3,I) = 0.0
            VST(4,I) = 0.0
 51         CONTINUE
         I = 2 * N
         CALL RFILL (I, 0.0, PHI)
         CALL RFILL (I, 0.0, THETA)
         CALL RFILL (I, 0.0, ERRP)
         CALL RFILL (I, 0.0, ERRT)
         GO TO 999
         END IF
      IF ((IREF.LE.0) .OR. (IREF.GT.N)) THEN
         J = 0
         DO 55 I = 1,N
            IF (COBS(I).GT.J) THEN
               J = COBS(I)
               IREF = I
               END IF
 55         CONTINUE
         WRITE (MSGTXT,1055) IREF
         CALL MSGWRT (4)
         END IF
      ISWT = 4.0 * NOBS / SWT
      DO 60 K = 1,NOBS
         WT(1,K) = WT(1,K) * ISWT
         WT(2,K) = WT(2,K) * ISWT
         WT(3,K) = WT(3,K) * ISWT
         WT(4,K) = WT(4,K) * ISWT
 60      CONTINUE
C                                        if FIXMOD set model
C                                        visibilities.
      IF (FIXMOD) THEN
         DO 70 K = 1,NOBS
            L = ICAL(K)
C                                       Get observed Ipol for
C                                       normalization.
            IF ((WT(1,K)+WT(4,K)) .GT. 0.0) THEN
               POLI = (WT(1,K) * VOBS(1,K) + WT(4,K) * VOBS(4,K)) /
     *            (WT(1,K)+WT(4,K))
            ELSE
C                                       Must have I visibility.
               POLI = CMPLX (1.0, 0.0)
               WT(2,K) = 0.0
               WT(3,K) = 0.0
               END IF
C                                       Compute model
            VSTMOD(1,K) = POLI
C                                       Poln model is fractional values;
C                                       normalize with observed IPOL.
            VSTMOD(2,K) = (VST(2,L)/VST(1,L)) * POLI
            VSTMOD(3,K) = (VST(3,L)/VST(1,L)) * POLI
            VSTMOD(4,K) = (VST(4,L)/VST(1,L)) * POLI
            ICAL(K) = 0
 70         CONTINUE
         END IF
C                                       Prefit residuals, stats
      MSGTXT = 'Prefit statistics'
      CALL MSGWRT (4)
      CALL PRTRES (X, FOPT, NOBS, VOBS, WT, CHI, IJS, N, VSTMOD, NUCAL,
     *   ICAL, IREF, PRTLV, NFP)
C                                       Initialize errors
      DO 80 I = 1,NVAR
         ERR(I) = 1.0D0
 80      CONTINUE
C                                       Initialize fitting control
C                                       parameters IV and V.
C                                       DEFLT is part of the
C                                       minimization package (ACM
C                                       Algorithm 611). It sets
C                                       default values for the
C                                       parameters used by the main
C                                       minimization routine, SUMIT.
      CALL DEFLT (2, IV, LLIV, LLV, V)
C                                       Use non default values for some
C                                       parameters.
      IV(1) = 12
      IF (MAXIT.GE.0) THEN
         IV(17) = MAXIT * 2
         IV(18) = MAXIT
      ELSE
         IV(17) = 400
         IV(18) = 250
         END IF
      IV(21) = 0
      IF (PRTLV.GT.1) IV(21) = 5
      IV(22) = 0
      IV(24) = 0
C                                       Convergence criteria
      V(31) = 1.0D-4
      IF (RFCTOL.GT.0.) THEN
         V(32) = RFCTOL
      ELSE
         V(32) = 1.0D-4
         END IF
      IF (XCTOL.GT.0.) THEN
         V(33) = XCTOL
      ELSE
         V(33) = 1.0D-4
         END IF
C                                       SUMIT is the main minimization
C                                       routine.  Calling it with
C                                       iv(1)=12 tells it to go to work.
C                                       When it returns, it still is not
C                                       done unless it has set iv(1) to
C                                       some number other than 1 or 2.
C                                       When it returns with iv(1)=1, it
C                                       wants the objective function,
C                                       FOPT, to be calculated.  When it
C                                       returns with iv(1)=2, it wants
C                                       the gradient, G, of FOPT, to be
C                                       calculated.  Each time it asks
C                                       for the gradient set to 0 those
C                                       elements of g that correspond to
C                                       parameters which are to be held
C                                       fixed.
 100     CONTINUE
C                                       Minimization step
         CALL SUMIT (ERR, FOPT, G, IV, LLIV, LLV, NVAR, V, X)
C                                       Accumulate partials, etc.
         IF (IV(1).EQ.1) THEN
            CALL FX (X, FOPT, NOBS, VOBS, WT, CHI, IJS, N, VSTMOD,
     *         NUCAL, ICAL, IREF)
         ELSE IF (IV(1).EQ.2) THEN
            CALL FG (X, FOPTT, G, NOBS, VOBS, WT, CHI, IJS, N, VSTMOD,
     *         NUCAL, ICAL, IREF, IPD)
            IF (NFP.GT.0) THEN
C                                       Zero partials for fixed parms
               DO 120 I = 1,NVAR
                  IF (IFX(I).EQ.1) G(I) = 0.0D0
 120              CONTINUE
               END IF
         END IF
      IF ((IV(1).EQ.12) .OR. (IV(1).EQ.1) .OR. (IV(1).EQ.2)) GO TO 100
C                                       Postfit residuals, stats
      MSGTXT = 'Postfit statistics'
      CALL MSGWRT (4)
      CALL PRTRES (X, FOPT, NOBS, VOBS, WT, CHI, IJS, N, VSTMOD, NUCAL,
     *   ICAL, IREF, PRTLV, NFP)
C                                       Now SUMIT is finished.  FOPTT is
C                                       the value of the weighted sum of
C                                       squared moduli of residuals at
C                                       the (approximate) minimum found
C                                       by SUMIT.  SUMIT has left the
C                                       Cholesky factor of its estimate
C                                       of the Hessian matrix H
C                                       (evaluated at the approximate
C                                       minimum of FOPT) in the work
C                                       array V, starting at location
C                                       V(IV(42)).
C                                       The subroutine DPPDI of LINPACK
C                                       is used to find the matrix
C                                       inverse and determinant.
      RNOBS = 0.0
      SWT = 0.0
      SWT2 = 0.0
      CALL FILL (N, 0, IAP)
      DO 160 K = 1,NOBS
         DO 140 L = 1,4
            IF (WT(L,K) .GT. 0.0) THEN
               SWT = SWT + WT(L,K)
               SWT2 = SWT2 + WT(L,K) * WT(L,K)
               RNOBS = RNOBS + 2.0
               IAP(IJS(1,K)) = 1
               IAP(IJS(2,K)) = 1
               END IF
 140           CONTINUE
 160        CONTINUE
C                                       IAP tells which antennas have
C                                       data.  Now, to properly
C                                       normalize the standard error
C                                       estimates, NFP possibly has to
C                                       be increased, because the
C                                       calling program may not have
C                                       set the corresponding
C                                       elements of IFP and IFT.
      DO 200 I = 1,N
         IF (IAP(I).EQ.0) THEN
            DO 180 J = 1,2
               IF (IFP(J,I).EQ.0) THEN
                  NFP = NFP + 1
                  IFX((J-1)*N+I) = 1
                  END IF
               IF (IFT(J,I).EQ.0) THEN
                  NFP = NFP + 1
                  IFX((J+1)*N+I) = 1
                  END IF
 180           CONTINUE
            END IF
 200     CONTINUE
C                                       Error determination
C  was RMSRES = SQRT (FOPT / (RNOBS-NVAR+NFP) * RNOBS / (2.0*SWT))
      DOF = SWT**2 / SWT2
      RMSRES = SQRT (FOPT / (DOF-NVAR+NFP) / (2.0*SWT))
      WRITE (MSGTXT,1001) RMSRES, DOF
      IF (PRTLV.GT.0) CALL MSGWRT (4)
C***??? The error analysis doesn't work very well.
C***??? Also DPPI sometimes goes into infinite loops
      DET(1) = 0.0D0
      DET(2) = 0.0D0
      IF (NOERR.LE.0.0) CALL DPPDI (V(IV(42)), NVAR, DET, 11)
      K = 0
C                                       Compute errors
      DO 220 I = 1,NVAR
         K = K + 1
C                                       V(IV(42)+K-1) < 0 was => 0
         IF ((IFX(I).EQ.1) .OR. (DET(1).LE.0.0D0)) THEN
            ERR(I) = 0.0
         ELSE
            ERR(I) = SQRT (ABS(V(IV(42)+K-1)*SWT)) * RMSRES
            END IF
 220     CONTINUE
C                                       Now the subroutine prints out
C                                       the parameter estimates and the
C                                       standard error estimates.
      DO 260 I = 1,N
         PHI(1,I) = X(I)
         PHI(2,I) = X(N+I)
         THETA(1,I) = X(2*N+I)
         THETA(2,I) = X(3*N+I)
         ERRP(1,I) = ERR(I)
         ERRP(2,I) = ERR(N+I)
         ERRT(1,I) = ERR(2*N+I)
         ERRT(2,I) = ERR(3*N+I)
C                                       Print results if requested
         IF (PRTLV.GT.0) THEN
            WRITE (MSGTXT,1003) I
            CALL MSGWRT (4)
            WRITE (MSGTXT,1004)
            CALL MSGWRT (4)
            DO 240 J = 1,2
               P = PHI(J,I) * 57.29578
               T = THETA(J,I) * 57.29578
               EP = ERRP(J,I) * 57.29578
               ET = ERRT(J,I) * 57.29578
               WRITE (MSGTXT,1005) P, EP, T, ET
               CALL MSGWRT (4)
 240           CONTINUE
            END IF
 260     CONTINUE
C                                       Source parameters
      IF (NUCAL.GT.0) THEN
         DO 300 I = 1,NUCAL
            IF (PRTLV.GT.0) THEN
               WRITE (MSGTXT,1006) I
               CALL MSGWRT (4)
               END IF
C                                       Renormalize fitted fluxes
            DO 280 J = 1,4
               IF (J.GT.1) VST(J,I) = X(4*(N+I-1)+J) * VST(1,I)
               ERRVST(J,I) = ERR(4*(N+I-1)+J)
 280           CONTINUE
            IF (PRTLV.GT.0) THEN
               WRITE (MSGTXT,1007) VST(1,I), VST(2,I), VST(3,I),
     *            VST(4,I)
               CALL MSGWRT (4)
               WRITE (MSGTXT,1008) ERRVST(1,I), ERRVST(2,I),
     *            ERRVST(3,I), ERRVST(4,I)
               CALL MSGWRT (4)
               END IF
 300        CONTINUE
         END IF
C                                       "R-L" phase difference
      PD = X(NVAR)
      ERRPD = ERR(NVAR)
      IF (PRTLV.GT.0.) THEN
         WRITE (MSGTXT,1009) PD*57.29578
         CALL MSGWRT (4)
         WRITE (MSGTXT,1010) ERRPD*57.29578
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TOO MANY SOURCES TO SOLVE FOR: ',I3,' > MAX=',I3)
 1001 FORMAT ('RMS residual = ',1PE12.5,'  DOF =',0PF10.2)
 1003 FORMAT ('Interferometer element ',I3)
 1004 FORMAT ('  Orientation (degrees)   Ellipticity (degrees)')
 1005 FORMAT (4X,2F8.2, 10X,2F8.2)
 1006 FORMAT ('Calibration source number ',I3)
 1007 FORMAT ('I,Q,U,V =',4F11.4)
 1008 FORMAT ('         ',4F11.4)
 1009 FORMAT('Opposite polarization phase diff. = ',F10.2)
 1010 FORMAT('                                    ',F10.2)
 1055 FORMAT ('IPCALC: Setting reference antenna to',I3)
      END
      SUBROUTINE FG (X, S, G, NOBS, VOBS, WT, CHI, IJS, N, VSTMOD,
     *   NUCAL, ICAL, IREF, IPD)
C-----------------------------------------------------------------------
C   Routine to compute Chi squares of residuals and the gradient of
C   chi_squares wrt the parameters.
C    Inputs:
C     X(*)       D    Parameter array
C     NOBS       I    The number of observations
C     VOBS(4,*)  Cx   Observed visibilities (RR, RL, LR, LL)
C     WT(4,*)    R    Weights of the visibilities
C     CHI(2,*)   R    Parallactic angles of the feeds for each antenna.
C     IJS(2,*)   I    Antenna numbers
C     N          I    Number of antennas
C     VSTMOD(4,*)Cx   Model polarizations per observed visibility
C                     (I,Q,U,V)
C     NUCAL      I    Number of unknown calibrator sources
C     ICAL(*)    I    Source numbers of the observations.
C     IREF       I    Reference antenna number.
C     IPD        I    fit/fix flag for PD
C   Output:
C     CHISQ      D    Chi squares sum
C     G(*)       D    Array of partial derivatives of CHISQ wrt X.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION X(*), S, G(*)
      INTEGER   NOBS, IJS(2,*), N, NUCAL, ICAL(*), IREF, IPD
      COMPLEX VOBS(4,*), VSTMOD(4,*)
      REAL   WT(4,*), CHI(2,*)
C
      INTEGER   I, J, K, L, M, NVAR, LOOP
      DOUBLE PRECISION PHIW(2,MAXANT), THETAW(2,MAXANT), PD, SR(MAXANT),
     *   DR(MAXANT), SL(MAXANT), DL(MAXANT), ROOT2, SWT
       COMPLEX V11, V12, V21, V22, RES11, RES12, RES21, RES22,
     *   VC(4), POLI, PA1, PA2, PR(MAXANT), PL(MAXANT), RS(MAXANT),
     *   RD(MAXANT), LS(MAXANT), LD(MAXANT), PPSTAR, PRREF, PLREF, PPD,
     *   RRC, RLC, LRC, LLC , DFDP(2), DFDT(2), DFDST(4), DFDPD, CPA1,
     *   CPA2, DFDRRE, DFDLRE
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NVAR = 4*(N+NUCAL)+1
      ROOT2 = 1.0 / SQRT (2.0)
C                                       Extract parameters and
C                                       precompute some values.
      DO 20 I = 1,N
         PHIW(1,I) = X(I)
         PHIW(2,I) = X(N+I)
         THETAW(1,I) = X(2*N+I)
         THETAW(2,I) = X(3*N+I)
C                                        After some trig manipulations
C                                        factorize the model into
C                                        antenna based factors.
         SR(I) = COS (THETAW(1,I)) + SIN (THETAW(1,I))
         DR(I) = COS (THETAW(1,I)) - SIN (THETAW(1,I))
         SL(I) = COS (THETAW(2,I)) + SIN (THETAW(2,I))
         DL(I) = COS (THETAW(2,I)) - SIN (THETAW(2,I))
         PR(I) = CMPLX (COS (2.0*PHIW(1,I)),  SIN (2.0*PHIW(1,I)))
         PL(I) = CMPLX (COS (2.0*PHIW(2,I)), -SIN (2.0*PHIW(2,I)))
         RS(I) = CMPLX (ROOT2*SR(I), 0.0D0)
         RD(I) = CMPLX (ROOT2*DR(I), 0.0D0) * PR(I)
         LS(I) = CMPLX (ROOT2*SL(I), 0.0D0) * PL(I)
         LD(I) = CMPLX (ROOT2*DL(I), 0.0D0)
 20      CONTINUE
C                                       R-L phase difference
      PD = X(NVAR)
C                                        Reference antenna phi.
      PRREF = CMPLX (COS (PHIW(1,IREF)),  SIN (PHIW(1,IREF)))
      PLREF = CMPLX (COS (PHIW(2,IREF)), -SIN (PHIW(2,IREF)))
      PPD = CMPLX (COS (PD),  -SIN (PD))
      S = 0.0D0
C                                        Init gradient sum
      DO 60 I = 1,NVAR
         G(I) = 0.0D0
 60      CONTINUE
C                                       Loop over data
      SWT = 0.0
      DO 200 K = 1,NOBS
C                                        Sum of weights
         SWT = SWT + WT(1,K) + WT(2,K) + WT(3,K) + WT(4,K)
         I = IJS(1,K)
         J = IJS(2,K)
C                                       Set source model from current
C                                       parameters.
         L = ICAL(K)
         IF (L.GT.0) THEN
C                                       Get observed Ipol for
C                                       normalization.
            IF ((WT(1,K)+WT(4,K)) .GT. 0.0) THEN
               POLI = (WT(1,K) * VOBS(1,K) + WT(4,K) * VOBS(4,K)) /
     *            (WT(1,K)+WT(4,K))
            ELSE
C                                       Must have I visibility.
               POLI = CMPLX (1.0, 0.0)
               WT(2,K) = 0.0
               WT(3,K) = 0.0
               END IF
C                                       Compute model
            VSTMOD(1,K) = POLI
            DO 40 LOOP = 2,4
C                                       Poln model is fractional values;
C                                       normalize with observed IPOL.
               VSTMOD(LOOP,K) = X(4*(N+L-1)+LOOP) * POLI
 40            CONTINUE
            ELSE
C                                       Get POLI from model provided
               POLI = VSTMOD(1,K)
            END IF
C                                       Parallactic angle factors
         PA1 = CMPLX (COS (2.0*CHI(1,K)), -SIN (2.0*CHI(1,K)))
         PA2 = CMPLX (COS (2.0*CHI(2,K)), -SIN (2.0*CHI(2,K)))
         CPA1 = CONJG (PA1)
         CPA2 = CONJG (PA2)
C                                       Compute model as correlator
C                                       values.(RR,RL,LR,LL)
         VC(1) = (VSTMOD(1,K) + VSTMOD(4,K))
         VC(2) = (VSTMOD(2,K) + CMPLX (0.0, 1.0)*VSTMOD(3,K))
         VC(3) = (VSTMOD(2,K) - CMPLX (0.0, 1.0)*VSTMOD(3,K))
         VC(4) = (VSTMOD(1,K) - VSTMOD(4,K))
C                                       RR
         IF (WT(1,K).GT.0.) THEN
C                                       Compute model
            RRC = RS(I) * CONJG (RS(J))
            RLC = RS(I) * CONJG (RD(J)) * CPA2
            LRC = RD(I) * CONJG (RS(J)) * PA1
            LLC = RD(I) * CONJG (RD(J)) * PA1 * CPA2
            V11 = VC(1) * RRC + VC(2) * RLC + VC(3) * LRC + VC(4) * LLC
C                                       Residual
            RES11 = V11 - VOBS(1,K)
C                                       Sum chi squares
            S = S+WT(1,K)*(REAL (RES11)**2+AIMAG (RES11)**2)
C                                       Compute contributions to
C                                       gradient
C                                       partial of model wrt phi1
            DFDP(1) = CMPLX (0.0D0, 2.0*ROOT2) * DR(I) * PA1 *
     *         (VC(4) * CONJG (RD(J)) * CPA2 + VC(3) * CONJG (RS(J)))
C                                       partial of model wrt phi2
            DFDP(2) = -CMPLX (0.0D0, 2.0*ROOT2) * DR(J) * CPA2 *
     *         (VC(4) * RD(I) * PA1 + VC(2) * RS(I))
C                                       partial of model wrt theta1
            DFDT(1) = ROOT2 *
     *         (DR(I) * (VC(1) * CONJG (RS(J)) +
     *                   VC(2) * CONJG (RD(J)) * CPA2)
     *       - SR(I) * PR(I) * PA1 * (VC(3) * CONJG (RS(J)) +
     *                                VC(4) * CONJG (RD(J)) * CPA2))
C                                       partial of model wrt theta2
            DFDT(2) = ROOT2 *
     *         (DR(J) * (VC(1) * RS(I) +
     *                   VC(3) * RD(I) * PA1)
     *       - SR(J) * CONJG (PR(J)) * CPA2 *
     *          (VC(2) * RS(I) + VC(4) * RD(I) * PA1))
C                                       Gradient of chi squares
C                                       components.
C                                       phi1
            G(I) = G(I)+WT(1,K)*
     *         (REAL (RES11)*REAL (DFDP(1))+AIMAG (RES11)*
     *         AIMAG (DFDP(1)))
C                                       phi2
            G(J) = G(J)+WT(1,K)*
     *         (REAL (RES11)*REAL (DFDP(2))+AIMAG (RES11)*
     *         AIMAG (DFDP(2)))
C                                       theta1
            G(2*N+I) = G(2*N+I)+WT(1,K)*
     *         (REAL (RES11)*REAL (DFDT(1))+AIMAG (RES11)*
     *         AIMAG (DFDT(1)))
C                                       theta2
            G(2*N+J) = G(2*N+J)+WT(1,K)*
     *         (REAL (RES11)*REAL (DFDT(2))+AIMAG (RES11)*
     *         AIMAG (DFDT(2)))
C                                       Source fractional Stokes
C                                       parameters.
            IF (L.GT.0) THEN
C                                        partial of model wrt Ipol
               DFDST(1) = POLI * (RRC + LLC)
C                                       partial of model wrt Qpol
               DFDST(2) = POLI * (RLC + LRC)
C                                       partial of model wrt Upol
               DFDST(3) = CMPLX (0.0, 1.0) * POLI * (RLC - LRC)
C                                       partial of model wrt Vpol
               DFDST(4) = POLI * (RRC - LLC)
               DO 100 M = 1,4
                  G(4*(N+L-1)+M) = G(4*(N+L-1)+M)+WT(1,K)
     *               * (REAL (RES11)*REAL (DFDST(M)) +
     *               AIMAG (RES11)*AIMAG (DFDST(M)))
 100              CONTINUE
               END IF
            END IF
C                                       RL
         IF (WT(2,K).GT.0.) THEN
C                                       Compute residual
            PPSTAR = PRREF * CONJG (PLREF * PPD)
            RRC = RS(I) * CONJG(LS(J)) * PA2
            RLC = RS(I) * CONJG(LD(J))
            LRC = RD(I) * CONJG(LS(J)) * PA1 * PA2
            LLC = RD(I) * CONJG(LD(J)) * PA1
            V12 = PPSTAR * (VC(1) * RRC + VC(2) * RLC + VC(3) * LRC +
     *         VC(4) * LLC)
            RES12 = V12 - VOBS(2,K)
C                                       Sum chi squares
            S = S + WT(2,K)*(REAL (RES12)**2+AIMAG (RES12)**2)
C                                       Compute contributions to
C                                       gradient
C                                       partial of model wrt phi1
            IF (IREF.EQ.I) THEN
               DFDP(1) = CMPLX (0.0D0, ROOT2) * CONJG (PLREF * PPD) *
     *            DR(I) * CONJG (PRREF) * PA1 *
     *            (VC(4) * LD(J) + VC(3) * CONJG (LS(J)) * PA2)
            ELSE
               DFDP(1) = CMPLX (0.0D0, 2.0*ROOT2) * PPSTAR * PA1 *
     *            DR(I) * (VC(4) * LD(J) + VC(3) * CONJG (LS(J)) * PA2)
C               DFDRRE =  -CMPLX (0.0, 1.0) * CONJG (PLREF* PPD) *
C     *            (RRC + RLC + LRC + LLC)
               DFDRRE = CMPLX (0.0, 1.0) * V12
               G(IREF) = G(IREF)+WT(2,K)*
     *            (REAL (RES12)*REAL (DFDRRE)+AIMAG (RES12)*
     *            AIMAG (DFDRRE))
               END IF
C                                       partial of model wrt phi2
            IF (IREF.EQ.J) THEN
               DFDP(2) = CMPLX (0.0D0, ROOT2) * PRREF * CONJG (PPD) *
     *            SL(J) * PLREF * PA2 *
     *            (VC(1) * RS(I) + VC(3) * RD(I) * PA1)
            ELSE
               DFDP(2) = CMPLX (0.0D0, 2.0*ROOT2) * PPSTAR * PA2 *
     *            SL(J) * (VC(1) * RS(I) + VC(3) * RD(I) * PA1)
C               DFDLRE =  -CMPLX (0.0, 1.0) * PRREF * CONJG (PPD) *
C     *            (RRC + RLC + LRC + LLC)
               DFDLRE =  CMPLX (0.0, 1.0) * V12
               G(N+IREF) = G(N+IREF)+WT(2,K)*
     *            (REAL (RES12)*REAL (DFDLRE)+AIMAG (RES12)*
     *            AIMAG (DFDLRE))
               END IF
C                                       partial of model wrt theta1
            DFDT(1) = ROOT2 * PPSTAR *
     *         (DR(I) * (VC(1) * CONJG (LS(J)) * PA2 +
     *                   VC(2) * CONJG (LD(J)))
     *       - SR(I) * PR(I) * PA1 * (VC(3) * CONJG (LS(J)) * PA2 +
     *                                VC(4) * CONJG (LD(J))))
C                                       partial of model wrt theta2
            DFDT(2) = ROOT2 * PPSTAR *
     *         (DL(J) * CONJG (PL(J)) * PA2 *
     *            (VC(1) * RS(I)+ VC(3) * RD(I) * PA1)
     *       - SL(J) * (VC(2) * RS(I) + VC(4) * RD(I) * PA1))
C                                       Gradient of chi squares
C                                       components.
C                                       phi1
            G(I) = G(I)+WT(2,K)*
     *         (REAL (RES12)*REAL (DFDP(1))+AIMAG (RES12)*
     *         AIMAG (DFDP(1)))
C                                       phi2
            G(N+J) = G(N+J)+WT(2,K)*
     *         (REAL (RES12)*REAL (DFDP(2))+AIMAG (RES12)*
     *         AIMAG (DFDP(2)))
C                                       theta1
            G(2*N+I) = G(2*N+I)+WT(2,K)*
     *         (REAL (RES12)*REAL (DFDT(1))+AIMAG (RES12)*
     *         AIMAG (DFDT(1)))
C                                       theta2
            G(3*N+J) = G(3*N+J)+WT(2,K)*
     *         (REAL (RES12)*REAL (DFDT(2))+AIMAG (RES12)*
     *         AIMAG (DFDT(2)))
C                                       Source fractional Stokes
C                                       parameters.
            IF (L.GT.0) THEN
C                                        partial of model wrt Ipol
               DFDST(1) = POLI * (RRC + LLC)
C                                       partial of model wrt Qpol
               DFDST(2) = POLI * (RLC + LRC)
C                                       partial of model wrt Upol
               DFDST(3) = CMPLX (0.0, 1.0) * POLI * (RLC - LRC)
C                                       partial of model wrt Vpol
               DFDST(4) = POLI * (RRC - LLC)
               DO 120 M = 1,4
                  G(4*(N+L-1)+M) = G(4*(N+L-1)+M)+WT(2,K)
     *               * (REAL (RES12)*REAL (DFDST(M)) +
     *               AIMAG (RES12)*AIMAG (DFDST(M)))
 120              CONTINUE
               END IF
C                                       Phase difference:
            IF (IPD.EQ.0) THEN
C                                       partial of model wrt PD
               DFDPD = CMPLX (0.0, 1.0) * V12
               G(NVAR) = G(NVAR)+WT(2,K)*(REAL (RES12)*REAL (DFDPD)+
     *            AIMAG (RES12)*AIMAG (DFDPD))
               END IF
            END IF
C                                       LR
         IF (WT(3,K).GT.0.) THEN
C                                       Compute residual
            PPSTAR = PLREF * CONJG (PRREF) * PPD
            RRC = LS(I) * CONJG(RS(J)) * CPA1
            RLC = LS(I) * CONJG(RD(J)) * CPA1 * CPA2
            LRC = LD(I) * CONJG(RS(J))
            LLC = LD(I) * CONJG(RD(J)) * CPA2
            V21 = PPSTAR * (VC(1) * RRC + VC(2) * RLC + VC(3) * LRC +
     *         VC(4) * LLC)
            RES21 = V21 - VOBS(3,K)
            S = S + WT(3,K)*(REAL (RES21)**2+AIMAG (RES21)**2)
C                                       Compute contributions to
C                                       gradient
C                                       partial of model wrt phi1
            IF (IREF.EQ.I) THEN
               DFDP(1) = -CMPLX (0.0D0, ROOT2) * PLREF * PPD *
     *            DR(J) * PRREF * CPA2 *
     *            (VC(4) * LD(I) + VC(2) * LS(I) * CPA1)
            ELSE
               DFDP(1) = -CMPLX (0.0D0, 2.0*ROOT2) * PPSTAR * CPA1 *
     *            SL(I) * (VC(1) * RS(J) + VC(2) * CONJG (RD(J)) * CPA2)
C               DFDLRE =  CMPLX (0.0, 1.0) * CONJG (PRREF) * PPD *
C     *            (RRC + RLC + LRC + LLC)
               DFDLRE =  -CMPLX (0.0, 1.0) * V21
               G(N+IREF) = G(N+IREF)+WT(3,K)*
     *            (REAL (RES21)*REAL (DFDLRE)+AIMAG (RES21)*
     *            AIMAG (DFDLRE))
               END IF
C                                       partial of model wrt phi2
            IF (IREF.EQ.J) THEN
               DFDP(2) = -CMPLX (0.0D0, ROOT2) * PRREF * PPD *
     *            SL(I) * CONJG (PRREF) * CPA1 *
     *            (VC(1) * RS(J) + VC(2) * CONJG (RD(J)) * CPA2)
            ELSE
               DFDP(2) = -CMPLX (0.0D0, 2.0*ROOT2) * PPSTAR * CPA2 *
     *            DR(J) * (VC(4) * LD(I) + VC(2) * LS(I) * CPA2)
C               DFDRRE =  CMPLX (0.0, 1.0) * PLREF * PPD *
C     *            (RRC + RLC + LRC + LLC)
               DFDRRE =  -CMPLX (0.0, 1.0) * V21
               G(IREF) = G(IREF)+WT(3,K)*
     *            (REAL (RES21)*REAL (DFDRRE)+AIMAG (RES21)*
     *            AIMAG (DFDRRE))
               END IF
C                                       partial of model wrt theta1
            DFDT(1) = ROOT2 * PPSTAR *
     *         (DL(I) * PL(I) * CPA1 *
     *            (VC(1) * CONJG (RS(J)) +
     *             VC(2) * CONJG (RD(J)) * CPA2)
     *       - SL(I) * (VC(3) * CONJG (RS(J)) +
     *                  VC(4) * CONJG (RD(J)) * CPA2))
C                                       partial of model wrt theta2
            DFDT(2) = ROOT2 * PPSTAR *
     *         (DR(J) * (VC(1) * LS(I) * CPA1 + VC(3) * LD(I))
     *        - SR(J) * CONJG (PR(J)) * CPA2 *
     *             (VC(2) * LS(I) * CPA1 + VC(4) * LD(I)))
C                                       Gradient of chi squares
C                                       components.
C                                       phi1
            G(N+I) = G(N+I)+WT(3,K)*
     *         (REAL (RES21)*REAL (DFDP(1))+AIMAG (RES21)*
     *         AIMAG (DFDP(1)))
C                                       phi2
            G(J) = G(J)+WT(3,K)*
     *         (REAL (RES21)*REAL (DFDP(2))+AIMAG (RES21)*
     *         AIMAG (DFDP(2)))
C                                       theta1
            G(3*N+I) = G(3*N+I)+WT(3,K)*
     *         (REAL (RES21)*REAL (DFDT(1))+AIMAG (RES21)*
     *         AIMAG (DFDT(1)))
C                                       theta2
            G(2*N+J) = G(2*N+J)+WT(3,K)*
     *         (REAL (RES21)*REAL (DFDT(2))+AIMAG (RES21)*
     *       AIMAG (DFDT(2)))
C                                       Source fractional Stokes
C                                       parameters.
            IF (L.GT.0) THEN
C                                        partial of model wrt Ipol
               DFDST(1) = POLI * (RRC + LLC)
C                                       partial of model wrt Qpol
               DFDST(2) = POLI * (RLC + LRC)
C                                       partial of model wrt Upol
               DFDST(3) = CMPLX (0.0, 1.0) * POLI * (RLC - LRC)
C                                       partial of model wrt Vpol
               DFDST(4) = POLI * (RRC - LLC)
               DO 140 M = 1,4
                  G(4*(N+L-1)+M) = G(4*(N+L-1)+M)+WT(3,K)
     *               * (REAL (RES21)*REAL (DFDST(M)) +
     *               AIMAG (RES21)*AIMAG (DFDST(M)))
 140              CONTINUE
               END IF
C                                       Phase difference:
            IF (IPD.EQ.0) THEN
C                                       partial of model wrt PD
               DFDPD =  -CMPLX (0.0, 1.0) * V21
               G(NVAR) = G(NVAR)+WT(3,K)*(REAL (RES21)*REAL (DFDPD)+
     *            AIMAG (RES21)*AIMAG (DFDPD))
               END IF
            END IF
C                                       LL
         IF (WT(4,K).GT.0.) THEN
C                                       Compute residual
            RRC = LS(I) * CONJG(LS(J)) * CPA1 * PA2
            RLC = LS(I) * CONJG(LD(J)) * CPA1
            LRC = LD(I) * CONJG(LS(J)) * PA2
            LLC = LD(I) * CONJG(LD(J))
            V22 = VC(1) * RRC + VC(2) * RLC + VC(3) * LRC + VC(4) * LLC
            RES22 = V22 - VOBS(4,K)
C                                       Sum chi squares
            S = S+WT(4,K)*(REAL (RES22)**2+AIMAG (RES22)**2)
C                                       Compute contributions to
C                                       gradient
C                                       partial of model wrt phi1
            DFDP(1) = -CMPLX (0.0D0, 2.0*ROOT2) * SL(I) * CPA1 *
     *         (VC(1) * CONJG (LS(J)) * PA2 + VC(2) * LD(J))
C                                       partial of model wrt phi2
            DFDP(2) = CMPLX (0.0D0, 2.0*ROOT2) * SL(J) * PA2 *
     *         (VC(1) * LS(I) * CPA1 + VC(3) * LD(I))
C                                       partial of model wrt theta1
            DFDT(1) = ROOT2 *
     *         (DL(I) * CPA1 * PL(I) *
     *            (VC(1) * CONJG (LS(J)) * PA2 +
     *             VC(2) * CONJG (LD(J)))
     *       - SL(I) * (VC(3) * CONJG (LS(J)) * PA2 +
     *                  VC(4) * CONJG (LD(J))))
C                                       partial of model wrt theta2
            DFDT(2) = ROOT2 *
     *         (DL(J) * CONJG (PL(J)) * PA2 *
     *            (VC(1) * LS(I) * CPA1 +
     *             VC(3) * LD(I))
     *       - SL(J) * (VC(2) * LS(I) * CPA1 +
     *                  VC(4) * LD(I)))
C                                       Gradient of chi squares
C                                       components.
C                                       phi1
            G(N+I) = G(N+I)+WT(4,K)*
     *         (REAL (RES22)*REAL (DFDP(1))+AIMAG (RES22)*
     *         AIMAG (DFDP(1)))
C                                       phi2
            G(N+J) = G(N+J)+WT(4,K)*
     *         (REAL (RES22)*REAL (DFDP(2))+AIMAG (RES22)*
     *         AIMAG (DFDP(2)))
C                                       theta1
            G(3*N+I) = G(3*N+I)+WT(4,K)*
     *         (REAL (RES22)*REAL (DFDT(1))+AIMAG (RES22)*
     *       AIMAG (DFDT(1)))
C                                       theta2
            G(3*N+J) = G(3*N+J)+WT(4,K)*
     *         (REAL (RES22)*REAL (DFDT(2))+AIMAG (RES22)*
     *         AIMAG (DFDT(2)))
C                                       Source fractional Stokes
C                                       parameters.
            IF (L.GT.0) THEN
C                                        partial of model wrt Ipol
               DFDST(1) = POLI * (RRC + LLC)
C                                       partial of model wrt Qpol
               DFDST(2) = POLI * (RLC + LRC)
C                                       partial of model wrt Upol
               DFDST(3) = CMPLX (0.0, 1.0) * POLI * (RLC - LRC)
C                                       partial of model wrt Vpol
               DFDST(4) = POLI * (RRC - LLC)
               DO 150 M = 1,4
                  G(4*(N+L-1)+M) = G(4*(N+L-1)+M)+WT(4,K)
     *               * (REAL (RES22)*REAL (DFDST(M)) +
     *               AIMAG (RES22)*AIMAG (DFDST(M)))
 150              CONTINUE
               END IF
            END IF
 200     CONTINUE
C                                        Add constraint that
C                                        thetaR-thetaL=pi/2
C                                        Use weight 1/(10*nant)
C      WWT = SWT / (10.0*N)
C      DO 250 I = 1,N
C         RES = THETAW(1,I) - THETAW(2,I) - 1.570796327
CC                                        Chi squares
C         S = S + WWT * RES*RES
CC                                        Gradient
C         G(3*N+I) = G(3*N+I) + WWT * RES
C 250     CONTINUE
C                                        Multiply gradient by 2 for the
C                                        2 in chi squares.
      DO 300 I = 1,NVAR
         G(I) = 2.0E0*G(I)
 300     CONTINUE
C
      RETURN
      END
      SUBROUTINE FX (X, S, NOBS, VOBS, WT, CHI, IJS, N, VSTMOD,
     *   NUCAL, ICAL, IREF)
C-----------------------------------------------------------------------
C   Routine to compute Chi squares of residuals.
C    Inputs:
C     X(*)       D    Parameter array
C     NOBS       I    The number of observations
C     VOBS(4,*)  Cx   Observed visibilities (RR, RL, LR, LL)
C     WT(4,*)    R    Weights of the visibilities
C     CHI(2,*)   R    Parallactic angles of the feeds for each antenna.
C     IJS(2,*)   I    Antenna numbers
C     N          I    Number of antennas
C     VSTMOD(4,*)Cx   Model polarizations per observation (I,Q,U,V)
C     NUCAL      I    Number of unknown calibrator sources
C     ICAL(*)    I    Source numbers of the observations.
C     IREF       I    Reference antenna number.
C   Output:
C     S          D    Chi squares sum
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION X(*), S
      INTEGER   NOBS, IJS(2,*), N, NUCAL, ICAL(*), IREF
      REAL   WT(4,*), CHI(2,*)
      COMPLEX VOBS(4,*), VSTMOD(4,*)
C
      INTEGER   I, J, K, L, NVAR
      DOUBLE PRECISION PHIW(2,MAXANT), THETAW(2,MAXANT), PD, SR(MAXANT),
     *   DR(MAXANT), SL(MAXANT), DL(MAXANT), ROOT2, SWT
      COMPLEX  V11, V12, V21, V22, RES11, RES12, RES21, RES22,
     *   VC(4), POLI, PA1, PA2, PR, PL, RS(MAXANT), RD(MAXANT),
     *   LS(MAXANT), LD(MAXANT), PPSTAR, PRREF, PLREF, RRC, RLC, LRC,
     *   LLC, CPA1, CPA2
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NVAR = 4*(N+NUCAL)+1
      ROOT2 = 1.0 / SQRT (2.0)
C                                       Extract parameters and
C                                       precompute some values.
      DO 20 I = 1,N
         PHIW(1,I) = X(I)
         PHIW(2,I) = X(N+I)
         THETAW(1,I) = X(2*N+I)
         THETAW(2,I) = X(3*N+I)
C                                        After some trig manipulations
C                                        factorize the model into
C                                        antenna based factors.
         SR(I) = COS (THETAW(1,I)) + SIN (THETAW(1,I))
         DR(I) = COS (THETAW(1,I)) - SIN (THETAW(1,I))
         SL(I) = COS (THETAW(2,I)) + SIN (THETAW(2,I))
         DL(I) = COS (THETAW(2,I)) - SIN (THETAW(2,I))
         PR    = CMPLX (COS (2.0*PHIW(1,I)),  SIN (2.0*PHIW(1,I)))
         PL    = CMPLX (COS (2.0*PHIW(2,I)), -SIN (2.0*PHIW(2,I)))
         RS(I) = CMPLX (ROOT2*SR(I), 0.0D0)
         RD(I) = CMPLX (ROOT2*DR(I), 0.0D0) * PR
         LS(I) = CMPLX (ROOT2*SL(I), 0.0D0) * PL
         LD(I) = CMPLX (ROOT2*DL(I), 0.0D0)
 20      CONTINUE
C                                       R-L phase difference
      PD = X(NVAR)
      PRREF = CMPLX (COS (PHIW(1,IREF)), SIN (PHIW(1,IREF)))
      PLREF = CMPLX (COS (PHIW(2,IREF)+PD), -SIN (PHIW(2,IREF)+PD))
C                                       Set source model from current
C                                       parameters.
      IF (NUCAL.GT.0) THEN
         DO 50 K = 1,NOBS
            L = ICAL(K)
            IF (L.GT.0) THEN
C                                       Get observed Ipol for
C                                       normalization.
               IF ((WT(1,K)+WT(4,K)) .GT. 0.0) THEN
                  POLI = (WT(1,K) * VOBS(1,K) + WT(4,K) * VOBS(4,K)) /
     *               (WT(1,K)+WT(4,K))
               ELSE
C                                       Must have I visibility.
                  POLI = CMPLX (1.0, 0.0)
                  WT(2,K) = 0.0
                  WT(3,K) = 0.0
                  END IF
C                                       Compute model
               VSTMOD(1,K) = POLI
               DO 40 J = 2,4
C                                       Poln model is fractional values;
C                                       normalize with observed IPOL.
                  VSTMOD(J,K) = CMPLX (X(4*(N+L-1)+J), 0.0D0) * POLI
 40               CONTINUE
            END IF
 50         CONTINUE
      END IF
      S = 0.0D0
      SWT = 0.0
C                                       Loop over data
      DO 200 K = 1,NOBS
C                                        Sum of weights
         SWT = SWT + WT(1,K) + WT(2,K) + WT(3,K) + WT(4,K)
         I = IJS(1,K)
         J = IJS(2,K)
C                                       Parallactic angle factors
         PA1 = CMPLX (COS (2.0*CHI(1,K)), -SIN (2.0*CHI(1,K)))
         PA2 = CMPLX (COS (2.0*CHI(2,K)), -SIN (2.0*CHI(2,K)))
         CPA1 = CONJG (PA1)
         CPA2 = CONJG (PA2)
C                                       Compute model as correlator
C                                       values.
         VC(1) = (VSTMOD(1,K) + VSTMOD(4,K))
         VC(2) = (VSTMOD(2,K) + CMPLX (0.0, 1.0)*VSTMOD(3,K))
         VC(3) = (VSTMOD(2,K) - CMPLX (0.0, 1.0)*VSTMOD(3,K))
         VC(4) = (VSTMOD(1,K) - VSTMOD(4,K))
C                                       RR
         IF (WT(1,K).GT.0.) THEN
C                                       Compute model
            RRC = VC(1) * RS(I) * CONJG(RS(J))
            RLC = VC(2) * RS(I) * CONJG(RD(J)) * CPA2
            LRC = VC(3) * RD(I) * CONJG(RS(J)) * PA1
            LLC = VC(4) * RD(I) * CONJG(RD(J)) * PA1 * CPA2
            V11 = RRC + RLC + LRC + LLC
C                                       Residual
            RES11 = V11 - VOBS(1,K)
C                                       Sum chi squares
            S = S+WT(1,K)*(REAL (RES11)**2+AIMAG (RES11)**2)
            END IF
C                                       RL
         IF (WT(2,K).GT.0.) THEN
C                                       Compute residual
            PPSTAR = PRREF * CONJG (PLREF)
            RRC = PPSTAR * VC(1) * RS(I) * CONJG(LS(J)) * PA2
            RLC = PPSTAR * VC(2) * RS(I) * CONJG(LD(J))
            LRC = PPSTAR * VC(3) * RD(I) * CONJG(LS(J)) * PA1 * PA2
            LLC = PPSTAR * VC(4) * RD(I) * CONJG(LD(J)) * PA1
            V12 = RRC + RLC + LRC + LLC
            RES12 = V12 - VOBS(2,K)
C                                       Sum chi squares
            S = S + WT(2,K)*(REAL (RES12)**2+AIMAG (RES12)**2)
            END IF
C                                       LR
         IF (WT(3,K).GT.0.) THEN
C                                       Compute residual
            PPSTAR = PLREF * CONJG (PRREF)
            RRC = PPSTAR * VC(1) * LS(I) * CONJG(RS(J)) * CPA1
            RLC = PPSTAR * VC(2) * LS(I) * CONJG(RD(J)) * CPA1 *
     *         CPA2
            LRC = PPSTAR * VC(3) * LD(I) * CONJG(RS(J))
            LLC = PPSTAR * VC(4) * LD(I) * CONJG(RD(J)) * CPA2
            V21 = RRC + RLC + LRC + LLC
            RES21 = V21 - VOBS(3,K)
            S = S + WT(3,K)*(REAL (RES21)**2+AIMAG (RES21)**2)
            END IF
C                                       LL
         IF (WT(4,K).GT.0.) THEN
C                                       Compute residual
            RRC = VC(1) * LS(I) * CONJG(LS(J)) * CPA1 * PA2
            RLC = VC(2) * LS(I) * CONJG(LD(J)) * CPA1
            LRC = VC(3) * LD(I) * CONJG(LS(J)) * PA2
            LLC = VC(4) * LD(I) * CONJG(LD(J))
            V22 = RRC + RLC + LRC + LLC
            RES22 = V22 - VOBS(4,K)
C                                       Sum chi squares
            S = S+WT(4,K)*(REAL (RES22)**2+AIMAG (RES22)**2)
            END IF
 200     CONTINUE
C                                        Add constraint that
C                                        thetaR-thetaL=pi/2
C                                        Use weight 1/(10*nant)
C      WWT = SWT / (10.0*N)
C      DO 250 I = 1,N
C         RES = THETAW(1,I) - THETAW(2,I) - 1.570796327
CC                                        Chi squares
C         S = S + WWT * RES*RES
C 250     CONTINUE
C
      RETURN
      END
      SUBROUTINE PRTRES (X, S, NOBS, VOBS, WT, CHI, IJS, N, VSTMOD,
     *   NUCAL, ICAL, IREF, PRTLEV, NFP)
C-----------------------------------------------------------------------
C   Routine to print the residuals
C    Inputs:
C     X(*)       D    Parameter array
C     NOBS       I    The number of observations
C     VOBS(4,*)  Cx   Observed visibilities (RR, RL, LR, LL)
C     WT(4,*)    R    Weights of the visibilities
C     CHI(2,*)   R    Parallactic angles of the feeds for each antenna.
C     IJS(2,*)   I    Antenna numbers
C     N          I    Number of antennas
C     VSTMOD(4,*)Cx   Model polarizations per observation (I,Q,U,V)
C     NUCAL      I    Number of unknown calibrator sources
C     ICAL(*)    I    Source numbers of the observations.
C     IREF       I    Reference antenna number.
C     PRTLEV     I    Print level, .GE.5 => print residuals else only
C                     statistics.
C     NFP        I    Number of fixed parameters
C   Output:
C     S          D    Chi squares sum
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION X(*), S
      INTEGER   NOBS, IJS(2,*), N, NUCAL, ICAL(*), IREF, PRTLEV, NFP
      REAL   WT(4,*), CHI(2,*)
      COMPLEX VOBS(4,*), VSTMOD(4,*)
C
      INTEGER   I, J, K, L, NVAR, IX1, IX2, IA11, IA12, IA21, IA22,
     *   IP11, IP12, IP21, IP22, IROUND
      REAL   AMP, PHAS, RNOBS, RNOBSP, RNOBSC
      DOUBLE PRECISION SWT, SP, SC, SWTP, SWTC, RMSRES
      DOUBLE PRECISION PHIW(2,MAXANT), THETAW(2,MAXANT), PD, SR(MAXANT),
     *   DR(MAXANT), SL(MAXANT), DL(MAXANT), ROOT2
      COMPLEX V11, V12, V21, V22, RES11, RES12, RES21, RES22,
     *   VC(4), POLI, PA1, PA2, PR, PL, RS(MAXANT), RD(MAXANT),
     *   LS(MAXANT), LD(MAXANT), PPSTAR, PRREF, PLREF, RRC, RLC, LRC,
     *   LLC , CPA1, CPA2
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NVAR = 4*(N+NUCAL)+1
      ROOT2 = 1.0 / SQRT (2.0)
C                                       Extract parameters and
C                                       precompute some values.
      DO 20 I = 1,N
         PHIW(1,I) = X(I)
         PHIW(2,I) = X(N+I)
         THETAW(1,I) = X(2*N+I)
         THETAW(2,I) = X(3*N+I)
C                                        After some trig manipulations
C                                        factorize the model into
C                                        antenna based factors.
         SR(I) = COS (THETAW(1,I)) + SIN (THETAW(1,I))
         DR(I) = COS (THETAW(1,I)) - SIN (THETAW(1,I))
         SL(I) = COS (THETAW(2,I)) + SIN (THETAW(2,I))
         DL(I) = COS (THETAW(2,I)) - SIN (THETAW(2,I))
         PR    = CMPLX (COS (2.0*PHIW(1,I)),  SIN (2.0*PHIW(1,I)))
         PL    = CMPLX (COS (2.0*PHIW(2,I)), -SIN (2.0*PHIW(2,I)))
         RS(I) = CMPLX (ROOT2*SR(I), 0.0D0)
         RD(I) = CMPLX (ROOT2*DR(I), 0.0D0) * PR
         LS(I) = CMPLX (ROOT2*SL(I), 0.0D0) * PL
         LD(I) = CMPLX (ROOT2*DL(I), 0.0D0)
 20      CONTINUE
C                                       R-L phase difference
      PD = X(NVAR)
      PRREF = CMPLX (COS (PHIW(1,IREF)), SIN (PHIW(1,IREF)))
      PLREF = CMPLX (COS (PHIW(2,IREF)+PD), -SIN (PHIW(2,IREF)+PD))
C                                       Set source model from current
C                                       parameters.
      IF (NUCAL.GT.0) THEN
         DO 50 K = 1,NOBS
            L = ICAL(K)
            IF (L.GT.0) THEN
C                                       Get observed Ipol for
C                                       normalization.
               IF ((WT(1,K)+WT(4,K)) .GT. 0.0) THEN
                  POLI = (WT(1,K) * VOBS(1,K) + WT(4,K) * VOBS(4,K)) /
     *               (WT(1,K)+WT(4,K))
               ELSE
C                                       Must have I visibility.
                  POLI = CMPLX (1.0, 0.0)
                  WT(2,K) = 0.0
                  WT(3,K) = 0.0
                  END IF
C                                       Compute model
               VSTMOD(1,K) = POLI
               DO 40 J = 2,4
C                                       Poln model is fractional values;
C                                       normalize with observed IPOL.
                  VSTMOD(J,K) = CMPLX (X(4*(N+L-1)+J),0.0D0) * POLI
 40               CONTINUE
            END IF
 50         CONTINUE
      END IF
      S = 0.0D0
      SWT = 0.0D0
      RNOBS = 0.0
      SP = 0.0D0
      SWTP = 0.0D0
      RNOBSP = 0.0
      SC = 0.0D0
      SWTC = 0.0D0
      RNOBSC = 0.0
C                                       Label
      IF (PRTLEV.GE.4) THEN
         MSGTXT = 'Residual values in mJy and degrees'
         CALL MSGWRT (4)
         MSGTXT = ' no.  ant1 ant2 chi1 chi2  a RR p    a RL p  '
     *      // '  a LR p    a LL p'
         CALL MSGWRT (4)
         END IF
C                                       Loop over data
      DO 200 K = 1,NOBS
         I = IJS(1,K)
         J = IJS(2,K)
C                                       Parallactic angle factors
         PA1 = CMPLX (COS (2.0*CHI(1,K)), -SIN (2.0*CHI(1,K)))
         PA2 = CMPLX (COS (2.0*CHI(2,K)), -SIN (2.0*CHI(2,K)))
         CPA1 = CONJG (PA1)
         CPA2 = CONJG (PA2)
C                                       Compute model as correlator
C                                       values.
         VC(1) = (VSTMOD(1,K) + VSTMOD(4,K))
         VC(2) = (VSTMOD(2,K) + CMPLX (0.0, 1.0)*VSTMOD(3,K))
         VC(3) = (VSTMOD(2,K) - CMPLX (0.0, 1.0)*VSTMOD(3,K))
         VC(4) = (VSTMOD(1,K) - VSTMOD(4,K))
C                                       RR
         IF (WT(1,K).GT.0.) THEN
C                                       Compute model
            RRC = VC(1) * RS(I) * CONJG(RS(J))
            RLC = VC(2) * RS(I) * CONJG(RD(J)) * CPA2
            LRC = VC(3) * RD(I) * CONJG(RS(J)) * PA1
            LLC = VC(4) * RD(I) * CONJG(RD(J)) * PA1 * CPA2
            V11 = RRC + RLC + LRC + LLC
C                                       Residual
            RES11 = V11 - VOBS(1,K)
C                                       Sum chi squares
            RNOBS = RNOBS + 1.0
            RNOBSP = RNOBSP + 1.0
            S = S+WT(1,K)*(REAL (RES11)**2+AIMAG (RES11)**2)
            SWT = SWT + WT(1,K)
            SP = SP+WT(1,K)*(REAL (RES11)**2+AIMAG (RES11)**2)
            SWTP = SWTP + WT(1,K)
         ELSE
            RES11 = CMPLX (0.0, 0.0)
            END IF
C                                       RL
         IF (WT(2,K).GT.0.) THEN
C                                       Compute residual
            PPSTAR = PRREF * CONJG (PLREF)
            RRC = PPSTAR * VC(1) * RS(I) * CONJG(LS(J)) * PA2
            RLC = PPSTAR * VC(2) * RS(I) * CONJG(LD(J))
            LRC = PPSTAR * VC(3) * RD(I) * CONJG(LS(J)) * PA1 * PA2
            LLC = PPSTAR * VC(4) * RD(I) * CONJG(LD(J)) * PA1
            V12 = RRC + RLC + LRC + LLC
            RES12 = V12 - VOBS(2,K)
C                                       Sum chi squares
            RNOBS = RNOBS + 1.0
            RNOBSC = RNOBSC + 1.0
            S = S+WT(2,K)*(REAL (RES12)**2+AIMAG (RES12)**2)
            SWT = SWT + WT(2,K)
            SC = SC+WT(2,K)*(REAL (RES12)**2+AIMAG (RES12)**2)
            SWTC = SWTC + WT(2,K)
         ELSE
            RES12 = CMPLX(0.0, 0.0)
            END IF
C                                       LR
         IF (WT(3,K).GT.0.) THEN
C                                       Compute residual
            PPSTAR = PLREF * CONJG (PRREF)
            RRC = PPSTAR * VC(1) * LS(I) * CONJG(RS(J)) * CPA1
            RLC = PPSTAR * VC(2) * LS(I) * CONJG(RD(J)) * CPA1 *
     *         CPA2
            LRC = PPSTAR * VC(3) * LD(I) * CONJG(RS(J))
            LLC = PPSTAR * VC(4) * LD(I) * CONJG(RD(J)) * CPA2
            V21 = RRC + RLC + LRC + LLC
            RES21 = V21 - VOBS(3,K)
C                                       Sum chi squares
            RNOBS = RNOBS + 1.0
            RNOBSC = RNOBSC + 1.0
            S = S+WT(3,K)*(REAL (RES21)**2+AIMAG (RES21)**2)
            SWT = SWT + WT(3,K)
            SC = SC+WT(3,K)*(REAL (RES21)**2+AIMAG (RES21)**2)
            SWTC = SWTC + WT(3,K)
         ELSE
            RES21 = CMPLX (0.0, 0.0)
            END IF
C                                       LL
         IF (WT(4,K).GT.0.) THEN
C                                       Compute residual
            RRC = VC(1) * LS(I) * CONJG(LS(J)) * CPA1 * PA2
            RLC = VC(2) * LS(I) * CONJG(LD(J)) * CPA1
            LRC = VC(3) * LD(I) * CONJG(LS(J)) * PA2
            LLC = VC(4) * LD(I) * CONJG(LD(J))
            V22 = RRC + RLC + LRC + LLC
            RES22 = V22 - VOBS(4,K)
C                                       Sum chi squares
            RNOBS = RNOBS + 1.0
            RNOBSP = RNOBSP + 1.0
            S = S+WT(4,K)*(REAL (RES22)**2+AIMAG (RES22)**2)
            SWT = SWT + WT(4,K)
            SP = SP+WT(4,K)*(REAL (RES22)**2+AIMAG (RES22)**2)
            SWTP = SWTP + WT(4,K)
         ELSE
            RES22 = CMPLX (0.0, 0.0)
            END IF
C                                       Print residual
            IF (PRTLEV.GE.5) THEN
               IX1 = IROUND (CHI(1,K)*57.296)
               IX2 = IROUND (CHI(2,K)*57.296)
CDEBUG
C     RES11 = VOBS(1,K)
C     RES12 = VOBS(2,K)
C     RES21 = VOBS(3,K)
C     RES22 = VOBS(4,K)
C                                       RR
               AMP = 1000.0 * SQRT ((REAL (RES11))**2 +
     *            (AIMAG (RES11))**2)
               IF (AMP.GT.9998.9) AMP = 999.
               PHAS = 57.296 * ATAN2 (AIMAG (RES11),
     *            REAL (RES11)+1.0E-20)
               IA11 = IROUND (AMP)
               IF (IA11.GT.9999) IA11 = 9999
               IP11 = IROUND (PHAS)
C                                       RL
               AMP = 1000.0 * SQRT ((REAL (RES12))**2 +
     *            (AIMAG (RES12))**2)
               IF (AMP.GT.9998.9) AMP = 999.
               PHAS = 57.296 * ATAN2 (AIMAG (RES12),
     *            REAL (RES12)+1.0E-20)
               IA12 = IROUND (AMP)
               IF (IA12.GT.9999) IA12 = 9999
               IP12 = IROUND (PHAS)
C                                       LR
               AMP = 1000.0 * SQRT ((REAL (RES21))**2 +
     *            (AIMAG (RES21))**2)
               IF (AMP.GT.9998.9) AMP = 999.
               PHAS = 57.296 * ATAN2 (AIMAG (RES21),
     *            REAL (RES21)+1.0E-20)
               IA21 = IROUND (AMP)
               IF (IA21.GT.9999) IA21 = 9999
               IP21 = IROUND (PHAS)
C                                       LL
               AMP = 1000.0 * SQRT ((REAL (RES22))**2 +
     *            (AIMAG (RES22))**2)
               IF (AMP.GT.9998.9) AMP = 999.
               PHAS = 57.296 * ATAN2 (AIMAG (RES22),
     *            REAL (RES22)+1.0E-20)
               IA22 = IROUND (AMP)
               IF (IA22.GT.9999) IA22 = 9999
               IP22 = IROUND (PHAS)
               WRITE (MSGTXT,1200) K, I, J, IX1, IX2, IA11, IP11, IA12,
     *            IP12, IA21, IP21, IA22, IP22
               CALL MSGWRT (4)
               END IF
 200     CONTINUE
      RMSRES = SQRT (S / (RNOBS-NVAR+NFP) * RNOBS / (2.0*SWT))
      WRITE (MSGTXT,1201) RMSRES
      CALL MSGWRT (4)
      RMSRES = SQRT (SP / (RNOBSP-NVAR+NFP) * RNOBSP / (2.0*SWTP))
      WRITE (MSGTXT,1202) RMSRES
      CALL MSGWRT (4)
      RMSRES = SQRT (SC / (RNOBSC-NVAR+NFP) * RNOBSC / (2.0*SWTC))
      WRITE (MSGTXT,1203) RMSRES
      CALL MSGWRT (4)
C
      RETURN
C-----------------------------------------------------------------------
 1200 FORMAT (13I5)
 1201 FORMAT ('Total RMS residual = ',1PD15.5, ' Jy')
 1202 FORMAT ('Parallel hand RMS residual = ',1PD15.5, ' Jy')
 1203 FORMAT ('Cross hand RMS residual = ',1PD15.5, ' Jy')
      END
      SUBROUTINE SUMIT(D, FX, G, IV, LIV, LV, N, V, X)
C-----------------------------------------------------------------------
C   AIPS modifications:
C    Changed WRITEs in PARCK to use AIPS routine MSGWRT
C    Changed ITSUM to use MSGWRT.
C    Removed IMDCON and calls (return value set to 0)
C-----------------------------------------------------------------------
C
C  ***  CARRY OUT SUMSL (UNCONSTRAINED MINIMIZATION) ITERATIONS, USING
C  ***  DOUBLE-DOGLEG/BFGS STEPS.
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER   LIV, LV
      INTEGER   IV(LIV), N
      DOUBLE PRECISION    D(*), FX, G(*), V(LV), X(*)
C
C--------------------------  PARAMETER USAGE  --------------------------
C
C D.... SCALE VECTOR.
C FX... FUNCTION VALUE.
C G.... GRADIENT VECTOR.
C IV... INTEGER VALUE ARRAY.
C LIV.. LENGTH OF IV (AT LEAST 60).
C LV... LENGTH OF V (AT LEAST 71 + N*(N+13)/2).
C N.... NUMBER OF VARIABLES (COMPONENTS IN X AND G).
C V.... FLOATING-POINT VALUE ARRAY.
C X.... VECTOR OF PARAMETERS TO BE OPTIMIZED.
C
C  ***  DISCUSSION  ***
C
C        PARAMETERS IV, N, V, AND X ARE THE SAME AS THE CORRESPONDING
C     ONES TO SUMSL (WHICH SEE), EXCEPT THAT V CAN BE SHORTER (SINCE
C     THE PART OF V THAT SUMSL USES FOR STORING G IS NOT NEEDED).
C     MOREOVER, COMPARED WITH SUMSL, IV(1) MAY HAVE THE TWO ADDITIONAL
C     OUTPUT VALUES 1 AND 2, WHICH ARE EXPLAINED BELOW, AS IS THE USE
C     OF IV(TOOBIG) AND IV(NFGCAL).  THE VALUE IV(G), WHICH IS AN
C     OUTPUT VALUE FROM SUMSL (AND SMSNO), IS NOT REFERENCED BY
C     SUMIT OR THE SUBROUTINES IT CALLS.
C        FX AND G NEED NOT HAVE BEEN INITIALIZED WHEN SUMIT IS CALLED
C     WITH IV(1) = 12, 13, OR 14.
C
C IV(1) = 1 MEANS THE CALLER SHOULD SET FX TO F(X), THE FUNCTION VALUE
C             AT X, AND CALL SUMIT AGAIN, HAVING CHANGED NONE OF THE
C             OTHER PARAMETERS.  AN EXCEPTION OCCURS IF F(X) CANNOT BE
C             (E.G. IF OVERFLOW WOULD OCCUR), WHICH MAY HAPPEN BECAUSE
C             OF AN OVERSIZED STEP.  IN THIS CASE THE CALLER SHOULD SET
C             IV(TOOBIG) = IV(2) TO 1, WHICH WILL CAUSE SUMIT TO IG-
C             NORE FX AND TRY A SMALLER STEP.  THE PARAMETER NF THAT
C             SUMSL PASSES TO CALCF (FOR POSSIBLE USE BY CALCG) IS A
C             COPY OF IV(NFCALL) = IV(6).
C IV(1) = 2 MEANS THE CALLER SHOULD SET G TO G(X), THE GRADIENT VECTOR
C             OF F AT X, AND CALL SUMIT AGAIN, HAVING CHANGED NONE OF
C             THE OTHER PARAMETERS EXCEPT POSSIBLY THE SCALE VECTOR D
C             WHEN IV(DTYPE) = 0.  THE PARAMETER NF THAT SUMSL PASSES
C             TO CALCG IS IV(NFGCAL) = IV(7).  IF G(X) CANNOT BE
C             EVALUATED, THEN THE CALLER MAY SET IV(NFGCAL) TO 0, IN
C             WHICH CASE SUMIT WILL RETURN WITH IV(1) = 65.
C.
C  ***  GENERAL  ***
C
C     CODED BY DAVID M. GAY (DECEMBER 1979).  REVISED SEPT. 1982.
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED
C     IN PART BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324 AND MCS-7906671.
C
C        (SEE SUMSL FOR REFERENCES.)
C
C+++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++++
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER   DG1, DUMMY, G01, I, K, L, LSTGST, NWTST1, STEP1,
     1          TEMP1, W, X01, Z
      DOUBLE PRECISION T
C
C     ***  CONSTANTS  ***
C
      DOUBLE PRECISION HALF, NEGONE, ONE, ONEP2, ZERO
C
C  ***  NO INTRINSIC FUNCTIONS  ***
C
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
      EXTERNAL ASSST, DBDOG, DEFLT, DOTPRD, LITVMU, LIVMUL, ITSUM,
     1         LTVMUL, LUPDAT, LVMUL, PARCK, RELDST, STOPX, VAXPY,
     2         VCOPY, VSCOPY, VVMULP, V2NORM, WZBFGS
      LOGICAL STOPX
      DOUBLE PRECISION DOTPRD, RELDST, V2NORM
C
C ASSST.... ASSESSES CANDIDATE STEP.
C DBDOG.... COMPUTES DOUBLE-DOGLEG (CANDIDATE) STEP.
C DEFLT.... SUPPLIES DEFAULT IV AND V INPUT COMPONENTS.
C DOTPRD... RETURNS INNER PRODUCT OF TWO VECTORS.
C ITSUM.... PRINTS ITERATION SUMMARY AND INFO ON INITIAL AND FINAL X.
C LITVMU... MULTIPLIES INVERSE TRANSPOSE OF LOWER TRIANGLE TIMES VECTOR.
C LIVMUL... MULTIPLIES INVERSE OF LOWER TRIANGLE TIMES VECTOR.
C LTVMUL... MULTIPLIES TRANSPOSE OF LOWER TRIANGLE TIMES VECTOR.
C LUPDT.... UPDATES CHOLESKY FACTOR OF HESSIAN APPROXIMATION.
C LVMUL.... MULTIPLIES LOWER TRIANGLE TIMES VECTOR.
C PARCK.... CHECKS VALIDITY OF INPUT IV AND V VALUES.
C RELDST... COMPUTES V(RELDX) = RELATIVE STEP SIZE.
C STOPX.... RETURNS .TRUE. IF THE BREAK KEY HAS BEEN PRESSED.
C VAXPY.... COMPUTES SCALAR TIMES ONE VECTOR PLUS ANOTHER.
C VCOPY.... COPIES ONE VECTOR TO ANOTHER.
C VSCOPY... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.
C VVMULP... MULTIPLIES VECTOR BY VECTOR RAISED TO POWER (COMPONENTWISE).
C V2NORM... RETURNS THE 2-NORM OF A VECTOR.
C WZBFGS... COMPUTES W AND Z FOR LUPDAT CORRESPONDING TO BFGS UPDATE.
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
      INTEGER   CNVCOD, DG, DGNORM, DINIT, DSTNRM, DST0, F, F0, FDIF,
     1        GTHG, GTSTEP, G0, INCFAC, INITH, IRC, KAGQT, LMAT, LMAX0,
     2        LMAXS, MODE, MODEL, MXFCAL, MXITER, NEXTV, NFCALL, NFGCAL,
     3        NGCALL, NITER, NREDUC, NWTSTP, PREDUC, RADFAC, RADINC,
     4        RADIUS, RAD0, RELDX, RESTOR, STEP, STGLIM, STLSTG, TOOBIG,
     5        TUNER4, TUNER5, VNEED, XIRC, X0
      INTEGER   ITEMP
C
C  ***  IV SUBSCRIPT VALUES  ***
C
C/6
      DATA CNVCOD/55/, DG/37/, G0/48/, INITH/25/, IRC/29/, KAGQT/33/,
     1     MODE/35/, MODEL/5/, MXFCAL/17/, MXITER/18/, NFCALL/6/,
     2     NFGCAL/7/, NGCALL/30/, NITER/31/, NWTSTP/34/, RADINC/8/,
     3     RESTOR/9/, STEP/40/, STGLIM/11/, STLSTG/41/, TOOBIG/2/,
     4     VNEED/4/, XIRC/13/, X0/43/
C/7
C     PARAMETER (CNVCOD=55, DG=37, G0=48, INITH=25, IRC=29, KAGQT=33,
C    1           MODE=35, MODEL=5, MXFCAL=17, MXITER=18, NFCALL=6,
C    2           NFGCAL=7, NGCALL=30, NITER=31, NWTSTP=34, RADINC=8,
C    3           RESTOR=9, STEP=40, STGLIM=11, STLSTG=41, TOOBIG=2,
C    4           VNEED=4, XIRC=13, X0=43)
C/
C
C  ***  V SUBSCRIPT VALUES  ***
C
C/6
      DATA DGNORM/1/, DINIT/38/, DSTNRM/2/, DST0/3/, F/10/, F0/13/,
     1     FDIF/11/, GTHG/44/, GTSTEP/4/, INCFAC/23/, LMAT/42/,
     2     LMAX0/35/, LMAXS/36/, NEXTV/47/, NREDUC/6/, PREDUC/7/,
     3     RADFAC/16/, RADIUS/8/, RAD0/9/, RELDX/17/, TUNER4/29/,
     4     TUNER5/30/
C/7
C     PARAMETER (DGNORM=1, DINIT=38, DSTNRM=2, DST0=3, F=10, F0=13,
C    1           FDIF=11, GTHG=44, GTSTEP=4, INCFAC=23, LMAT=42,
C    2           LMAX0=35, LMAXS=36, NEXTV=47, NREDUC=6, PREDUC=7,
C    3           RADFAC=16, RADIUS=8, RAD0=9, RELDX=17, TUNER4=29,
C    4           TUNER5=30)
C/
C
C/6
      DATA HALF/0.5D+0/, NEGONE/-1.D+0/, ONE/1.D+0/, ONEP2/1.2D+0/,
     1     ZERO/0.D+0/
C/7
C     PARAMETER (HALF=0.5D+0, NEGONE=-1.D+0, ONE=1.D+0, ONEP2=1.2D+0,
C    1           ZERO=0.D+0)
C/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C-----------------------------------------------------------------------
      I = IV(1)
      IF (I .EQ. 1) GO TO 50
      IF (I .EQ. 2) GO TO 60
C
C  ***  CHECK VALIDITY OF IV AND V INPUT VALUES  ***
C
      IF (IV(1) .EQ. 0) CALL DEFLT(2, IV, LIV, LV, V)
      IF (IV(1) .EQ. 12 .OR. IV(1) .EQ. 13)
     1     IV(VNEED) = IV(VNEED) + N*(N+13)/2
      CALL PARCK(2, D, IV, LIV, LV, N, V)
      I = IV(1) - 2
      IF (I .GT. 12) GO TO 999
      GO TO (180, 180, 180, 180, 180, 180, 120, 90, 120, 10, 10, 20), I
C
C  ***  STORAGE ALLOCATION  ***
C
10    L = IV(LMAT)
      IV(X0) = L + N*(N+1)/2
      IV(STEP) = IV(X0) + N
      IV(STLSTG) = IV(STEP) + N
      IV(G0) = IV(STLSTG) + N
      IV(NWTSTP) = IV(G0) + N
      IV(DG) = IV(NWTSTP) + N
      IV(NEXTV) = IV(DG) + N
      IF (IV(1) .NE. 13) GO TO 20
         IV(1) = 14
         GO TO 999
C
C  ***  INITIALIZATION  ***
C
 20   IV(NITER) = 0
      IV(NFCALL) = 1
      IV(NGCALL) = 1
      IV(NFGCAL) = 1
      IV(MODE) = -1
      IV(MODEL) = 1
      IV(STGLIM) = 1
      IV(TOOBIG) = 0
      IV(CNVCOD) = 0
      IV(RADINC) = 0
      V(RAD0) = ZERO
      IF (V(DINIT) .GE. ZERO) CALL VSCOPY(N, D, V(DINIT))
      IF (IV(INITH) .NE. 1) GO TO 40
C
C     ***  SET THE INITIAL HESSIAN APPROXIMATION TO DIAG(D)**-2  ***
C
         L = IV(LMAT)
         ITEMP = N*(N+1)/2
         CALL VSCOPY(ITEMP, V(L), ZERO)
         K = L - 1
         DO 30 I = 1, N
              K = K + I
              T = D(I)
              IF (T .LE. ZERO) T = ONE
              V(K) = T
 30           CONTINUE
C
C  ***  COMPUTE INITIAL FUNCTION VALUE  ***
C
 40   IV(1) = 1
      GO TO 999
C
 50   V(F) = FX
      IF (IV(MODE) .GE. 0) GO TO 180
      IV(1) = 2
      IF (IV(TOOBIG) .EQ. 0) GO TO 999
         IV(1) = 63
         GO TO 300
C
C  ***  MAKE SURE GRADIENT COULD BE COMPUTED  ***
C
 60   IF (IV(NFGCAL) .NE. 0) GO TO 70
         IV(1) = 65
         GO TO 300
C
 70   DG1 = IV(DG)
      CALL VVMULP(N, V(DG1), G, D, -1)
      V(DGNORM) = V2NORM(N, V(DG1))
C
      IF (IV(CNVCOD) .NE. 0) GO TO 290
      IF (IV(MODE) .EQ. 0) GO TO 250
C
C  ***  ALLOW FIRST STEP TO HAVE SCALED 2-NORM AT MOST V(LMAX0)  ***
C
      V(RADIUS) = V(LMAX0)
C
      IV(MODE) = 0
C
C
C-----------------------------  MAIN LOOP  -----------------------------
C
C
C  ***  PRINT ITERATION SUMMARY, CHECK ITERATION LIMIT  ***
C
 80   CALL ITSUM(D, G, IV, LIV, N, V, X)
 90   K = IV(NITER)
      IF (K .LT. IV(MXITER)) GO TO 100
         IV(1) = 10
         GO TO 300
C
C  ***  UPDATE RADIUS  ***
C
 100  IV(NITER) = K + 1
      IF(K.GT.0)V(RADIUS) = V(RADFAC) * V(DSTNRM)
C
C  ***  INITIALIZE FOR START OF NEXT ITERATION  ***
C
      G01 = IV(G0)
      X01 = IV(X0)
      V(F0) = V(F)
      IV(IRC) = 4
      IV(KAGQT) = -1
C
C     ***  COPY X TO X0, G TO G0  ***
C
      CALL VCOPY(N, V(X01), X)
      CALL VCOPY(N, V(G01), G)
C
C  ***  CHECK STOPX AND FUNCTION EVALUATION LIMIT  ***
C
 110  IF (.NOT.STOPX (DUMMY)) GO TO 130
         IV(1) = 11
         GO TO 140
C
C     ***  COME HERE WHEN RESTARTING AFTER FUNC. EVAL. LIMIT OR STOPX.
C
 120  IF (V(F) .GE. V(F0)) GO TO 130
         V(RADFAC) = ONE
         K = IV(NITER)
         GO TO 100
C
 130  IF (IV(NFCALL) .LT. IV(MXFCAL)) GO TO 150
         IV(1) = 9
 140     IF (V(F) .GE. V(F0)) GO TO 300
C
C        ***  IN CASE OF STOPX OR FUNCTION EVALUATION LIMIT WITH
C        ***  IMPROVED V(F), EVALUATE THE GRADIENT AT X.
C
              IV(CNVCOD) = IV(1)
              GO TO 240
C
C. . . . . . . . . . . . .  COMPUTE CANDIDATE STEP  . . . . . . . . . .
C
 150  STEP1 = IV(STEP)
      DG1 = IV(DG)
      NWTST1 = IV(NWTSTP)
      IF (IV(KAGQT) .GE. 0) GO TO 160
         L = IV(LMAT)
         CALL LIVMUL(N, V(NWTST1), V(L), G)
         V(NREDUC) = HALF * DOTPRD(N, V(NWTST1), V(NWTST1))
         CALL LITVMU(N, V(NWTST1), V(L), V(NWTST1))
         CALL VVMULP(N, V(STEP1), V(NWTST1), D, 1)
         V(DST0) = V2NORM(N, V(STEP1))
         CALL VVMULP(N, V(DG1), V(DG1), D, -1)
         CALL LTVMUL(N, V(STEP1), V(L), V(DG1))
         V(GTHG) = V2NORM(N, V(STEP1))
         IV(KAGQT) = 0
 160  CALL DBDOG(V(DG1), LV, N, V(NWTST1), V(STEP1), V)
      IF (IV(IRC) .EQ. 6) GO TO 180
C
C  ***  CHECK WHETHER EVALUATING F(X0 + STEP) LOOKS WORTHWHILE  ***
C
      IF (V(DSTNRM) .LE. ZERO) GO TO 180
      IF (IV(IRC) .NE. 5) GO TO 170
      IF (V(RADFAC) .LE. ONE) GO TO 170
      IF (V(PREDUC) .LE. ONEP2 * V(FDIF)) GO TO 180
C
C  ***  COMPUTE F(X0 + STEP)  ***
C
 170  X01 = IV(X0)
      STEP1 = IV(STEP)
      CALL VAXPY(N, X, ONE, V(STEP1), V(X01))
      IV(NFCALL) = IV(NFCALL) + 1
      IV(1) = 1
      IV(TOOBIG) = 0
      GO TO 999
C
C. . . . . . . . . . . . .  ASSESS CANDIDATE STEP  . . . . . . . . . . .
C
 180  X01 = IV(X0)
      V(RELDX) = RELDST(N, D, X, V(X01))
      CALL ASSST(IV, LIV, LV, V)
      STEP1 = IV(STEP)
      LSTGST = IV(STLSTG)
      IF (IV(RESTOR) .EQ. 1) CALL VCOPY(N, X, V(X01))
      IF (IV(RESTOR) .EQ. 2) CALL VCOPY(N, V(LSTGST), V(STEP1))
      IF (IV(RESTOR) .NE. 3) GO TO 190
         CALL VCOPY(N, V(STEP1), V(LSTGST))
         CALL VAXPY(N, X, ONE, V(STEP1), V(X01))
         V(RELDX) = RELDST(N, D, X, V(X01))
C
 190  K = IV(IRC)
      GO TO (200,230,230,230,200,210,220,220,220,220,220,220,280,250), K
C
C     ***  RECOMPUTE STEP WITH CHANGED RADIUS  ***
C
 200     V(RADIUS) = V(RADFAC) * V(DSTNRM)
         GO TO 110
C
C  ***  COMPUTE STEP OF LENGTH V(LMAXS) FOR SINGULAR CONVERGENCE TEST.
C
 210  V(RADIUS) = V(LMAXS)
      GO TO 150
C
C  ***  CONVERGENCE OR FALSE CONVERGENCE  ***
C
 220  IV(CNVCOD) = K - 4
      IF (V(F) .GE. V(F0)) GO TO 290
         IF (IV(XIRC) .EQ. 14) GO TO 290
              IV(XIRC) = 14
C
C. . . . . . . . . . . .  PROCESS ACCEPTABLE STEP  . . . . . . . . . . .
C
 230  IF (IV(IRC) .NE. 3) GO TO 240
         STEP1 = IV(STEP)
         TEMP1 = IV(STLSTG)
C
C     ***  SET  TEMP1 = HESSIAN * STEP  FOR USE IN GRADIENT TESTS  ***
C
         L = IV(LMAT)
         CALL LTVMUL(N, V(TEMP1), V(L), V(STEP1))
         CALL LVMUL(N, V(TEMP1), V(L), V(TEMP1))
C
C  ***  COMPUTE GRADIENT  ***
C
 240  IV(NGCALL) = IV(NGCALL) + 1
      IV(1) = 2
      GO TO 999
C
C  ***  INITIALIZATIONS -- G0 = G - G0, ETC.  ***
C
 250  G01 = IV(G0)
      CALL VAXPY(N, V(G01), NEGONE, V(G01), G)
      STEP1 = IV(STEP)
      TEMP1 = IV(STLSTG)
      IF (IV(IRC) .NE. 3) GO TO 270
C
C  ***  SET V(RADFAC) BY GRADIENT TESTS  ***
C
C     ***  SET  TEMP1 = DIAG(D)**-1 * (HESSIAN*STEP + (G(X0)-G(X)))  ***
C
         CALL VAXPY(N, V(TEMP1), NEGONE, V(G01), V(TEMP1))
         CALL VVMULP(N, V(TEMP1), V(TEMP1), D, -1)
C
C        ***  DO GRADIENT TESTS  ***
C
         IF (V2NORM(N, V(TEMP1)) .LE. V(DGNORM) * V(TUNER4))
     1                  GO TO 260
              IF (DOTPRD(N, G, V(STEP1))
     1                  .GE. V(GTSTEP) * V(TUNER5))  GO TO 270
 260               V(RADFAC) = V(INCFAC)
C
C  ***  UPDATE H, LOOP  ***
C
 270  W = IV(NWTSTP)
      Z = IV(X0)
      L = IV(LMAT)
      CALL WZBFGS(V(L), N, V(STEP1), V(W), V(G01), V(Z))
C
C     ** USE THE N-VECTORS STARTING AT V(STEP1) AND V(G01) FOR SCRATCH..
      CALL LUPDAT(V(TEMP1), V(STEP1), V(L), V(G01), V(L), N, V(W), V(Z))
      IV(1) = 2
      GO TO 80
C
C. . . . . . . . . . . . . .  MISC. DETAILS  . . . . . . . . . . . . . .
C
C  ***  BAD PARAMETERS TO ASSESS  ***
C
 280  IV(1) = 64
      GO TO 300
C
C  ***  PRINT SUMMARY OF FINAL ITERATION AND OTHER REQUESTED ITEMS  ***
C
 290  IV(1) = IV(CNVCOD)
      IV(CNVCOD) = 0
 300  CALL ITSUM(D, G, IV, LIV, N, V, X)
C
 999  RETURN
C
C  ***  LAST LINE OF SUMIT FOLLOWS  ***
      END
      SUBROUTINE DEFLT(ALG, IV, LIV, LV, V)
C-----------------------------------------------------------------------
C  ***  SUPPLY ***SOL (VERSION 2.3) DEFAULT VALUES TO IV AND V  ***
C
C  ***  ALG = 1 MEANS REGRESSION CONSTANTS.
C  ***  ALG = 2 MEANS GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS.
C-----------------------------------------------------------------------
      INTEGER   LIV, LV
      INTEGER   ALG, IV(LIV)
      DOUBLE PRECISION V(LV)
C
C      EXTERNAL IMDCON, VDFLT
C      INTEGER IMDCON
      EXTERNAL VDFLT
C IMDCON... RETURNS MACHINE-DEPENDENT INTEGER CONSTANTS.
C VDFLT.... PROVIDES DEFAULT VALUES TO V.
C
      INTEGER   MIV, MV
      INTEGER   MINIV(2), MINV(2)
C
C  ***  SUBSCRIPTS FOR IV  ***
C
      INTEGER   ALGSAV, COVPRT, COVREQ, DTYPE, HC, IERR, INITH, INITS,
     1        IPIVOT, IVNEED, LASTIV, LASTV, LMAT, MXFCAL, MXITER,
     2        NFCOV, NGCOV, NVDFLT, OUTLEV, PARPRT, PARSAV, PERM,
     3        PRUNIT, QRTYP, RDREQ, RMAT, SOLPRT, STATPR, VNEED,
     4        VSAVE, X0PRT
C
C  ***  IV SUBSCRIPT VALUES  ***
C
C/6
      DATA ALGSAV/51/, COVPRT/14/, COVREQ/15/, DTYPE/16/, HC/71/,
     1     IERR/75/, INITH/25/, INITS/25/, IPIVOT/76/, IVNEED/3/,
     2     LASTIV/44/, LASTV/45/, LMAT/42/, MXFCAL/17/, MXITER/18/,
     3     NFCOV/52/, NGCOV/53/, NVDFLT/50/, OUTLEV/19/, PARPRT/20/,
     4     PARSAV/49/, PERM/58/, PRUNIT/21/, QRTYP/80/, RDREQ/57/,
     5     RMAT/78/, SOLPRT/22/, STATPR/23/, VNEED/4/, VSAVE/60/,
     6     X0PRT/24/
C/7
C     PARAMETER (ALGSAV=51, COVPRT=14, COVREQ=15, DTYPE=16, HC=71,
C    1           IERR=75, INITH=25, INITS=25, IPIVOT=76, IVNEED=3,
C    2           LASTIV=44, LASTV=45, LMAT=42, MXFCAL=17, MXITER=18,
C    3           NFCOV=52, NGCOV=53, NVDFLT=50, OUTLEV=19, PARPRT=20,
C    4           PARSAV=49, PERM=58, PRUNIT=21, QRTYP=80, RDREQ=57,
C    5           RMAT=78, SOLPRT=22, STATPR=23, VNEED=4, VSAVE=60,
C    6           X0PRT=24)
C/
      DATA MINIV(1)/80/, MINIV(2)/59/, MINV(1)/98/, MINV(2)/71/
C
C-------------------------------  BODY  --------------------------------
C
      IF (ALG .LT. 1 .OR. ALG .GT. 2) GO TO 40
      MIV = MINIV(ALG)
      IF (LIV .LT. MIV) GO TO 20
      MV = MINV(ALG)
      IF (LV .LT. MV) GO TO 30
      CALL VDFLT(ALG, LV, V)
      IV(1) = 12
      IV(ALGSAV) = ALG
      IV(IVNEED) = 0
      IV(LASTIV) = MIV
      IV(LASTV) = MV
      IV(LMAT) = MV + 1
      IV(MXFCAL) = 200
      IV(MXITER) = 150
      IV(OUTLEV) = 1
      IV(PARPRT) = 1
      IV(PERM) = MIV + 1
      IV(PRUNIT) = 0
      IV(SOLPRT) = 1
      IV(STATPR) = 1
      IV(VNEED) = 0
      IV(X0PRT) = 1
C
      IF (ALG .GE. 2) GO TO 10
C
C  ***  REGRESSION  VALUES
C
      IV(COVPRT) = 3
      IV(COVREQ) = 1
      IV(DTYPE) = 1
      IV(HC) = 0
      IV(IERR) = 0
      IV(INITS) = 0
      IV(IPIVOT) = 0
      IV(NVDFLT) = 32
      IV(PARSAV) = 67
      IV(QRTYP) = 1
      IV(RDREQ) = 3
      IV(RMAT) = 0
      IV(VSAVE) = 58
      GO TO 999
C
C  ***  GENERAL OPTIMIZATION VALUES
C
 10   IV(DTYPE) = 0
      IV(INITH) = 1
      IV(NFCOV) = 0
      IV(NGCOV) = 0
      IV(NVDFLT) = 25
      IV(PARSAV) = 47
      GO TO 999
C
 20   IV(1) = 15
      GO TO 999
C
 30   IV(1) = 16
      GO TO 999
C
 40   IV(1) = 67
C
 999  RETURN
C  ***  LAST CARD OF DEFLT FOLLOWS  ***
      END
      SUBROUTINE ASSST(IV, LIV, LV, V)
C-----------------------------------------------------------------------
C  ***  ASSESS CANDIDATE STEP (***SOL VERSION 2.3)  ***
C
      INTEGER   LIV, LV
      INTEGER   IV(LIV)
      DOUBLE PRECISION    V(LV)
C
C  ***  PURPOSE  ***
C
C        THIS SUBROUTINE IS CALLED BY AN UNCONSTRAINED MINIMIZATION
C     ROUTINE TO ASSESS THE NEXT CANDIDATE STEP.  IT MAY RECOMMEND ONE
C     OF SEVERAL COURSES OF ACTION, SUCH AS ACCEPTING THE STEP, RECOM-
C     PUTING IT USING THE SAME OR A NEW QUADRATIC MODEL, OR HALTING DUE
C     TO CONVERGENCE OR FALSE CONVERGENCE.  SEE THE RETURN CODE LISTING
C     BELOW.
C
C--------------------------  PARAMETER USAGE  --------------------------
C
C  IV (I/O) INTEGER PARAMETER AND SCRATCH VECTOR -- SEE DESCRIPTION
C             BELOW OF IV VALUES REFERENCED.
C LIV (IN)  LENGTH OF IV ARRAY.
C  LV (IN)  LENGTH OF V ARRAY.
C   V (I/O) REAL PARAMETER AND SCRATCH VECTOR -- SEE DESCRIPTION
C             BELOW OF V VALUES REFERENCED.
C
C  ***  IV VALUES REFERENCED  ***
C
C    IV(IRC) (I/O) ON INPUT FOR THE FIRST STEP TRIED IN A NEW ITERATION,
C             IV(IRC) SHOULD BE SET TO 3 OR 4 (THE VALUE TO WHICH IT IS
C             SET WHEN STEP IS DEFINITELY TO BE ACCEPTED).  ON INPUT
C             AFTER STEP HAS BEEN RECOMPUTED, IV(IRC) SHOULD BE
C             UNCHANGED SINCE THE PREVIOUS RETURN OF ASSST.
C                ON OUTPUT, IV(IRC) IS A RETURN CODE HAVING ONE OF THE
C             FOLLOWING VALUES...
C                  1 = SWITCH MODELS OR TRY SMALLER STEP.
C                  2 = SWITCH MODELS OR ACCEPT STEP.
C                  3 = ACCEPT STEP AND DETERMINE V(RADFAC) BY GRADIENT
C                       TESTS.
C                  4 = ACCEPT STEP, V(RADFAC) HAS BEEN DETERMINED.
C                  5 = RECOMPUTE STEP (USING THE SAME MODEL).
C                  6 = RECOMPUTE STEP WITH RADIUS = V(LMAXS) BUT DO NOT
C                       EVAULATE THE OBJECTIVE FUNCTION.
C                  7 = X-CONVERGENCE (SEE V(XCTOL)).
C                  8 = RELATIVE FUNCTION CONVERGENCE (SEE V(RFCTOL)).
C                  9 = BOTH X- AND RELATIVE FUNCTION CONVERGENCE.
C                 10 = ABSOLUTE FUNCTION CONVERGENCE (SEE V(AFCTOL)).
C                 11 = SINGULAR CONVERGENCE (SEE V(LMAXS)).
C                 12 = FALSE CONVERGENCE (SEE V(XFTOL)).
C                 13 = IV(IRC) WAS OUT OF RANGE ON INPUT.
C             RETURN CODE I HAS PRECDENCE OVER I+1 FOR I = 9, 10, 11.
C IV(MLSTGD) (I/O) SAVED VALUE OF IV(MODEL).
C  IV(MODEL) (I/O) ON INPUT, IV(MODEL) SHOULD BE AN INTEGER IDENTIFYING
C             THE CURRENT QUADRATIC MODEL OF THE OBJECTIVE FUNCTION.
C             IF A PREVIOUS STEP YIELDED A BETTER FUNCTION REDUCTION,
C             THEN IV(MODEL) WILL BE SET TO IV(MLSTGD) ON OUTPUT.
C IV(NFCALL) (IN)  INVOCATION COUNT FOR THE OBJECTIVE FUNCTION.
C IV(NFGCAL) (I/O) VALUE OF IV(NFCALL) AT STEP THAT GAVE THE BIGGEST
C             FUNCTION REDUCTION THIS ITERATION.  IV(NFGCAL) REMAINS
C             UNCHANGED UNTIL A FUNCTION REDUCTION IS OBTAINED.
C IV(RADINC) (I/O) THE NUMBER OF RADIUS INCREASES (OR MINUS THE NUMBER
C             OF DECREASES) SO FAR THIS ITERATION.
C IV(RESTOR) (OUT) SET TO 1 IF V(F) HAS BEEN RESTORED AND X SHOULD BE
C             RESTORED TO ITS INITIAL VALUE, TO 2 IF X SHOULD BE SAVED,
C             TO 3 IF X SHOULD BE RESTORED FROM THE SAVED VALUE, AND TO
C             0 OTHERWISE.
C  IV(STAGE) (I/O) COUNT OF THE NUMBER OF MODELS TRIED SO FAR IN THE
C             CURRENT ITERATION.
C IV(STGLIM) (IN)  MAXIMUM NUMBER OF MODELS TO CONSIDER.
C IV(SWITCH) (OUT) SET TO 0 UNLESS A NEW MODEL IS BEING TRIED AND IT
C             GIVES A SMALLER FUNCTION VALUE THAN THE PREVIOUS MODEL,
C             IN WHICH CASE ASSST SETS IV(SWITCH) = 1.
C IV(TOOBIG) (IN)  IS NONZERO IF STEP WAS TOO BIG (E.G. IF IT CAUSED
C             OVERFLOW).
C   IV(XIRC) (I/O) VALUE THAT IV(IRC) WOULD HAVE IN THE ABSENCE OF
C             CONVERGENCE, FALSE CONVERGENCE, AND OVERSIZED STEPS.
C
C  ***  V VALUES REFERENCED  ***
C
C V(AFCTOL) (IN)  ABSOLUTE FUNCTION CONVERGENCE TOLERANCE.  IF THE
C             ABSOLUTE VALUE OF THE CURRENT FUNCTION VALUE V(F) IS LESS
C             THAN V(AFCTOL), THEN ASSST RETURNS WITH IV(IRC) = 10.
C V(DECFAC) (IN)  FACTOR BY WHICH TO DECREASE RADIUS WHEN IV(TOOBIG) IS
C             NONZERO.
C V(DSTNRM) (IN)  THE 2-NORM OF D*STEP.
C V(DSTSAV) (I/O) VALUE OF V(DSTNRM) ON SAVED STEP.
C   V(DST0) (IN)  THE 2-NORM OF D TIMES THE NEWTON STEP (WHEN DEFINED,
C             I.E., FOR V(NREDUC) .GE. 0).
C      V(F) (I/O) ON BOTH INPUT AND OUTPUT, V(F) IS THE OBJECTIVE FUNC-
C             TION VALUE AT X.  IF X IS RESTORED TO A PREVIOUS VALUE,
C             THEN V(F) IS RESTORED TO THE CORRESPONDING VALUE.
C   V(FDIF) (OUT) THE FUNCTION REDUCTION V(F0) - V(F) (FOR THE OUTPUT
C             VALUE OF V(F) IF AN EARLIER STEP GAVE A BIGGER FUNCTION
C             DECREASE, AND FOR THE INPUT VALUE OF V(F) OTHERWISE).
C V(FLSTGD) (I/O) SAVED VALUE OF V(F).
C     V(F0) (IN)  OBJECTIVE FUNCTION VALUE AT START OF ITERATION.
C V(GTSLST) (I/O) VALUE OF V(GTSTEP) ON SAVED STEP.
C V(GTSTEP) (IN)  INNER PRODUCT BETWEEN STEP AND GRADIENT.
C V(INCFAC) (IN)  MINIMUM FACTOR BY WHICH TO INCREASE RADIUS.
C  V(LMAXS) (IN)  MAXIMUM REASONABLE STEP SIZE (AND INITIAL STEP BOUND).
C             IF THE ACTUAL FUNCTION DECREASE IS NO MORE THAN TWICE
C             WHAT WAS PREDICTED, IF A RETURN WITH IV(IRC) = 7, 8, 9,
C             OR 10 DOES NOT OCCUR, IF V(DSTNRM) .GT. V(LMAXS), AND IF
C             V(PREDUC) .LE. V(SCTOL) * ABS(V(F0)), THEN ASSST RE-
C             TURNS WITH IV(IRC) = 11.  IF SO DOING APPEARS WORTHWHILE,
C             THEN ASSST REPEATS THIS TEST WITH V(PREDUC) COMPUTED FOR
C             A STEP OF LENGTH V(LMAXS) (BY A RETURN WITH IV(IRC) = 6).
C V(NREDUC) (I/O)  FUNCTION REDUCTION PREDICTED BY QUADRATIC MODEL FOR
C             NEWTON STEP.  IF ASSST IS CALLED WITH IV(IRC) = 6, I.E.,
C             IF V(PREDUC) HAS BEEN COMPUTED WITH RADIUS = V(LMAXS) FOR
C             USE IN THE SINGULAR CONVERVENCE TEST, THEN V(NREDUC) IS
C             SET TO -V(PREDUC) BEFORE THE LATTER IS RESTORED.
C V(PLSTGD) (I/O) VALUE OF V(PREDUC) ON SAVED STEP.
C V(PREDUC) (I/O) FUNCTION REDUCTION PREDICTED BY QUADRATIC MODEL FOR
C             CURRENT STEP.
C V(RADFAC) (OUT) FACTOR TO BE USED IN DETERMINING THE NEW RADIUS,
C             WHICH SHOULD BE V(RADFAC)*DST, WHERE  DST  IS EITHER THE
C             OUTPUT VALUE OF V(DSTNRM) OR THE 2-NORM OF
C             DIAG(NEWD)*STEP  FOR THE OUTPUT VALUE OF STEP AND THE
C             UPDATED VERSION, NEWD, OF THE SCALE VECTOR D.  FOR
C             IV(IRC) = 3, V(RADFAC) = 1.0 IS RETURNED.
C V(RDFCMN) (IN)  MINIMUM VALUE FOR V(RADFAC) IN TERMS OF THE INPUT
C             VALUE OF V(DSTNRM) -- SUGGESTED VALUE = 0.1.
C V(RDFCMX) (IN)  MAXIMUM VALUE FOR V(RADFAC) -- SUGGESTED VALUE = 4.0.
C  V(RELDX) (IN) SCALED RELATIVE CHANGE IN X CAUSED BY STEP, COMPUTED
C             (E.G.) BY FUNCTION  RELDST  AS
C                 MAX (D(I)*ABS(X(I)-X0(I)), 1 .LE. I .LE. P) /
C                    MAX (D(I)*(ABS(X(I))+ABS(X0(I))), 1 .LE. I .LE. P).
C V(RFCTOL) (IN)  RELATIVE FUNCTION CONVERGENCE TOLERANCE.  IF THE
C             ACTUAL FUNCTION REDUCTION IS AT MOST TWICE WHAT WAS PRE-
C             DICTED AND  V(NREDUC) .LE. V(RFCTOL)*ABS(V(F0)),  THEN
C             ASSST RETURNS WITH IV(IRC) = 8 OR 9.
C V(STPPAR) (IN)  MARQUARDT PARAMETER -- 0 MEANS FULL NEWTON STEP.
C V(TUNER1) (IN)  TUNING CONSTANT USED TO DECIDE IF THE FUNCTION
C             REDUCTION WAS MUCH LESS THAN EXPECTED.  SUGGESTED
C             VALUE = 0.1.
C V(TUNER2) (IN)  TUNING CONSTANT USED TO DECIDE IF THE FUNCTION
C             REDUCTION WAS LARGE ENOUGH TO ACCEPT STEP.  SUGGESTED
C             VALUE = 10**-4.
C V(TUNER3) (IN)  TUNING CONSTANT USED TO DECIDE IF THE RADIUS
C             SHOULD BE INCREASED.  SUGGESTED VALUE = 0.75.
C  V(XCTOL) (IN)  X-CONVERGENCE CRITERION.  IF STEP IS A NEWTON STEP
C             (V(STPPAR) = 0) HAVING V(RELDX) .LE. V(XCTOL) AND GIVING
C             AT MOST TWICE THE PREDICTED FUNCTION DECREASE, THEN
C             ASSST RETURNS IV(IRC) = 7 OR 9.
C  V(XFTOL) (IN)  FALSE CONVERGENCE TOLERANCE.  IF STEP GAVE NO OR ONLY
C             A SMALL FUNCTION DECREASE AND V(RELDX) .LE. V(XFTOL),
C             THEN ASSST RETURNS WITH IV(IRC) = 12.
C
C-------------------------------  NOTES  -------------------------------
C
C  ***  APPLICATION AND USAGE RESTRICTIONS  ***
C
C        THIS ROUTINE IS CALLED AS PART OF THE NL2SOL (NONLINEAR
C     LEAST-SQUARES) PACKAGE.  IT MAY BE USED IN ANY UNCONSTRAINED
C     MINIMIZATION SOLVER THAT USES DOGLEG, GOLDFELD-QUANDT-TROTTER,
C     OR LEVENBERG-MARQUARDT STEPS.
C
C  ***  ALGORITHM NOTES  ***
C
C        SEE (1) FOR FURTHER DISCUSSION OF THE ASSESSING AND MODEL
C     SWITCHING STRATEGIES.  WHILE NL2SOL CONSIDERS ONLY TWO MODELS,
C     ASSST IS DESIGNED TO HANDLE ANY NUMBER OF MODELS.
C
C  ***  USAGE NOTES  ***
C
C        ON THE FIRST CALL OF AN ITERATION, ONLY THE I/O VARIABLES
C     STEP, X, IV(IRC), IV(MODEL), V(F), V(DSTNRM), V(GTSTEP), AND
C     V(PREDUC) NEED HAVE BEEN INITIALIZED.  BETWEEN CALLS, NO I/O
C     VALUES EXECPT STEP, X, IV(MODEL), V(F) AND THE STOPPING TOLER-
C     ANCES SHOULD BE CHANGED.
C        AFTER A RETURN FOR CONVERGENCE OR FALSE CONVERGENCE, ONE CAN
C     CHANGE THE STOPPING TOLERANCES AND CALL ASSST AGAIN, IN WHICH
C     CASE THE STOPPING TESTS WILL BE REPEATED.
C
C  ***  REFERENCES  ***
C
C     (1) DENNIS, J.E., JR., GAY, D.M., AND WELSCH, R.E. (1981),
C        AN ADAPTIVE NONLINEAR LEAST-SQUARES ALGORITHM,
C        ACM TRANS. MATH. SOFTWARE, VOL. 7, NO. 3.
C
C     (2) POWELL, M.J.D. (1970)  A FORTRAN SUBROUTINE FOR SOLVING
C        SYSTEMS OF NONLINEAR ALGEBRAIC EQUATIONS, IN NUMERICAL
C        METHODS FOR NONLINEAR ALGEBRAIC EQUATIONS, EDITED BY
C        P. RABINOWITZ, GORDON AND BREACH, LONDON.
C
C  ***  HISTORY  ***
C
C        JOHN DENNIS DESIGNED MUCH OF THIS ROUTINE, STARTING WITH
C     IDEAS IN (2). ROY WELSCH SUGGESTED THE MODEL SWITCHING STRATEGY.
C        DAVID GAY AND STEPHEN PETERS CAST THIS SUBROUTINE INTO A MORE
C     PORTABLE FORM (WINTER 1977), AND DAVID GAY CAST IT INTO ITS
C     PRESENT FORM (FALL 1978).
C
C  ***  GENERAL  ***
C
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989, AND
C     MCS-7906671.
C
C------------------------  EXTERNAL QUANTITIES  ------------------------
C
C  ***  NO EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
C  ***  INTRINSIC FUNCTIONS  ***
C/+
C      DOUBLE PRECISION ABS, MAX
C/
C  ***  NO COMMON BLOCKS  ***
C
C--------------------------  LOCAL VARIABLES  --------------------------
C
      LOGICAL GOODX
      INTEGER   I, NFC
      DOUBLE PRECISION EMAX, EMAXS, GTS, RFAC1, XMAX
      DOUBLE PRECISION HALF, ONE, ONEP2, TWO, ZERO
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
      INTEGER   AFCTOL, DECFAC, DSTNRM, DSTSAV, DST0, F, FDIF, FLSTGD,
     1   F0, GTSLST, GTSTEP, INCFAC, IRC, LMAXS, MLSTGD, MODEL, NFCALL,
     2        NFGCAL, NREDUC, PLSTGD, PREDUC, RADFAC, RADINC, RDFCMN,
     3        RDFCMX, RELDX, RESTOR, RFCTOL, SCTOL, STAGE, STGLIM,
     4        STPPAR, SWITCH, TOOBIG, TUNER1, TUNER2, TUNER3, XCTOL,
     5        XFTOL, XIRC
C
C  ***  DATA INITIALIZATIONS  ***
C
C/6
      DATA HALF/0.5D+0/, ONE/1.D+0/, ONEP2/1.2D+0/, TWO/2.D+0/,
     1     ZERO/0.D+0/
C/7
C     PARAMETER (HALF=0.5D+0, ONE=1.D+0, ONEP2=1.2D+0, TWO=2.D+0,
C    1           ZERO=0.D+0)
C/
C
C/6
      DATA IRC/29/, MLSTGD/32/, MODEL/5/, NFCALL/6/, NFGCAL/7/,
     1     RADINC/8/, RESTOR/9/, STAGE/10/, STGLIM/11/, SWITCH/12/,
     2     TOOBIG/2/, XIRC/13/
C/7
C     PARAMETER (IRC=29, MLSTGD=32, MODEL=5, NFCALL=6, NFGCAL=7,
C    1           RADINC=8, RESTOR=9, STAGE=10, STGLIM=11, SWITCH=12,
C    2           TOOBIG=2, XIRC=13)
C/
C/6
      DATA AFCTOL/31/, DECFAC/22/, DSTNRM/2/, DST0/3/, DSTSAV/18/,
     1     F/10/, FDIF/11/, FLSTGD/12/, F0/13/, GTSLST/14/, GTSTEP/4/,
     2     INCFAC/23/, LMAXS/36/, NREDUC/6/, PLSTGD/15/, PREDUC/7/,
     3     RADFAC/16/, RDFCMN/24/, RDFCMX/25/, RELDX/17/, RFCTOL/32/,
     4     SCTOL/37/, STPPAR/5/, TUNER1/26/, TUNER2/27/, TUNER3/28/,
     5     XCTOL/33/, XFTOL/34/
C/7
C     PARAMETER (AFCTOL=31, DECFAC=22, DSTNRM=2, DST0=3, DSTSAV=18,
C    1           F=10, FDIF=11, FLSTGD=12, F0=13, GTSLST=14, GTSTEP=4,
C    2           INCFAC=23, LMAXS=36, NREDUC=6, PLSTGD=15, PREDUC=7,
C    3           RADFAC=16, RDFCMN=24, RDFCMX=25, RELDX=17, RFCTOL=32,
C    4           SCTOL=37, STPPAR=5, TUNER1=26, TUNER2=27, TUNER3=28,
C    5           XCTOL=33, XFTOL=34)
C/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C-----------------------------------------------------------------------
      NFC = IV(NFCALL)
      IV(SWITCH) = 0
      IV(RESTOR) = 0
      RFAC1 = ONE
      GOODX = .TRUE.
      I = IV(IRC)
      IF (I .GE. 1 .AND. I .LE. 12)
     1             GO TO (20,30,10,10,40,280,220,220,220,220,220,170), I
         IV(IRC) = 13
         GO TO 999
C
C  ***  INITIALIZE FOR NEW ITERATION  ***
C
 10   IV(STAGE) = 1
      IV(RADINC) = 0
      V(FLSTGD) = V(F0)
      IF (IV(TOOBIG) .EQ. 0) GO TO 110
         IV(STAGE) = -1
         IV(XIRC) = I
         GO TO 60
C
C  ***  STEP WAS RECOMPUTED WITH NEW MODEL OR SMALLER RADIUS  ***
C  ***  FIRST DECIDE WHICH  ***
C
 20   IF (IV(MODEL) .NE. IV(MLSTGD)) GO TO 30
C        ***  OLD MODEL RETAINED, SMALLER RADIUS TRIED  ***
C        ***  DO NOT CONSIDER ANY MORE NEW MODELS THIS ITERATION  ***
         IV(STAGE) = IV(STGLIM)
         IV(RADINC) = -1
         GO TO 110
C
C  ***  A NEW MODEL IS BEING TRIED.  DECIDE WHETHER TO KEEP IT.  ***
C
 30   IV(STAGE) = IV(STAGE) + 1
C
C     ***  NOW WE ADD THE POSSIBILTIY THAT STEP WAS RECOMPUTED WITH  ***
C     ***  THE SAME MODEL, PERHAPS BECAUSE OF AN OVERSIZED STEP.     ***
C
 40   IF (IV(STAGE) .GT. 0) GO TO 50
C
C        ***  STEP WAS RECOMPUTED BECAUSE IT WAS TOO BIG.  ***
C
         IF (IV(TOOBIG) .NE. 0) GO TO 60
C
C        ***  RESTORE IV(STAGE) AND PICK UP WHERE WE LEFT OFF.  ***
C
         IV(STAGE) = -IV(STAGE)
         I = IV(XIRC)
         GO TO (20, 30, 110, 110, 70), I
C
 50   IF (IV(TOOBIG) .EQ. 0) GO TO 70
C
C  ***  HANDLE OVERSIZE STEP  ***
C
      IF (IV(RADINC) .GT. 0) GO TO 80
         IV(STAGE) = -IV(STAGE)
         IV(XIRC) = IV(IRC)
C
 60      V(RADFAC) = V(DECFAC)
         IV(RADINC) = IV(RADINC) - 1
         IV(IRC) = 5
         IV(RESTOR) = 1
         GO TO 999
C
 70   IF (V(F) .LT. V(FLSTGD)) GO TO 110
C
C     *** THE NEW STEP IS A LOSER.  RESTORE OLD MODEL.  ***
C
      IF (IV(MODEL) .EQ. IV(MLSTGD)) GO TO 80
         IV(MODEL) = IV(MLSTGD)
         IV(SWITCH) = 1
C
C     ***  RESTORE STEP, ETC. ONLY IF A PREVIOUS STEP DECREASED V(F).
C
 80   IF (V(FLSTGD) .GE. V(F0)) GO TO 110
         IV(RESTOR) = 1
         V(F) = V(FLSTGD)
         V(PREDUC) = V(PLSTGD)
         V(GTSTEP) = V(GTSLST)
         IF (IV(SWITCH) .EQ. 0) RFAC1 = V(DSTNRM) / V(DSTSAV)
         V(DSTNRM) = V(DSTSAV)
         NFC = IV(NFGCAL)
         GOODX = .FALSE.
C
 110  V(FDIF) = V(F0) - V(F)
      IF (V(FDIF) .GT. V(TUNER2) * V(PREDUC)) GO TO 140
      IF(IV(RADINC).GT.0) GO TO 140
C
C        ***  NO (OR ONLY A TRIVIAL) FUNCTION DECREASE
C        ***  -- SO TRY NEW MODEL OR SMALLER RADIUS
C
         IF (V(F) .LT. V(F0)) GO TO 120
              IV(MLSTGD) = IV(MODEL)
              V(FLSTGD) = V(F)
              V(F) = V(F0)
              IV(RESTOR) = 1
              GO TO 130
 120     IV(NFGCAL) = NFC
 130     IV(IRC) = 1
         IF (IV(STAGE) .LT. IV(STGLIM)) GO TO 160
              IV(IRC) = 5
              IV(RADINC) = IV(RADINC) - 1
              GO TO 160
C
C  ***  NONTRIVIAL FUNCTION DECREASE ACHIEVED  ***
C
 140  IV(NFGCAL) = NFC
      RFAC1 = ONE
      V(DSTSAV) = V(DSTNRM)
      IF (V(FDIF) .GT. V(PREDUC)*V(TUNER1)) GO TO 190
C
C  ***  DECREASE WAS MUCH LESS THAN PREDICTED -- EITHER CHANGE MODELS
C  ***  OR ACCEPT STEP WITH DECREASED RADIUS.
C
      IF (IV(STAGE) .GE. IV(STGLIM)) GO TO 150
C        ***  CONSIDER SWITCHING MODELS  ***
         IV(IRC) = 2
         GO TO 160
C
C     ***  ACCEPT STEP WITH DECREASED RADIUS  ***
C
 150  IV(IRC) = 4
C
C  ***  SET V(RADFAC) TO FLETCHER*S DECREASE FACTOR  ***
C
 160  IV(XIRC) = IV(IRC)
      EMAX = V(GTSTEP) + V(FDIF)
      V(RADFAC) = HALF * RFAC1
      IF (EMAX .LT. V(GTSTEP)) V(RADFAC) = RFAC1 * MAX(V(RDFCMN),
     1                                           HALF * V(GTSTEP)/EMAX)
C
C  ***  DO FALSE CONVERGENCE TEST  ***
C
 170  IF (V(RELDX) .LE. V(XFTOL)) GO TO 180
         IV(IRC) = IV(XIRC)
         IF (V(F) .LT. V(F0)) GO TO 200
              GO TO 230
C
 180  IV(IRC) = 12
      GO TO 240
C
C  ***  HANDLE GOOD FUNCTION DECREASE  ***
C
 190  IF (V(FDIF) .LT. (-V(TUNER3) * V(GTSTEP))) GO TO 210
C
C     ***  INCREASING RADIUS LOOKS WORTHWHILE.  SEE IF WE JUST
C     ***  RECOMPUTED STEP WITH A DECREASED RADIUS OR RESTORED STEP
C     ***  AFTER RECOMPUTING IT WITH A LARGER RADIUS.
C
      IF (IV(RADINC) .LT. 0) GO TO 210
      IF (IV(RESTOR) .EQ. 1) GO TO 210
C
C        ***  WE DID NOT.  TRY A LONGER STEP UNLESS THIS WAS A NEWTON
C        ***  STEP.
C
         V(RADFAC) = V(RDFCMX)
         GTS = V(GTSTEP)
         IF (V(FDIF) .LT. (HALF/V(RADFAC) - ONE) * GTS)
     1            V(RADFAC) = MAX(V(INCFAC), HALF*GTS/(GTS + V(FDIF)))
         IV(IRC) = 4
         IF (V(STPPAR) .EQ. ZERO) GO TO 230
         IF (V(DST0) .GE. ZERO .AND. (V(DST0) .LT. TWO*V(DSTNRM)
     1             .OR. V(NREDUC) .LT. ONEP2*V(FDIF)))  GO TO 230
C             ***  STEP WAS NOT A NEWTON STEP.  RECOMPUTE IT WITH
C             ***  A LARGER RADIUS.
              IV(IRC) = 5
              IV(RADINC) = IV(RADINC) + 1
C
C  ***  SAVE VALUES CORRESPONDING TO GOOD STEP  ***
C
 200  V(FLSTGD) = V(F)
      IV(MLSTGD) = IV(MODEL)
      IF (IV(RESTOR) .NE. 1) IV(RESTOR) = 2
      V(DSTSAV) = V(DSTNRM)
      IV(NFGCAL) = NFC
      V(PLSTGD) = V(PREDUC)
      V(GTSLST) = V(GTSTEP)
      GO TO 230
C
C  ***  ACCEPT STEP WITH RADIUS UNCHANGED  ***
C
 210  V(RADFAC) = ONE
      IV(IRC) = 3
      GO TO 230
C
C  ***  COME HERE FOR A RESTART AFTER CONVERGENCE  ***
C
 220  IV(IRC) = IV(XIRC)
      IF (V(DSTSAV) .GE. ZERO) GO TO 240
         IV(IRC) = 12
         GO TO 240
C
C  ***  PERFORM CONVERGENCE TESTS  ***
C
 230  IV(XIRC) = IV(IRC)
 240  IF (IV(RESTOR) .EQ. 1 .AND. V(FLSTGD) .LT. V(F0)) IV(RESTOR) = 3
      IF (ABS(V(F)) .LT. V(AFCTOL)) IV(IRC) = 10
      IF (HALF * V(FDIF) .GT. V(PREDUC)) GO TO 999
      EMAX = V(RFCTOL) * ABS(V(F0))
      EMAXS = V(SCTOL) * ABS(V(F0))
      IF (V(DSTNRM) .GT. V(LMAXS) .AND. V(PREDUC) .LE. EMAXS)
     1                       IV(IRC) = 11
      IF (V(DST0) .LT. ZERO) GO TO 250
      I = 0
      IF ((V(NREDUC) .GT. ZERO .AND. V(NREDUC) .LE. EMAX) .OR.
     1    (V(NREDUC) .EQ. ZERO. AND. V(PREDUC) .EQ. ZERO))  I = 2
      IF (V(STPPAR) .EQ. ZERO .AND. V(RELDX) .LE. V(XCTOL)
     1                        .AND. GOODX)                  I = I + 1
      IF (I .GT. 0) IV(IRC) = I + 6
C
C  ***  CONSIDER RECOMPUTING STEP OF LENGTH V(LMAXS) FOR SINGULAR
C  ***  CONVERGENCE TEST.
C
 250  IF (IV(IRC) .GT. 5 .AND. IV(IRC) .NE. 12) GO TO 999
      IF (V(DSTNRM) .GT. V(LMAXS)) GO TO 260
         IF (V(PREDUC) .GE. EMAXS) GO TO 999
              IF (V(DST0) .LE. ZERO) GO TO 270
                   IF (HALF * V(DST0) .LE. V(LMAXS)) GO TO 999
                        GO TO 270
 260  IF (HALF * V(DSTNRM) .LE. V(LMAXS)) GO TO 999
      XMAX = V(LMAXS) / V(DSTNRM)
      IF (XMAX * (TWO - XMAX) * V(PREDUC) .GE. EMAXS) GO TO 999
 270  IF (V(NREDUC) .LT. ZERO) GO TO 290
C
C  ***  RECOMPUTE V(PREDUC) FOR USE IN SINGULAR CONVERGENCE TEST  ***
C
      V(GTSLST) = V(GTSTEP)
      V(DSTSAV) = V(DSTNRM)
      IF (IV(IRC) .EQ. 12) V(DSTSAV) = -V(DSTSAV)
      V(PLSTGD) = V(PREDUC)
      I = IV(RESTOR)
      IV(RESTOR) = 2
      IF (I .EQ. 3) IV(RESTOR) = 0
      IV(IRC) = 6
      GO TO 999
C
C  ***  PERFORM SINGULAR CONVERGENCE TEST WITH RECOMPUTED V(PREDUC)  ***
C
 280  V(GTSTEP) = V(GTSLST)
      V(DSTNRM) = ABS(V(DSTSAV))
      IV(IRC) = IV(XIRC)
      IF (V(DSTSAV) .LE. ZERO) IV(IRC) = 12
      V(NREDUC) = -V(PREDUC)
      V(PREDUC) = V(PLSTGD)
      IV(RESTOR) = 3
 290  IF (-V(NREDUC) .LE. V(RFCTOL) * ABS(V(F0))) IV(IRC) = 11
C
 999  RETURN
C
C  ***  LAST CARD OF ASSST FOLLOWS  ***
      END
      SUBROUTINE VDFLT(ALG, LV, V)
C-----------------------------------------------------------------------
C  ***  SUPPLY ***SOL (VERSION 2.3) DEFAULT VALUES TO V  ***
C
C  ***  ALG = 1 MEANS REGRESSION CONSTANTS.
C  ***  ALG = 2 MEANS GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS.
C
      INTEGER   LV
      INTEGER   ALG
      DOUBLE PRECISION V(LV)
C/+
C      DOUBLE PRECISION MAX
C/
      EXTERNAL RMDCON
      DOUBLE PRECISION RMDCON
C RMDCON... RETURNS MACHINE-DEPENDENT CONSTANTS
C
      DOUBLE PRECISION MACHEP, MEPCRT, ONE, SQTEPS, THREE
C
C  ***  SUBSCRIPTS FOR V  ***
C
      INTEGER   AFCTOL, BIAS, COSMIN, DECFAC, DELTA0, DFAC, DINIT,
     1        DLTFDC,DLTFDJ, DTINIT, D0INIT, EPSLON, ETA0, FUZZ, HUBERC,
     2        INCFAC, LMAX0, LMAXS, PHMNFC, PHMXFC, RDFCMN, RDFCMX,
     3        RFCTOL, RLIMIT, RSPTOL, SCTOL, SIGMIN, TUNER1, TUNER2,
     4        TUNER3, TUNER4, TUNER5, XCTOL, XFTOL
C
C/6
      DATA ONE/1.D+0/, THREE/3.D+0/
C/7
C     PARAMETER (ONE=1.D+0, THREE=3.D+0)
C/
C
C  ***  V SUBSCRIPT VALUES  ***
C
C/6
      DATA AFCTOL/31/, BIAS/43/, COSMIN/47/, DECFAC/22/, DELTA0/44/,
     1     DFAC/41/, DINIT/38/, DLTFDC/42/, DLTFDJ/43/, DTINIT/39/,
     2     D0INIT/40/, EPSLON/19/, ETA0/42/, FUZZ/45/, HUBERC/48/,
     3     INCFAC/23/, LMAX0/35/, LMAXS/36/, PHMNFC/20/, PHMXFC/21/,
     4     RDFCMN/24/, RDFCMX/25/, RFCTOL/32/, RLIMIT/46/, RSPTOL/49/,
     5     SCTOL/37/, SIGMIN/50/, TUNER1/26/, TUNER2/27/, TUNER3/28/,
     6     TUNER4/29/, TUNER5/30/, XCTOL/33/, XFTOL/34/
C/7
C     PARAMETER (AFCTOL=31, BIAS=43, COSMIN=47, DECFAC=22, DELTA0=44,
C    1           DFAC=41, DINIT=38, DLTFDC=42, DLTFDJ=43, DTINIT=39,
C    2           D0INIT=40, EPSLON=19, ETA0=42, FUZZ=45, HUBERC=48,
C    3           INCFAC=23, LMAX0=35, LMAXS=36, PHMNFC=20, PHMXFC=21,
C    4           RDFCMN=24, RDFCMX=25, RFCTOL=32, RLIMIT=46, RSPTOL=49,
C    5           SCTOL=37, SIGMIN=50, TUNER1=26, TUNER2=27, TUNER3=28,
C    6           TUNER4=29, TUNER5=30, XCTOL=33, XFTOL=34)
C/
C
C-------------------------------  BODY  --------------------------------
C
      MACHEP = RMDCON(3)
      V(AFCTOL) = 1.D-20
      IF (MACHEP .GT. 1.D-10) V(AFCTOL) = MACHEP**2
      V(DECFAC) = 0.5D+0
      SQTEPS = RMDCON(4)
      V(DFAC) = 0.6D+0
      V(DELTA0) = SQTEPS
      V(DTINIT) = 1.D-6
      MEPCRT = MACHEP ** (ONE/THREE)
      V(D0INIT) = 1.D+0
      V(EPSLON) = 0.1D+0
      V(INCFAC) = 2.D+0
      V(LMAX0) = 1.D+0
      V(LMAXS) = 1.D+0
      V(PHMNFC) = -0.1D+0
      V(PHMXFC) = 0.1D+0
      V(RDFCMN) = 0.1D+0
      V(RDFCMX) = 4.D+0
      V(RFCTOL) = MAX(1.D-10, MEPCRT**2)
      V(SCTOL) = V(RFCTOL)
      V(TUNER1) = 0.1D+0
      V(TUNER2) = 1.D-4
      V(TUNER3) = 0.75D+0
      V(TUNER4) = 0.5D+0
      V(TUNER5) = 0.75D+0
      V(XCTOL) = SQTEPS
      V(XFTOL) = 1.D+2 * MACHEP
C
      IF (ALG .GE. 2) GO TO 10
C
C  ***  REGRESSION  VALUES
C
      V(COSMIN) = MAX(1.D-6, 1.D+2 * MACHEP)
      V(DINIT) = 0.D+0
      V(DLTFDC) = MEPCRT
      V(DLTFDJ) = SQTEPS
      V(FUZZ) = 1.5D+0
      V(HUBERC) = 0.7D+0
      V(RLIMIT) = RMDCON(5)
      V(RSPTOL) = 1.D-3
      V(SIGMIN) = 1.D-4
      GO TO 999
C
C  ***  GENERAL OPTIMIZATION VALUES
C
 10   V(BIAS) = 0.8D+0
      V(DINIT) = -1.0D+0
      V(ETA0) = 1.0D+3 * MACHEP
C
 999  RETURN
C  ***  LAST CARD OF VDFLT FOLLOWS  ***
      END
      SUBROUTINE DBDOG(DIG, LV, N, NWTSTP, STEP, V)
C-----------------------------------------------------------------------
C  ***  COMPUTE DOUBLE DOGLEG STEP  ***
C
C  ***  PARAMETER DECLARATIONS  ***
C-----------------------------------------------------------------------
      INTEGER   LV
      INTEGER   N
      DOUBLE PRECISION DIG(*), NWTSTP(*), STEP(*), V(LV)
C
C  ***  PURPOSE  ***
C
C        THIS SUBROUTINE COMPUTES A CANDIDATE STEP (FOR USE IN AN UNCON-
C     STRAINED MINIMIZATION CODE) BY THE DOUBLE DOGLEG ALGORITHM OF
C     DENNIS AND MEI (REF. 1), WHICH IS A VARIATION ON POWELL*S DOGLEG
C     SCHEME (REF. 2, P. 95).
C
C--------------------------  PARAMETER USAGE  --------------------------
C
C    DIG (INPUT) DIAG(D)**-2 * G -- SEE ALGORITHM NOTES.
C      G (INPUT) THE CURRENT GRADIENT VECTOR.
C     LV (INPUT) LENGTH OF V.
C      N (INPUT) NUMBER OF COMPONENTS IN  DIG, G, NWTSTP,  AND  STEP.
C NWTSTP (INPUT) NEGATIVE NEWTON STEP -- SEE ALGORITHM NOTES.
C   STEP (OUTPUT) THE COMPUTED STEP.
C      V (I/O) VALUES ARRAY, THE FOLLOWING COMPONENTS OF WHICH ARE
C             USED HERE...
C V(BIAS)   (INPUT) BIAS FOR RELAXED NEWTON STEP, WHICH IS V(BIAS) OF
C             THE WAY FROM THE FULL NEWTON TO THE FULLY RELAXED NEWTON
C             STEP.  RECOMMENDED VALUE = 0.8 .
C V(DGNORM) (INPUT) 2-NORM OF DIAG(D)**-1 * G -- SEE ALGORITHM NOTES.
C V(DSTNRM) (OUTPUT) 2-NORM OF DIAG(D) * STEP, WHICH IS V(RADIUS)
C             UNLESS V(STPPAR) = 0 -- SEE ALGORITHM NOTES.
C V(DST0) (INPUT) 2-NORM OF DIAG(D) * NWTSTP -- SEE ALGORITHM NOTES.
C V(GRDFAC) (OUTPUT) THE COEFFICIENT OF  DIG  IN THE STEP RETURNED --
C             STEP(I) = V(GRDFAC)*DIG(I) + V(NWTFAC)*NWTSTP(I).
C V(GTHG)   (INPUT) SQUARE-ROOT OF (DIG**T) * (HESSIAN) * DIG -- SEE
C             ALGORITHM NOTES.
C V(GTSTEP) (OUTPUT) INNER PRODUCT BETWEEN G AND STEP.
C V(NREDUC) (OUTPUT) FUNCTION REDUCTION PREDICTED FOR THE FULL NEWTON
C             STEP.
C V(NWTFAC) (OUTPUT) THE COEFFICIENT OF  NWTSTP  IN THE STEP RETURNED --
C             SEE V(GRDFAC) ABOVE.
C V(PREDUC) (OUTPUT) FUNCTION REDUCTION PREDICTED FOR THE STEP RETURNED.
C V(RADIUS) (INPUT) THE TRUST REGION RADIUS.  D TIMES THE STEP RETURNED
C             HAS 2-NORM V(RADIUS) UNLESS V(STPPAR) = 0.
C V(STPPAR) (OUTPUT) CODE TELLING HOW STEP WAS COMPUTED... 0 MEANS A
C             FULL NEWTON STEP.  BETWEEN 0 AND 1 MEANS V(STPPAR) OF THE
C             WAY FROM THE NEWTON TO THE RELAXED NEWTON STEP.  BETWEEN
C             1 AND 2 MEANS A TRUE DOUBLE DOGLEG STEP, V(STPPAR) - 1 OF
C             THE WAY FROM THE RELAXED NEWTON TO THE CAUCHY STEP.
C             GREATER THAN 2 MEANS 1 / (V(STPPAR) - 1) TIMES THE CAUCHY
C             STEP.
C
C-------------------------------  NOTES  -------------------------------
C
C  ***  ALGORITHM NOTES  ***
C
C        LET  G  AND  H  BE THE CURRENT GRADIENT AND HESSIAN APPROXIMA-
C     TION RESPECTIVELY AND LET D BE THE CURRENT SCALE VECTOR.  THIS
C     ROUTINE ASSUMES DIG = DIAG(D)**-2 * G  AND  NWTSTP = H**-1 * G.
C     THE STEP COMPUTED IS THE SAME ONE WOULD GET BY REPLACING G AND H
C     BY  DIAG(D)**-1 * G  AND  DIAG(D)**-1 * H * DIAG(D)**-1,
C     COMPUTING STEP, AND TRANSLATING STEP BACK TO THE ORIGINAL
C     VARIABLES, I.E., PREMULTIPLYING IT BY DIAG(D)**-1.
C
C  ***  REFERENCES  ***
C
C 1.  DENNIS, J.E., AND MEI, H.H.W. (1979), TWO NEW UNCONSTRAINED OPTI-
C             MIZATION ALGORITHMS WHICH USE FUNCTION AND GRADIENT
C             VALUES, J. OPTIM. THEORY APPLIC. 28, PP. 453-482.
C 2. POWELL, M.J.D. (1970), A HYBRID METHOD FOR NON-LINEAR EQUATIONS,
C             IN NUMERICAL METHODS FOR NON-LINEAR EQUATIONS, EDITED BY
C             P. RABINOWITZ, GORDON AND BREACH, LONDON.
C
C  ***  GENERAL  ***
C
C     CODED BY DAVID M. GAY.
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED
C     BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS MCS-7600324 AND
C     MCS-7906671.
C
C------------------------  EXTERNAL QUANTITIES  ------------------------
C
C  ***  FUNCTIONS AND SUBROUTINES CALLED  ***
C
      EXTERNAL DOTPRD, V2NORM
      DOUBLE PRECISION DOTPRD, V2NORM
C
C DOTPRD... RETURNS INNER PRODUCT OF TWO VECTORS.
C V2NORM... RETURNS 2-NORM OF A VECTOR.
C
C  ***  INTRINSIC FUNCTIONS  ***
C/+
C      DOUBLE PRECISION SQRT
C/
C--------------------------  LOCAL VARIABLES  --------------------------
C
      INTEGER   I
      DOUBLE PRECISION CFACT, CNORM, CTRNWT, GHINVG, FEMNSQ, GNORM,
     1                 NWTNRM, RELAX, RLAMBD, T, T1, T2
      DOUBLE PRECISION HALF, ONE, TWO, ZERO
C
C  ***  V SUBSCRIPTS  ***
C
      INTEGER   BIAS, DGNORM, DSTNRM, DST0, GRDFAC, GTHG, GTSTEP,
     1        NREDUC, NWTFAC, PREDUC, RADIUS, STPPAR
C
C  ***  DATA INITIALIZATIONS  ***
C
C/6
      DATA HALF/0.5D+0/, ONE/1.D+0/, TWO/2.D+0/, ZERO/0.D+0/
C/7
C     PARAMETER (HALF=0.5D+0, ONE=1.D+0, TWO=2.D+0, ZERO=0.D+0)
C/
C
C/6
      DATA BIAS/43/, DGNORM/1/, DSTNRM/2/, DST0/3/, GRDFAC/45/,
     1     GTHG/44/, GTSTEP/4/, NREDUC/6/, NWTFAC/46/, PREDUC/7/,
     2     RADIUS/8/, STPPAR/5/
C/7
C     PARAMETER (BIAS=43, DGNORM=1, DSTNRM=2, DST0=3, GRDFAC=45,
C    1           GTHG=44, GTSTEP=4, NREDUC=6, NWTFAC=46, PREDUC=7,
C    2           RADIUS=8, STPPAR=5)
C/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      NWTNRM = V(DST0)
      RLAMBD = ONE
      IF (NWTNRM .GT. ZERO) RLAMBD = V(RADIUS) / NWTNRM
      GNORM = V(DGNORM)
      GHINVG = TWO * V(NREDUC)
      V(GRDFAC) = ZERO
      V(NWTFAC) = ZERO
      IF (RLAMBD .LT. ONE) GO TO 30
C
C        ***  THE NEWTON STEP IS INSIDE THE TRUST REGION  ***
C
         V(STPPAR) = ZERO
         V(DSTNRM) = NWTNRM
         V(GTSTEP) = -GHINVG
         V(PREDUC) = V(NREDUC)
         V(NWTFAC) = -ONE
         DO 20 I = 1, N
 20           STEP(I) = -NWTSTP(I)
         GO TO 999
C
 30   V(DSTNRM) = V(RADIUS)
      CFACT = (GNORM / V(GTHG))**2
C     ***  CAUCHY STEP = -CFACT * G.
      CNORM = GNORM * CFACT
      RELAX = ONE - V(BIAS) * (ONE - GNORM*CNORM/GHINVG)
      IF (RLAMBD .LT. RELAX) GO TO 50
C
C        ***  STEP IS BETWEEN RELAXED NEWTON AND FULL NEWTON STEPS  ***
C
         V(STPPAR)  =  ONE  -  (RLAMBD - RELAX) / (ONE - RELAX)
         T = -RLAMBD
         V(GTSTEP) = T * GHINVG
         V(PREDUC) = RLAMBD * (ONE - HALF*RLAMBD) * GHINVG
         V(NWTFAC) = T
         DO 40 I = 1, N
 40           STEP(I) = T * NWTSTP(I)
         GO TO 999
C
 50   IF (CNORM .LT. V(RADIUS)) GO TO 70
C
C        ***  THE CAUCHY STEP LIES OUTSIDE THE TRUST REGION --
C        ***  STEP = SCALED CAUCHY STEP  ***
C
         T = -V(RADIUS) / GNORM
         V(GRDFAC) = T
         V(STPPAR) = ONE  +  CNORM / V(RADIUS)
         V(GTSTEP) = -V(RADIUS) * GNORM
      V(PREDUC) = V(RADIUS)*(GNORM - HALF*V(RADIUS)*(V(GTHG)/GNORM)**2)
         DO 60 I = 1, N
 60           STEP(I) = T * DIG(I)
         GO TO 999
C
C     ***  COMPUTE DOGLEG STEP BETWEEN CAUCHY AND RELAXED NEWTON  ***
C     ***  FEMUR = RELAXED NEWTON STEP MINUS CAUCHY STEP  ***
C
 70   CTRNWT = CFACT * RELAX * GHINVG / GNORM
C     *** CTRNWT = INNER PROD. OF CAUCHY AND RELAXED NEWTON STEPS,
C     *** SCALED BY GNORM**-1.
      T1 = CTRNWT - GNORM*CFACT**2
C     ***  T1 = INNER PROD. OF FEMUR AND CAUCHY STEP, SCALED BY
C     ***  GNORM**-1.
      T2 = V(RADIUS)*(V(RADIUS)/GNORM) - GNORM*CFACT**2
      T = RELAX * NWTNRM
      FEMNSQ = (T/GNORM)*T - CTRNWT - T1
C     ***  FEMNSQ = SQUARE OF 2-NORM OF FEMUR, SCALED BY GNORM**-1.
      T = T2 / (T1 + SQRT(T1**2 + FEMNSQ*T2))
C     ***  DOGLEG STEP  =  CAUCHY STEP  +  T * FEMUR.
      T1 = (T - ONE) * CFACT
      V(GRDFAC) = T1
      T2 = -T * RELAX
      V(NWTFAC) = T2
      V(STPPAR) = TWO - T
      V(GTSTEP) = T1*GNORM**2 + T2*GHINVG
      V(PREDUC) = -T1*GNORM * ((T2 + ONE)*GNORM)
     1                 - T2 * (ONE + HALF*T2)*GHINVG
     2                  - HALF * (V(GTHG)*T1)**2
      DO 80 I = 1, N
 80      STEP(I) = T1*DIG(I) + T2*NWTSTP(I)
C
 999  RETURN
C  ***  LAST LINE OF DBDOG FOLLOWS  ***
      END
      DOUBLE PRECISION FUNCTION DOTPRD(P, X, Y)
C-----------------------------------------------------------------------
C  ***  RETURN THE INNER PRODUCT OF THE P-VECTORS X AND Y.  ***
C-----------------------------------------------------------------------
      INTEGER   P
      DOUBLE PRECISION X(*), Y(*)
C
      INTEGER   I
      DOUBLE PRECISION ONE, SQTETA, T, ZERO
C/+
C      DOUBLE PRECISION MAX, ABS
C/
      EXTERNAL RMDCON
      DOUBLE PRECISION RMDCON
C
C  ***  RMDCON(2) RETURNS A MACHINE-DEPENDENT CONSTANT, SQTETA, WHICH
C  ***  IS SLIGHTLY LARGER THAN THE SMALLEST POSITIVE NUMBER THAT
C  ***  CAN BE SQUARED WITHOUT UNDERFLOWING.
C
C/6
      DATA ONE/1.D+0/, SQTETA/0.D+0/, ZERO/0.D+0/
C/7
C     PARAMETER (ONE=1.D+0, ZERO=0.D+0)
C     DATA SQTETA/0.D+0/
C/
C-----------------------------------------------------------------------
      DOTPRD = ZERO
      IF (P .LE. 0) GO TO 999
      IF (SQTETA .EQ. ZERO) SQTETA = RMDCON(2)
      DO 20 I = 1, P
         T = MAX(ABS(X(I)), ABS(Y(I)))
         IF (T .GT. ONE) GO TO 10
         IF (T .LT. SQTETA) GO TO 20
         T = (X(I)/SQTETA)*Y(I)
         IF (ABS(T) .LT. SQTETA) GO TO 20
 10      DOTPRD = DOTPRD + X(I)*Y(I)
 20   CONTINUE
C
 999  RETURN
C  ***  LAST CARD OF DOTPRD FOLLOWS  ***
      END
      SUBROUTINE LITVMU(N, X, L, Y)
C-----------------------------------------------------------------------
C  ***  SOLVE  (L**T)*X = Y,  WHERE  L  IS AN  N X N  LOWER TRIANGULAR
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME
C  ***  STORAGE.  ***
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION X(*), L(*), Y(*)
      INTEGER   I, II, IJ, IM1, I0, J, NP1
      DOUBLE PRECISION XI, ZERO
C/6
      DATA ZERO/0.D+0/
C/7
C     PARAMETER (ZERO=0.D+0)
C/
C-----------------------------------------------------------------------
      DO 10 I = 1, N
 10      X(I) = Y(I)
      NP1 = N + 1
      I0 = N*(N+1)/2
      DO 30 II = 1, N
         I = NP1 - II
         XI = X(I)/L(I0)
         X(I) = XI
         IF (I .LE. 1) GO TO 999
         I0 = I0 - I
         IF (XI .EQ. ZERO) GO TO 30
         IM1 = I - 1
         DO 20 J = 1, IM1
              IJ = I0 + J
              X(J) = X(J) - XI*L(IJ)
 20           CONTINUE
 30      CONTINUE
 999  RETURN
C  ***  LAST CARD OF LITVMU FOLLOWS  ***
      END
      SUBROUTINE LIVMUL(N, X, L, Y)
C-----------------------------------------------------------------------
C  ***  SOLVE  L*X = Y, WHERE  L  IS AN  N X N  LOWER TRIANGULAR
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME
C  ***  STORAGE.  ***
C
      INTEGER   N
      DOUBLE PRECISION X(*), L(*), Y(*)
      EXTERNAL DOTPRD
      DOUBLE PRECISION DOTPRD
      INTEGER   I, J, K, ITEMP
      DOUBLE PRECISION T, ZERO
C/6
      DATA ZERO/0.D+0/
C/7
C     PARAMETER (ZERO=0.D+0)
C/
C-----------------------------------------------------------------------
      DO 10 K = 1, N
         IF (Y(K) .NE. ZERO) GO TO 20
         X(K) = ZERO
 10      CONTINUE
      GO TO 999
 20   J = K*(K+1)/2
      X(K) = Y(K) / L(J)
      IF (K .GE. N) GO TO 999
      K = K + 1
      DO 30 I = K, N
         ITEMP = I-1
         T = DOTPRD(ITEMP, L(J+1), X)
         J = J + I
         X(I) = (Y(I) - T)/L(J)
 30      CONTINUE
 999  RETURN
C  ***  LAST CARD OF LIVMUL FOLLOWS  ***
      END
      SUBROUTINE LTVMUL(N, X, L, Y)
C-----------------------------------------------------------------------
C  ***  COMPUTE  X = (L**T)*Y, WHERE  L  IS AN  N X N  LOWER
C  ***  TRIANGULAR MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY
C  ***  OCCUPY THE SAME STORAGE.  ***
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION X(*), L(*), Y(*)
C     DIMENSION L(N*(N+1)/2)
      INTEGER   I, IJ, I0, J
      DOUBLE PRECISION YI, ZERO
C/6
      DATA ZERO/0.D+0/
C/7
C     PARAMETER (ZERO=0.D+0)
C/
C-----------------------------------------------------------------------
      I0 = 0
      DO 20 I = 1, N
         YI = Y(I)
         X(I) = ZERO
         DO 10 J = 1, I
              IJ = I0 + J
              X(J) = X(J) + YI*L(IJ)
 10           CONTINUE
         I0 = I0 + I
 20      CONTINUE
 999  RETURN
C  ***  LAST CARD OF LTVMUL FOLLOWS  ***
      END
      SUBROUTINE LUPDAT(BETA, GAMMA, L, LAMBDA, LPLUS, N, W, Z)
C-----------------------------------------------------------------------
C  ***  COMPUTE LPLUS = SECANT UPDATE OF L  ***
C
C  ***  PARAMETER DECLARATIONS  ***
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION BETA(*), GAMMA(*), L(*), LAMBDA(*), LPLUS(*),
     1                 W(*), Z(*)
C     DIMENSION L(N*(N+1)/2), LPLUS(N*(N+1)/2)
C
C--------------------------  PARAMETER USAGE  --------------------------
C
C   BETA = SCRATCH VECTOR.
C  GAMMA = SCRATCH VECTOR.
C      L (INPUT) LOWER TRIANGULAR MATRIX, STORED ROWWISE.
C LAMBDA = SCRATCH VECTOR.
C  LPLUS (OUTPUT) LOWER TRIANGULAR MATRIX, STORED ROWWISE, WHICH MAY
C             OCCUPY THE SAME STORAGE AS  L.
C      N (INPUT) LENGTH OF VECTOR PARAMETERS AND ORDER OF MATRICES.
C      W (INPUT, DESTROYED ON OUTPUT) RIGHT SINGULAR VECTOR OF RANK 1
C             CORRECTION TO  L.
C      Z (INPUT, DESTROYED ON OUTPUT) LEFT SINGULAR VECTOR OF RANK 1
C             CORRECTION TO  L.
C
C-------------------------------  NOTES  -------------------------------
C
C  ***  APPLICATION AND USAGE RESTRICTIONS  ***
C
C        THIS ROUTINE UPDATES THE CHOLESKY FACTOR  L  OF A SYMMETRIC
C     POSITIVE DEFINITE MATRIX TO WHICH A SECANT UPDATE IS BEING
C     APPLIED -- IT COMPUTES A CHOLESKY FACTOR  LPLUS  OF
C     L * (I + Z*W**T) * (I + W*Z**T) * L**T.  IT IS ASSUMED THAT  W
C     AND  Z  HAVE BEEN CHOSEN SO THAT THE UPDATED MATRIX IS STRICTLY
C     POSITIVE DEFINITE.
C
C  ***  ALGORITHM NOTES  ***
C
C        THIS CODE USES RECURRENCE 3 OF REF. 1 (WITH D(J) = 1 FOR ALL J)
C     TO COMPUTE  LPLUS  OF THE FORM  L * (I + Z*W**T) * Q,  WHERE  Q
C     IS AN ORTHOGONAL MATRIX THAT MAKES THE RESULT LOWER TRIANGULAR.
C        LPLUS MAY HAVE SOME NEGATIVE DIAGONAL ELEMENTS.
C
C  ***  REFERENCES  ***
C
C 1.  GOLDFARB, D. (1976), FACTORIZED VARIABLE METRIC METHODS FOR UNCON-
C             STRAINED OPTIMIZATION, MATH. COMPUT. 30, PP. 796-811.
C
C  ***  GENERAL  ***
C
C     CODED BY DAVID M. GAY (FALL 1979).
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED
C     BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS MCS-7600324 AND
C     MCS-7906671.
C
C------------------------  EXTERNAL QUANTITIES  ------------------------
C
C  ***  INTRINSIC FUNCTIONS  ***
C/+
C      DOUBLE PRECISION SQRT
C/
C--------------------------  LOCAL VARIABLES  --------------------------
C
      INTEGER   I, IJ, J, JJ, JP1, K, NM1, NP1
      DOUBLE PRECISION A, B, BJ, ETA, GJ, LJ, LIJ, LJJ, NU, S, THETA,
     1                 WJ, ZJ
      DOUBLE PRECISION ONE, ZERO
C
C  ***  DATA INITIALIZATIONS  ***
C
C/6
      DATA ONE/1.D+0/, ZERO/0.D+0/
C/7
C     PARAMETER (ONE=1.D+0, ZERO=0.D+0)
C/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      NU = ONE
      ETA = ZERO
      IF (N .LE. 1) GO TO 30
      NM1 = N - 1
C
C  ***  TEMPORARILY STORE S(J) = SUM OVER K = J+1 TO N OF W(K)**2 IN
C  ***  LAMBDA(J).
C
      S = ZERO
      DO 10 I = 1, NM1
         J = N - I
         S = S + W(J+1)**2
         LAMBDA(J) = S
 10      CONTINUE
C
C  ***  COMPUTE LAMBDA, GAMMA, AND BETA BY GOLDFARB*S RECURRENCE 3.
C
      DO 20 J = 1, NM1
         WJ = W(J)
         A = NU*Z(J) - ETA*WJ
         THETA = ONE + A*WJ
         S = A*LAMBDA(J)
         LJ = SQRT(THETA**2 + A*S)
         IF (THETA .GT. ZERO) LJ = -LJ
         LAMBDA(J) = LJ
         B = THETA*WJ + S
         GAMMA(J) = B * NU / LJ
         BETA(J) = (A - B*ETA) / LJ
         NU = -NU / LJ
         ETA = -(ETA + (A**2)/(THETA - LJ)) / LJ
 20      CONTINUE
 30   LAMBDA(N) = ONE + (NU*Z(N) - ETA*W(N))*W(N)
C
C  ***  UPDATE L, GRADUALLY OVERWRITING  W  AND  Z  WITH  L*W  AND  L*Z.
C
      NP1 = N + 1
      JJ = N * (N + 1) / 2
      DO 60 K = 1, N
         J = NP1 - K
         LJ = LAMBDA(J)
         LJJ = L(JJ)
         LPLUS(JJ) = LJ * LJJ
         WJ = W(J)
         W(J) = LJJ * WJ
         ZJ = Z(J)
         Z(J) = LJJ * ZJ
         IF (K .EQ. 1) GO TO 50
         BJ = BETA(J)
         GJ = GAMMA(J)
         IJ = JJ + J
         JP1 = J + 1
         DO 40 I = JP1, N
              LIJ = L(IJ)
              LPLUS(IJ) = LJ*LIJ + BJ*W(I) + GJ*Z(I)
              W(I) = W(I) + LIJ*WJ
              Z(I) = Z(I) + LIJ*ZJ
              IJ = IJ + I
 40           CONTINUE
 50      JJ = JJ - J
 60      CONTINUE
C
 999  RETURN
C  ***  LAST CARD OF LUPDAT FOLLOWS  ***
      END
      SUBROUTINE LVMUL(N, X, L, Y)
C-----------------------------------------------------------------------
C  ***  COMPUTE  X = L*Y, WHERE  L  IS AN  N X N  LOWER TRIANGULAR
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME
C  ***  STORAGE.  ***
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION X(*), L(*), Y(*)
C     DIMENSION L(N*(N+1)/2)
      INTEGER   I, II, IJ, I0, J, NP1
      DOUBLE PRECISION T, ZERO
C/6
      DATA ZERO/0.D+0/
C/7
C     PARAMETER (ZERO=0.D+0)
C/
C-----------------------------------------------------------------------
      NP1 = N + 1
      I0 = N*(N+1)/2
      DO 20 II = 1, N
         I = NP1 - II
         I0 = I0 - I
         T = ZERO
         DO 10 J = 1, I
              IJ = I0 + J
              T = T + L(IJ)*Y(J)
 10           CONTINUE
         X(I) = T
 20      CONTINUE
 999  RETURN
C  ***  LAST CARD OF LVMUL FOLLOWS  ***
      END
      SUBROUTINE PARCK(ALG, D, IV, LIV, LV, N, V)
C-----------------------------------------------------------------------
C  ***  CHECK ***SOL (VERSION 2.3) PARAMETERS, PRINT CHANGED VALUES  ***
C
C  ***  ALG = 1 FOR REGRESSION, ALG = 2 FOR GENERAL UNCONSTRAINED OPT.
C-----------------------------------------------------------------------
      INTEGER   LIV, LV
      INTEGER   ALG, N
      INTEGER   IV(LIV)
      DOUBLE PRECISION D(*), V(LV)
C
      EXTERNAL RMDCON, VCOPY, VDFLT
      DOUBLE PRECISION RMDCON
C RMDCON -- RETURNS MACHINE-DEPENDENT CONSTANTS.
C VCOPY  -- COPIES ONE VECTOR TO ANOTHER.
C VDFLT  -- SUPPLIES DEFAULT PARAMETER VALUES TO V ALONE.
C/+
C      INTEGER   MAX
C/
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER   I, II, IV1, J, K, L, M, MIV1, MIV2, NDFALT, PARSV1, PU
      INTEGER   IJMP, JLIM(2), MINIV(2), NDFLT(2)
C/6
C/7
      CHARACTER*1 VARNM(2), SH(2)
      CHARACTER*12 CNGD, DFLT, WHICH
      CHARACTER*8 VN(34)
C/
      DOUBLE PRECISION BIG, MACHEP, TINY, VK, VM(34), VX(34), ZERO
C
C  ***  IV AND V SUBSCRIPTS  ***
C
      INTEGER   ALGSAV, DINIT, DTYPE, DTYPE0, EPSLON, INITS, IVNEED,
     1        LASTIV, LASTV, LMAT, NEXTIV, NEXTV, NVDFLT, OLDN,
     2        PARPRT, PARSAV, PERM, PRUNIT, VNEED
C
C
C/6
      INTEGER   ITEMP
      INCLUDE 'INCS:DMSG.INC'
      DATA ALGSAV/51/, DINIT/38/, DTYPE/16/, DTYPE0/54/, EPSLON/19/,
     1     INITS/25/, IVNEED/3/, LASTIV/44/, LASTV/45/, LMAT/42/,
     2     NEXTIV/46/, NEXTV/47/, NVDFLT/50/, OLDN/38/, PARPRT/20/,
     3     PARSAV/49/, PERM/58/, PRUNIT/21/, VNEED/4/
C/7
C     PARAMETER (ALGSAV=51, DINIT=38, DTYPE=16, DTYPE0=54, EPSLON=19,
C    1           INITS=25, IVNEED=3, LASTIV=44, LASTV=45, LMAT=42,
C    2           NEXTIV=46, NEXTV=47, NVDFLT=50, OLDN=38, PARPRT=20,
C    3           PARSAV=49, PERM=58, PRUNIT=21, VNEED=4)
C     SAVE BIG, MACHEP, TINY
C/
C
      DATA BIG/0.D+0/, MACHEP/-1.D+0/, TINY/1.D+0/, ZERO/0.D+0/
C/6
      DATA VN(1)/'EPSLON..'/
      DATA VN(2)/'PHMNFC..'/
      DATA VN(3)/'PHMXFC..'/
      DATA VN(4)/'DECFAC..'/
      DATA VN(5)/'INCFAC..'/
      DATA VN(6)/'RDFCMN..'/
      DATA VN(7)/'RDFCMX..'/
      DATA VN(8)/'TUNER1..'/
      DATA VN(9)/'TUNER2..'/
      DATA VN(10)/'TUNER3..'/
      DATA VN(11)/'TUNER4..'/
      DATA VN(12)/'TUNER5..'/
      DATA VN(13)/'AFCTOL..'/
      DATA VN(14)/'RFCTOL..'/
      DATA VN(15)/'XCTOL...'/
      DATA VN(16)/'XFTOL...'/
      DATA VN(17)/'LMAX0...'/
      DATA VN(18)/'LMAXS...'/
      DATA VN(19)/'SCTOL...'/
      DATA VN(20)/'DINIT...'/
      DATA VN(21)/'DTINIT..'/
      DATA VN(22)/'D0INIT..'/
      DATA VN(23)/'DFAC....'/
      DATA VN(24)/'DLTFDC..'/
      DATA VN(25)/'DLTFDJ..'/
      DATA VN(26)/'DELTA0..'/
      DATA VN(27)/'FUZZ....'/
      DATA VN(28)/'RLIMIT..'/
      DATA VN(29)/'COSMIN..'/
      DATA VN(30)/'HUBERC..'/
      DATA VN(31)/'RSPTOL..'/
      DATA VN(32)/'SIGMIN..'/
      DATA VN(33)/'ETA0....'/
      DATA VN(34)/'BIAS....'/
C
      DATA VM(1)/1.0D-3/, VM(2)/-0.99D+0/, VM(3)/1.0D-3/, VM(4)/1.0D-2/,
     1     VM(5)/1.2D+0/, VM(6)/1.D-2/, VM(7)/1.2D+0/, VM(8)/0.D+0/,
     2     VM(9)/0.D+0/, VM(10)/1.D-3/, VM(11)/-1.D+0/, VM(13)/0.D+0/,
     3     VM(15)/0.D+0/, VM(16)/0.D+0/, VM(19)/0.D+0/, VM(20)/-10.D+0/,
     4     VM(21)/0.D+0/, VM(22)/0.D+0/, VM(23)/0.D+0/, VM(27)/1.01D+0/,
     5     VM(28)/1.D+10/, VM(30)/0.D+0/, VM(31)/0.D+0/, VM(32)/0.D+0/,
     6     VM(34)/0.D+0/
      DATA VX(1)/0.9D+0/, VX(2)/-1.D-3/, VX(3)/1.D+1/, VX(4)/0.8D+0/,
     1     VX(5)/1.D+2/, VX(6)/0.8D+0/, VX(7)/1.D+2/, VX(8)/0.5D+0/,
     2     VX(9)/0.5D+0/, VX(10)/1.D+0/, VX(11)/1.D+0/, VX(14)/0.1D+0/,
     3     VX(15)/1.D+0/, VX(16)/1.D+0/, VX(19)/1.D+0/, VX(23)/1.D+0/,
     4     VX(24)/1.D+0/, VX(25)/1.D+0/, VX(26)/1.D+0/, VX(27)/1.D+10/,
     5     VX(29)/1.D+0/, VX(31)/1.D+0/, VX(32)/1.D+0/, VX(33)/1.D+0/,
     6     VX(34)/1.D+0/
C
C/6
C/7
      DATA VARNM(1)/'P'/, VARNM(2)/'N'/, SH(1)/'S'/, SH(2)/'H'/
      DATA CNGD /'---CHANGED V'/, DFLT /'NONDEFAULT V'/
C/
      DATA IJMP/33/, JLIM(1)/0/, JLIM(2)/24/, NDFLT(1)/32/, NDFLT(2)/25/
      DATA MINIV(1)/80/, MINIV(2)/59/
C
C...............................  BODY  ................................
C-----------------------------------------------------------------------
      PU = 0
      IF (PRUNIT .LE. LIV) PU = IV(PRUNIT)
      IF (ALG .LT. 1 .OR. ALG .GT. 2) GO TO 340
      IF (IV(1) .EQ. 0) CALL DEFLT(ALG, IV, LIV, LV, V)
      IV1 = IV(1)
      IF (IV1 .NE. 13 .AND. IV1 .NE. 12) GO TO 10
      MIV1 = MINIV(ALG)
      ITEMP = IV(PERM) - 1
      IF (PERM .LE. LIV) MIV1 = MAX(MIV1, ITEMP)
      IF (IVNEED .LE. LIV) MIV2 = MIV1 + MAX(IV(IVNEED), 0)
      IF (LASTIV .LE. LIV) IV(LASTIV) = MIV2
      IF (LIV .LT. MIV1) GO TO 300
      IV(IVNEED) = 0
      IV(LASTV) = MAX(IV(VNEED), 0) + IV(LMAT) - 1
      IV(VNEED) = 0
      IF (LIV .LT. MIV2) GO TO 300
      IF (LV .LT. IV(LASTV)) GO TO 320
 10   IF (ALG .EQ. IV(ALGSAV)) GO TO 30
C         IF (PU .NE. 0) WRITE(PU,20) ALG, IV(ALGSAV)
C                                       AIPS mod.
          IF (PU .NE.0) THEN
             WRITE (MSGTXT,20) ALG, IV(ALGSAV)
             CALL MSGWRT (6)
          ENDIF
 20      FORMAT(' THE FIRST PARAMETER TO DEFLT SHOULD BE ',I3,
C 20      FORMAT(39H THE FIRST PARAMETER TO DEFLT SHOULD BE,I3,
     1          ' RATHER THAN ',I3)
         IV(1) = 82
         GO TO 999
 30   IF (IV1 .LT. 12 .OR. IV1 .GT. 14) GO TO 60
         IF (N .GE. 1) GO TO 50
              IV(1) = 81
              IF (PU .EQ. 0) GO TO 999
C                                       AIPS mod
C              WRITE(PU,40) VARNM(ALG), N
              WRITE (MSGTXT,40) VARNM(ALG), N
              CALL MSGWRT (6)
C 40           FORMAT(/8H /// BAD,A1,2H =,I5)
 40           FORMAT(' *** BAD ',A1,2H =,I5)
              GO TO 999
 50      IF (IV1 .NE. 14) IV(NEXTIV) = IV(PERM)
         IF (IV1 .NE. 14) IV(NEXTV) = IV(LMAT)
         IF (IV1 .EQ. 13) GO TO 999
         K = IV(PARSAV) - EPSLON
         ITEMP = LV - K
         CALL VDFLT(ALG, ITEMP, V(K+1))
         IV(DTYPE0) = 2 - ALG
         IV(OLDN) = N
         WHICH = DFLT
         GO TO 110
 60   IF (N .EQ. IV(OLDN)) GO TO 80
         IV(1) = 17
         IF (PU .EQ. 0) GO TO 999
C                                       AIPS mod
C         WRITE(PU,70) VARNM(ALG), IV(OLDN), N
C 70      FORMAT(/5H /// ,1A1,14H CHANGED FROM ,I5,4H TO ,I5)
         WRITE (MSGTXT,70) VARNM(ALG), IV(OLDN), N
         CALL MSGWRT (6)
 70      FORMAT(' *** ',1A1,' CHANGED FROM ',I5,' TO ',I5)
         GO TO 999
C
 80   IF (IV1 .LE. 11 .AND. IV1 .GE. 1) GO TO 100
         IV(1) = 80
C                                       AIPS mod
C         IF (PU .NE. 0) WRITE(PU,90) IV1
C 90      FORMAT(/13H ///  IV(1) =,I5,28H SHOULD BE BETWEEN 0 AND 14.)
         IF (PU .NE. 0) THEN
            WRITE (MSGTXT,90) IV1
            CALL MSGWRT (6)
            ENDIF
 90      FORMAT(' ***  IV(1) =',I5,' SHOULD BE BETWEEN 0 AND 14.')
         GO TO 999
C
 100  WHICH = CNGD
C
 110  IF (IV1 .EQ. 14) IV1 = 12
      IF (BIG .GT. TINY) GO TO 120
         TINY = RMDCON(1)
         MACHEP = RMDCON(3)
         BIG = RMDCON(6)
         VM(12) = MACHEP
         VX(12) = BIG
         VX(13) = BIG
         VM(14) = MACHEP
         VM(17) = TINY
         VX(17) = BIG
         VM(18) = TINY
         VX(18) = BIG
         VX(20) = BIG
         VX(21) = BIG
         VX(22) = BIG
         VM(24) = MACHEP
         VM(25) = MACHEP
         VM(26) = MACHEP
         VX(28) = RMDCON(5)
         VM(29) = MACHEP
         VX(30) = BIG
         VM(33) = MACHEP
 120  M = 0
      I = 1
      J = JLIM(ALG)
      K = EPSLON
      NDFALT = NDFLT(ALG)
      DO 150 L = 1, NDFALT
         VK = V(K)
         IF (VK .GE. VM(I) .AND. VK .LE. VX(I)) GO TO 140
              M = K
C                                       AIPS mod
C              IF (PU .NE. 0) WRITE(PU,130) VN(1,I), VN(2,I), K, VK,
C     1                                    VM(I), VX(I)
C 130          FORMAT(/6H ///  ,2A4,5H.. V(,I2,3H) =,D11.3,7H SHOULD,
C     1               11H BE BETWEEN,D11.3,4H AND,D11.3)
              IF (PU .NE. 0) THEN
                 WRITE (MSGTXT,130) VN(I), K, VK, VM(I), VX(I)
                 CALL MSGWRT (6)
                 END IF
 130          FORMAT(' ***  ',A,'.. V(',I2,') =',D11.3,' SHOULD',
     1               ' BE BETWEEN',D11.3,' AND ',D11.3)
 140     K = K + 1
         I = I + 1
         IF (I .EQ. J) I = IJMP
 150     CONTINUE
C
      IF (IV(NVDFLT) .EQ. NDFALT) GO TO 170
         IV(1) = 51
         IF (PU .EQ. 0) GO TO 999
C                                       AIPS mod
C         WRITE(PU,160) IV(NVDFLT), NDFALT
C 160     FORMAT(/13H IV(NVDFLT) =,I5,13H RATHER THAN ,I5)
         WRITE (MSGTXT,160)  IV(NVDFLT), NDFALT
         CALL MSGWRT (6)
 160     FORMAT(' IV(NVDFLT) =',I5,' RATHER THAN ',I5)
         GO TO 999
 170  IF ((IV(DTYPE) .GT. 0 .OR. V(DINIT) .GT. ZERO) .AND. IV1 .EQ. 12)
     1                  GO TO 200
      DO 190 I = 1, N
         IF (D(I) .GT. ZERO) GO TO 190
              M = 18
C                                       AIPS mod.
C              IF (PU .NE. 0) WRITE(PU,180) I, D(I)
C 180     FORMAT(/8H ///  D(,I3,3H) =,D11.3,19H SHOULD BE POSITIVE)
               IF (PU .NE. 0) THEN
                  WRITE(MSGTXT,180) I, D(I)
                  CALL MSGWRT (6)
                  END IF
 180     FORMAT(' ***  D(',I3,') =',D11.3,' SHOULD BE POSITIVE')
 190     CONTINUE
 200  IF (M .EQ. 0) GO TO 210
         IV(1) = M
         GO TO 999
C
 210  IF (IV(PARPRT) .EQ. 0) GO TO 999
      IF (IV1 .NE. 12 .OR. IV(INITS) .EQ. ALG-1) GO TO 230
         M = 1
C                                       AIPS mod
C         WRITE(PU,220) SH(ALG), IV(INITS)
C 220   FORMAT(/22H NONDEFAULT VALUES..../5H INIT,A1,14H..... IV(25) =,
         IF (PU .NE. 0) THEN
            WRITE (MSGTXT,220) SH(ALG), IV(INITS)
            CALL MSGWRT (6)
            END IF
 220     FORMAT(' Nondefault values.... INIT ',A1,'H..... IV(25) =',
     1          I3)
 230  IF (IV(DTYPE) .EQ. IV(DTYPE0)) GO TO 250
C                                       AIPS mod
C         IF (M .EQ. 0) WRITE(PU,260) WHICH
         IF ((M.EQ.0) .AND. (PU.NE.0)) WRITE (MSGTXT,260) WHICH
         IF ((M.EQ.0) .AND. (PU.NE.0)) CALL MSGWRT (6)
         M = 1
C         WRITE(PU,240) IV(DTYPE)
         WRITE (MSGTXT,240) IV(DTYPE)
         CALL MSGWRT (6)
 240     FORMAT(' DTYPE..... IV(16) =',I3)
 250  I = 1
      J = JLIM(ALG)
      K = EPSLON
      L = IV(PARSAV)
      NDFALT = NDFLT(ALG)
      DO 290 II = 1, NDFALT
         IF (V(K) .EQ. V(L)) GO TO 280
C                                       AIPS mod
C              IF (M .EQ. 0) WRITE(PU,260) WHICH
C 260          FORMAT(/1H ,3A4,9HALUES..../)
              IF ((M.EQ.0) .AND. (PU.NE.0)) WRITE (MSGTXT,260) WHICH
              IF ((M.EQ.0) .AND. (PU.NE.0)) CALL MSGWRT (6)
 260          FORMAT(' ',A,'ALUES.... ')
              M = 1
C                                       AIPS mod
C              WRITE(PU,270) VN(1,I), VN(2,I), K, V(K)
              IF (PU .NE. 0) THEN
                 WRITE (MSGTXT,270) VN(I), K, V(K)
                 CALL MSGWRT (6)
                 END IF
 270          FORMAT(1X,A,'.. V(',I2,') =',D15.7)
 280     K = K + 1
         L = L + 1
         I = I + 1
         IF (I .EQ. J) I = IJMP
 290     CONTINUE
C
      IV(DTYPE0) = IV(DTYPE)
      PARSV1 = IV(PARSAV)
      CALL VCOPY(IV(NVDFLT), V(PARSV1), V(EPSLON))
      GO TO 999
C
 300  IV(1) = 15
      IF (PU .EQ. 0) GO TO 999
C                                       AIPS mod
C      WRITE(PU,310) LIV, MIV2
C 310  FORMAT(/10H /// LIV =,I5,17H MUST BE AT LEAST,I5)
      WRITE (MSGTXT,310) LIV, MIV2
      CALL MSGWRT (6)
 310  FORMAT(' *** LIV =',I5,' MUST BE AT LEAST',I5)
      IF (LIV .LT. MIV1) GO TO 999
      IF (LV .LT. IV(LASTV)) GO TO 320
      GO TO 999
C
 320  IV(1) = 16
      IF (PU .EQ. 0) GO TO 999
C                                       AIPS mod
C      WRITE(PU,330) LV, IV(LASTV)
C 330  FORMAT(/9H /// LV =,I5,17H MUST BE AT LEAST,I5)
      WRITE (MSGTXT,330) LV, IV(LASTV)
      CALL MSGWRT (6)
 330  FORMAT(' *** LV =',I5,' MUST BE AT LEAST',I5)
      GO TO 999
C
 340  IV(1) = 67
      IF (PU .EQ. 0) GO TO 999
C                                       AIPS mod
C      WRITE(PU,350) ALG
C 350  FORMAT(/10H /// ALG =,I5,15H MUST BE 1 OR 2)
      WRITE (MSGTXT,350) ALG
      CALL MSGWRT (6)
 350  FORMAT(' *** ALG =',I5,' MUST BE 1 OR 2')
C
 999  RETURN
C  ***  LAST CARD OF PARCK FOLLOWS  ***
      END
      DOUBLE PRECISION FUNCTION RELDST(P, D, X, X0)
C
C  ***  COMPUTE AND RETURN RELATIVE DIFFERENCE BETWEEN X AND X0  ***
C  ***  NL2SOL VERSION 2.2  ***
C
      INTEGER   P
      DOUBLE PRECISION D(*), X(*), X0(*)
C/+
C      DOUBLE PRECISION ABS
C/
      INTEGER   I
      DOUBLE PRECISION EMAX, T, XMAX, ZERO
C/6
      DATA ZERO/0.D+0/
C/7
C     PARAMETER (ZERO=0.D+0)
C/
C
      EMAX = ZERO
      XMAX = ZERO
      DO 10 I = 1, P
         T = ABS(D(I) * (X(I) - X0(I)))
         IF (EMAX .LT. T) EMAX = T
         T = D(I) * (ABS(X(I)) + ABS(X0(I)))
         IF (XMAX .LT. T) XMAX = T
 10      CONTINUE
      RELDST = ZERO
      IF (XMAX .GT. ZERO) RELDST = EMAX / XMAX
 999  RETURN
C  ***  LAST CARD OF RELDST FOLLOWS  ***
      END
      LOGICAL FUNCTION STOPX (IDUMMY)
C-----------------------------------------------------------------------
C     *****PARAMETERS...
      INTEGER   IDUMMY
C
C     ..................................................................
C
C     *****PURPOSE...
C     THIS FUNCTION MAY SERVE AS THE STOPX (ASYNCHRONOUS INTERRUPTION)
C     FUNCTION FOR THE NL2SOL (NONLINEAR LEAST-SQUARES) PACKAGE AT
C     THOSE INSTALLATIONS WHICH DO NOT WISH TO IMPLEMENT A
C     DYNAMIC STOPX.
C
C     *****ALGORITHM NOTES...
C     AT INSTALLATIONS WHERE THE NL2SOL SYSTEM IS USED
C     INTERACTIVELY, THIS DUMMY STOPX SHOULD BE REPLACED BY A
C     FUNCTION THAT RETURNS .TRUE. IF AND ONLY IF THE INTERRUPT
C     (BREAK) KEY HAS BEEN PRESSED SINCE THE LAST CALL ON STOPX.
C
C     ..................................................................
C-----------------------------------------------------------------------
      STOPX = .FALSE.
      RETURN
      END
      SUBROUTINE VAXPY(P, W, A, X, Y)
C-----------------------------------------------------------------------
C  ***  SET W = A*X + Y  --  W, X, Y = P-VECTORS, A = SCALAR  ***
C-----------------------------------------------------------------------
      INTEGER   P
      DOUBLE PRECISION A, W(*), X(*), Y(*)
C
      INTEGER   I
C
      DO 10 I = 1, P
 10      W(I) = A*X(I) + Y(I)
      RETURN
      END
      SUBROUTINE VCOPY(P, Y, X)
C-----------------------------------------------------------------------
C  ***  SET Y = X, WHERE X AND Y ARE P-VECTORS  ***
C
      INTEGER   P
      DOUBLE PRECISION X(*), Y(*)
C-----------------------------------------------------------------------
      INTEGER   I
C
      DO 10 I = 1, P
 10      Y(I) = X(I)
      RETURN
      END
      SUBROUTINE VSCOPY(P, Y, S)
C-----------------------------------------------------------------------
C  ***  SET P-VECTOR Y TO SCALAR S  ***
C-----------------------------------------------------------------------
      INTEGER   P
      DOUBLE PRECISION S, Y(*)
C
      INTEGER   I
C
      DO 10 I = 1, P
 10      Y(I) = S
      RETURN
      END
      SUBROUTINE VVMULP(N, X, Y, Z, K)
C-----------------------------------------------------------------------
C ***  SET X(I) = Y(I) * Z(I)**K, 1 .LE. I .LE. N (FOR K = 1 OR -1)  ***
C-----------------------------------------------------------------------
      INTEGER   N, K
      DOUBLE PRECISION X(*), Y(*), Z(*)
      INTEGER   I
C
      IF (K .GE. 0) GO TO 20
      DO 10 I = 1, N
 10      X(I) = Y(I) / Z(I)
      GO TO 999
C
 20   DO 30 I = 1, N
 30      X(I) = Y(I) * Z(I)
 999  RETURN
C  ***  LAST CARD OF VVMULP FOLLOWS  ***
      END
      DOUBLE PRECISION FUNCTION V2NORM(P, X)
C-----------------------------------------------------------------------
C  ***  RETURN THE 2-NORM OF THE P-VECTOR X, TAKING  ***
C  ***  CARE TO AVOID THE MOST LIKELY UNDERFLOWS.    ***
C-----------------------------------------------------------------------
      INTEGER   P
      DOUBLE PRECISION X(*)
C
      INTEGER   I, J
      DOUBLE PRECISION ONE, R, SCALE, SQTETA, T, XI, ZERO
C/+
C      DOUBLE PRECISION ABS, SQRT
C/
      EXTERNAL RMDCON
      DOUBLE PRECISION RMDCON
C
C/6
      DATA ONE/1.D+0/, ZERO/0.D+0/
C/7
C     PARAMETER (ONE=1.D+0, ZERO=0.D+0)
C     SAVE SQTETA
C/
      DATA SQTETA/0.D+0/
C-----------------------------------------------------------------------
      IF (P .GT. 0) GO TO 10
         V2NORM = ZERO
         GO TO 999
 10   DO 20 I = 1, P
         IF (X(I) .NE. ZERO) GO TO 30
 20      CONTINUE
      V2NORM = ZERO
      GO TO 999
C
 30   SCALE = ABS(X(I))
      IF (I .LT. P) GO TO 40
         V2NORM = SCALE
         GO TO 999
 40   T = ONE
      IF (SQTETA .EQ. ZERO) SQTETA = RMDCON(2)
C
C     ***  SQTETA IS (SLIGHTLY LARGER THAN) THE SQUARE ROOT OF THE
C     ***  SMALLEST POSITIVE FLOATING POINT NUMBER ON THE MACHINE.
C     ***  THE TESTS INVOLVING SQTETA ARE DONE TO PREVENT UNDERFLOWS.
C
      J = I + 1
      DO 60 I = J, P
         XI = ABS(X(I))
         IF (XI .GT. SCALE) GO TO 50
              R = XI / SCALE
              IF (R .GT. SQTETA) T = T + R*R
              GO TO 60
 50           R = SCALE / XI
              IF (R .LE. SQTETA) R = ZERO
              T = ONE  +  T * R*R
              SCALE = XI
 60      CONTINUE
C
      V2NORM = SCALE * SQRT(T)
 999  RETURN
C  ***  LAST CARD OF V2NORM FOLLOWS  ***
      END
      SUBROUTINE WZBFGS (L, N, S, W, Y, Z)
C-----------------------------------------------------------------------
C  ***  COMPUTE  Y  AND  Z  FOR  LUPDAT  CORRESPONDING TO BFGS UPDATE.
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION L(*), S(*), W(*), Y(*), Z(*)
C     DIMENSION L(N*(N+1)/2)
C
C--------------------------  PARAMETER USAGE  --------------------------
C
C L (I/O) CHOLESKY FACTOR OF HESSIAN, A LOWER TRIANG. MATRIX STORED
C             COMPACTLY BY ROWS.
C N (INPUT) ORDER OF  L  AND LENGTH OF  S,  W,  Y,  Z.
C S (INPUT) THE STEP JUST TAKEN.
C W (OUTPUT) RIGHT SINGULAR VECTOR OF RANK 1 CORRECTION TO L.
C Y (INPUT) CHANGE IN GRADIENTS CORRESPONDING TO S.
C Z (OUTPUT) LEFT SINGULAR VECTOR OF RANK 1 CORRECTION TO L.
C
C-------------------------------  NOTES  -------------------------------
C
C  ***  ALGORITHM NOTES  ***
C
C        WHEN  S  IS COMPUTED IN CERTAIN WAYS, E.G. BY  GQTSTP  OR
C     DBLDOG,  IT IS POSSIBLE TO SAVE N**2/2 OPERATIONS SINCE  (L**T)*S
C     OR  L*(L**T)*S IS THEN KNOWN.
C        IF THE BFGS UPDATE TO L*(L**T) WOULD REDUCE ITS DETERMINANT TO
C     LESS THAN EPS TIMES ITS OLD VALUE, THEN THIS ROUTINE IN EFFECT
C     REPLACES  Y  BY  THETA*Y + (1 - THETA)*L*(L**T)*S,  WHERE  THETA
C     (BETWEEN 0 AND 1) IS CHOSEN TO MAKE THE REDUCTION FACTOR = EPS.
C
C  ***  GENERAL  ***
C
C     CODED BY DAVID M. GAY (FALL 1979).
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED
C     BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS MCS-7600324 AND
C     MCS-7906671.
C
C------------------------  EXTERNAL QUANTITIES  ------------------------
C
C  ***  FUNCTIONS AND SUBROUTINES CALLED  ***
C
      EXTERNAL DOTPRD, LIVMUL, LTVMUL
      DOUBLE PRECISION DOTPRD
C DOTPRD RETURNS INNER PRODUCT OF TWO VECTORS.
C LIVMUL MULTIPLIES L**-1 TIMES A VECTOR.
C LTVMUL MULTIPLIES L**T TIMES A VECTOR.
C
C  ***  INTRINSIC FUNCTIONS  ***
C/+
C      DOUBLE PRECISION SQRT
C/
C--------------------------  LOCAL VARIABLES  --------------------------
C
      INTEGER   I
      DOUBLE PRECISION CS, CY, EPS, EPSRT, ONE, SHS, YS, THETA
C
C  ***  DATA INITIALIZATIONS  ***
C
C/6
      DATA EPS/0.1D+0/, ONE/1.D+0/
C/7
C     PARAMETER (EPS=0.1D+0, ONE=1.D+0)
C/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      CALL LTVMUL(N, W, L, S)
      SHS = DOTPRD(N, W, W)
      YS = DOTPRD(N, Y, S)
      IF (YS .GE. EPS*SHS) GO TO 10
         THETA = (ONE - EPS) * SHS / (SHS - YS)
         EPSRT = SQRT(EPS)
         CY = THETA / (SHS * EPSRT)
         CS = (ONE + (THETA-ONE)/EPSRT) / SHS
         GO TO 20
 10   CY = ONE / (SQRT(YS) * SQRT(SHS))
      CS = ONE / SHS
 20   CALL LIVMUL(N, Z, L, Y)
      DO 30 I = 1, N
 30      Z(I) = CY * Z(I)  -  CS * W(I)
C
 999  RETURN
C  ***  LAST CARD OF WZBFGS FOLLOWS  ***
      END
      DOUBLE PRECISION FUNCTION RMDCON(K)
C-----------------------------------------------------------------------
C  ***  RETURN MACHINE DEPENDENT CONSTANTS USED BY NL2SOL  ***
C
C +++  COMMENTS BELOW CONTAIN DATA STATEMENTS FOR VARIOUS MACHINES.  +++
C +++  TO CONVERT TO ANOTHER MACHINE, PLACE A C IN COLUMN 1 OF THE   +++
C +++  DATA STATEMENT LINE(S) THAT CORRESPOND TO THE CURRENT MACHINE +++
C +++  AND REMOVE THE C FROM COLUMN 1 OF THE DATA STATEMENT LINE(S)  +++
C +++  THAT CORRESPOND TO THE NEW MACHINE.                           +++
C-----------------------------------------------------------------------
      INTEGER   K
C
C  ***  THE CONSTANT RETURNED DEPENDS ON K...
C
C  ***        K = 1... SMALLEST POS. ETA SUCH THAT -ETA EXISTS.
C  ***        K = 2... SQUARE ROOT OF ETA.
C  ***        K = 3... UNIT ROUNDOFF = SMALLEST POS. NO. MACHEP SUCH
C  ***                 THAT 1 + MACHEP .GT. 1 .AND. 1 - MACHEP .LT. 1.
C  ***        K = 4... SQUARE ROOT OF MACHEP.
C  ***        K = 5... SQUARE ROOT OF BIG (SEE K = 6).
C  ***        K = 6... LARGEST MACHINE NO. BIG SUCH THAT -BIG EXISTS.
C
      DOUBLE PRECISION BIG, ETA, MACHEP
      INTEGER   BIGI(4), ETAI(4), MACHEI(4)
C/+
C      DOUBLE PRECISION SQRT
C/
      EQUIVALENCE (BIG,BIGI(1)), (ETA,ETAI(1)), (MACHEP,MACHEI(1))
C
C  +++  IBM 360, IBM 370, OR XEROX  +++
C
C     DATA BIG/Z7FFFFFFFFFFFFFFF/, ETA/Z0010000000000000/,
C    1     MACHEP/Z3410000000000000/
C
C  +++  DATA GENERAL  +++
C
C     DATA BIG/0.7237005577D+76/, ETA/0.5397605347D-78/,
C    1     MACHEP/2.22044605D-16/
C
C  +++  DEC 11  +++
C
C     DATA BIG/1.7D+38/, ETA/2.938735878D-39/, MACHEP/2.775557562D-17/
C
C  +++  HP3000  +++
C
C     DATA BIG/1.157920892D+77/, ETA/8.636168556D-78/,
C    1     MACHEP/5.551115124D-17/
C
C  +++  HONEYWELL  +++
C
C     DATA BIG/1.69D+38/, ETA/5.9D-39/, MACHEP/2.1680435D-19/
C
C  +++  DEC10  +++
C
C     DATA BIG/"377777100000000000000000/,
C    1     ETA/"002400400000000000000000/,
C    2     MACHEP/"104400000000000000000000/
C
C  +++  BURROUGHS  +++
C
C     DATA BIG/O0777777777777777,O7777777777777777/,
C    1     ETA/O1771000000000000,O7770000000000000/,
C    2     MACHEP/O1451000000000000,O0000000000000000/
C
C  +++  CONTROL DATA  +++
C
C     DATA BIG/37767777777777777777B,37167777777777777777B/,
C    1     ETA/00014000000000000000B,00000000000000000000B/,
C    2     MACHEP/15614000000000000000B,15010000000000000000B/
C
C  +++  PRIME  +++
C
C     DATA BIG/1.0D+9786/, ETA/1.0D-9860/, MACHEP/1.4210855D-14/
C
C  +++  UNIVAC  +++
C
C     DATA BIG/8.988D+307/, ETA/1.2D-308/, MACHEP/1.734723476D-18/
C
C  +++  VAX  +++
C
      DATA BIG/1.7D+38/, ETA/2.939D-39/, MACHEP/1.3877788D-17/
C
C  +++  Convex C-1  +++
C
C      DATA BIG/8.98D+307/, ETA/5.563D-309/, MACHEP/2.22044605D-16/
C
C  +++  CRAY 1  +++
C
C     DATA BIGI(1)/577767777777777777777B/,
C    1     BIGI(2)/000007777777777777776B/,
C    2     ETAI(1)/200004000000000000000B/,
C    3     ETAI(2)/000000000000000000000B/,
C    4     MACHEI(1)/377224000000000000000B/,
C    5     MACHEI(2)/000000000000000000000B/
C
C  +++  PORT LIBRARY -- REQUIRES MORE THAN JUST A DATA STATEMENT... +++
C
C     EXTERNAL D1MACH
C     DOUBLE PRECISION D1MACH, ZERO
C     DATA BIG/0.D+0/, ETA/0.D+0/, MACHEP/0.D+0/, ZERO/0.D+0/
C     IF (BIG .GT. ZERO) GO TO 1
C        BIG = D1MACH(2)
C        ETA = D1MACH(1)
C        MACHEP = D1MACH(4)
C1    CONTINUE
C
C  +++ END OF PORT +++
C
C-------------------------------  BODY  --------------------------------
C
      GO TO (10, 20, 30, 40, 50, 60), K
C
 10   RMDCON = ETA
      GO TO 999
C
 20   RMDCON = SQRT(256.D+0*ETA)/16.D+0
      GO TO 999
C
 30   RMDCON = MACHEP
      GO TO 999
C
 40   RMDCON = SQRT(MACHEP)
      GO TO 999
C
 50   RMDCON = SQRT(BIG/256.D+0)*16.D+0
      GO TO 999
C
 60   RMDCON = BIG
C
 999  RETURN
C  ***  LAST CARD OF RMDCON FOLLOWS  ***
      END
      SUBROUTINE ITSUM (D, G, IV, LIV, P, V, X)
C-----------------------------------------------------------------------
C  ***  PRINT ITERATION SUMMARY FOR ***SOL (VERSION 2.3)  ***
C
C  ***  PARAMETER DECLARATIONS  ***
C-----------------------------------------------------------------------
      INTEGER   LIV
      INTEGER   IV(LIV), P
      DOUBLE PRECISION D(*), G(*), V(*), X(*)
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER   ALG, I, IV1, M, NF, NG, OL, PU
C/6
C/7
      CHARACTER*4 MODEL1(6), MODEL2(6)
C/
      DOUBLE PRECISION NRELDF, OLDF, PRELDF, RELDF, ZERO
C
C  ***  INTRINSIC FUNCTIONS  ***
C/+
C      INTEGER   IABS
C      DOUBLE PRECISION ABS, MAX
C/
C  ***  NO EXTERNAL FUNCTIONS OR SUBROUTINES  ***
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
      INTEGER   ALGSAV, DSTNRM, F, FDIF, F0, NEEDHD, NFCALL, NFCOV,
     1        NGCALL, NITER, NREDUC, OUTLEV, PREDUC, PRNTIT, PRUNIT,
     2        RELDX, SOLPRT, STATPR, STPPAR, SUSED, X0PRT, NGCOV
C
      INCLUDE 'INCS:DMSG.INC'
C  ***  IV SUBSCRIPT VALUES  ***
C
C/6
      DATA ALGSAV/51/, NEEDHD/36/, NFCALL/6/, NFCOV/52/, NGCALL/30/,
     1     NGCOV/53/, NITER/31/, OUTLEV/19/, PRNTIT/39/, PRUNIT/21/,
     2     SOLPRT/22/, STATPR/23/, SUSED/64/, X0PRT/24/
C/7
C     PARAMETER (ALGSAV=51, NEEDHD=36, NFCALL=6, NFCOV=52, NGCALL=30,
C    1           NGCOV=53, NITER=31, OUTLEV=19, PRNTIT=39, PRUNIT=21,
C    2           SOLPRT=22, STATPR=23, SUSED=64, X0PRT=24)
C/
C
C  ***  V SUBSCRIPT VALUES  ***
C
C/6
      DATA DSTNRM/2/, F/10/, F0/13/, FDIF/11/, NREDUC/6/, PREDUC/7/,
     1     RELDX/17/, STPPAR/5/
C/7
C     PARAMETER (DSTNRM=2, F=10, F0=13, FDIF=11, NREDUC=6, PREDUC=7,
C    1           RELDX=17, STPPAR=5)
C/
C
C/6
      DATA ZERO/0.D+0/
C/7
C     PARAMETER (ZERO=0.D+0)
C/
C/6
      DATA MODEL1/'    ','    ','    ','    ','  G ','  S '/,
     *   MODEL2/' G  ',' S  ','G-S ','S-G ','-S-G','-G-S'/
C/
C
C-------------------------------  BODY  --------------------------------
C
      PU = IV(PRUNIT)
      IF (PU .EQ. 0) GO TO 999
      IV1 = IV(1)
      IF (IV1 .GT. 62) IV1 = IV1 - 51
      OL = IV(OUTLEV)
      ALG = IV(ALGSAV)
      IF (IV1 .LT. 2 .OR. IV1 .GT. 15) GO TO 370
      IF (IV1 .GE. 12) GO TO 120
      IF (IV1 .EQ. 2 .AND. IV(NITER) .EQ. 0) GO TO 390
      IF (OL .EQ. 0) GO TO 120
      IF (IV1 .GE. 10 .AND. IV(PRNTIT) .EQ. 0) GO TO 120
      IF (IV1 .GT. 2) GO TO 10
         IV(PRNTIT) = IV(PRNTIT) + 1
         IF (IV(PRNTIT) .LT. ABS(OL)) GO TO 999
 10   NF = IV(NFCALL) - ABS(IV(NFCOV))
      IV(PRNTIT) = 0
      RELDF = ZERO
      PRELDF = ZERO
      OLDF = MAX(ABS(V(F0)), ABS(V(F)))
      IF (OLDF .LE. ZERO) GO TO 20
         RELDF = V(FDIF) / OLDF
         PRELDF = V(PREDUC) / OLDF
 20   IF (OL .GT. 0) GO TO 60
C
C        ***  PRINT SHORT SUMMARY LINE  ***
C
C                                       AIPS mod
C         IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 1) WRITE(PU,30)
C 30   FORMAT(/10H   IT   NF,6X,1HF,7X,5HRELDF,3X,6HPRELDF,3X,5HRELDX,
      WRITE (MSGTXT,30)
      IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 1) CALL MSGWRT (6)
 30   FORMAT('   IT   NF',6X,'F',7X,'RELDF',3X,'PRELDF',3X,'RELDX',
     *       2X,'MODEL  STPPAR')
C         IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 2) WRITE(PU,40)
C 40   FORMAT(/11H    IT   NF,7X,1HF,8X,5HRELDF,4X,6HPRELDF,4X,5HRELDX,
      WRITE (MSGTXT,30)
      IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 2) CALL MSGWRT (6)
 40   FORMAT('    IT   NF',7X,'F',8X,'RELDF',4X,'PRELDF',4X,'RELDX',
     *       3X,'STPPAR')
         IV(NEEDHD) = 0
         IF (ALG .EQ. 2) GO TO 50
         M = IV(SUSED)
C                                       AIPS mod
C         WRITE(PU,100) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX),
C     1                 MODEL1(M), MODEL2(M), V(STPPAR)
         WRITE (MSGTXT,100) IV(NITER), NF, V(F), RELDF, PRELDF,
     *                 V(RELDX), MODEL1(M), MODEL2(M), V(STPPAR)
         CALL MSGWRT (6)
         GO TO 120
C
 50      WRITE(PU,110) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX),
     1                 V(STPPAR)
         GO TO 120
C
C     ***  PRINT LONG SUMMARY LINE  ***
C
C                                       AIPS mod
C 60   IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 1) WRITE(PU,70)
C 70   FORMAT(/11H    IT   NF,6X,1HF,7X,5HRELDF,3X,6HPRELDF,3X,5HRELDX,
 60   IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 1) THEN
         WRITE (MSGTXT,70)
         CALL MSGWRT (6)
         END IF
 70   FORMAT('    IT   NF',6X,'F',7X,'RELDF',3X,'PRELDF',3X,'RELDX',
     *       2X,'MODEL  STPPAR',2X,'D*STEP',2X,'NPRELDF')
C      IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 2) WRITE(PU,80)
C 80   FORMAT(/11H    IT   NF,7X,1HF,8X,5HRELDF,4X,6HPRELDF,4X,5HRELDX,
      IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 2) THEN
         WRITE (MSGTXT,70)
         CALL MSGWRT (6)
         END IF
 80   FORMAT('    IT   NF',7X,'F',8X,'RELDF',4X,'PRELDF',4X,'RELDX',
     *       3X,'STPPAR',3X,'D*STEP',3X,'NPRELDF')
      IV(NEEDHD) = 0
      NRELDF = ZERO
      IF (OLDF .GT. ZERO) NRELDF = V(NREDUC) / OLDF
      IF (ALG .EQ. 2) GO TO 90
      M = IV(SUSED)
C                                       AIPS mod
C      WRITE(PU,100) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX),
C     1             MODEL1(M), MODEL2(M), V(STPPAR), V(DSTNRM), NRELDF
      WRITE (MSGTXT,100) IV(NITER), NF, V(F), RELDF, PRELDF,
     *   V(RELDX), MODEL1(M), MODEL2(M), V(STPPAR), V(DSTNRM), NRELDF
      CALL MSGWRT (6)
      GO TO 120
C
C                                       AIPS mod
C 90   WRITE(PU,110) IV(NITER), NF, V(F), RELDF, PRELDF,
C     1             V(RELDX), V(STPPAR), V(DSTNRM), NRELDF
 90   WRITE (MSGTXT,110) IV(NITER), NF, V(F), RELDF, PRELDF,
     *             V(RELDX), V(STPPAR), V(DSTNRM), NRELDF
      CALL MSGWRT (6)
 100  FORMAT (I6,I5,1PD10.3,2D9.2,D8.1,A3,A4,2D8.1,D9.2)
 110  FORMAT (I6,I5,1PD11.3,2D10.2,3D9.1,D10.2)
C
 120  IF (IV(STATPR) .LT. 0) GO TO 430
      GO TO (999, 999, 130, 150, 170, 190, 210, 230, 250, 270, 290, 310,
     1       330, 350, 520), IV1
C
C                                       AIPS mod
 130  WRITE (MSGTXT,140)
      CALL MSGWRT (6)
 140  FORMAT (' ***** X-CONVERGENCE *****')
      GO TO 430
C
C                                       AIPS mod
 150  WRITE (MSGTXT,160)
      CALL MSGWRT (6)
 160  FORMAT (' ***** RELATIVE FUNCTION CONVERGENCE *****')
      GO TO 430
C
C                                       AIPS mod
 170  WRITE (MSGTXT,180)
      CALL MSGWRT (6)
 180  FORMAT (' ***** X- AND RELATIVE FUNCTION CONVERGENCE *****')
      GO TO 430
C
C                                       AIPS mod
 190  WRITE (MSGTXT,200)
      CALL MSGWRT (6)
 200  FORMAT (' ***** ABSOLUTE FUNCTION CONVERGENCE *****')
      GO TO 430
C
C                                       AIPS mod
 210  WRITE (MSGTXT,220)
      CALL MSGWRT (6)
 220  FORMAT (' ***** SINGULAR CONVERGENCE *****')
      GO TO 430
C
C                                       AIPS mod
 230  WRITE (MSGTXT,240)
      CALL MSGWRT (6)
 240  FORMAT (' ***** FALSE CONVERGENCE *****')
      GO TO 430
C
C                                       AIPS mod
 250  WRITE (MSGTXT,260)
      CALL MSGWRT (6)
 260  FORMAT (' ***** FUNCTION EVALUATION LIMIT *****')
      GO TO 430
C
C                                       AIPS mod
 270  WRITE (MSGTXT,280)
      CALL MSGWRT (6)
 280  FORMAT (' ***** ITERATION LIMIT *****')
      GO TO 430
C
C                                       AIPS mod
 290  WRITE (MSGTXT,300)
      CALL MSGWRT (6)
 300  FORMAT (' ***** STOPX *****')
      GO TO 430
C
C                                       AIPS mod
 310  WRITE (MSGTXT,320)
      CALL MSGWRT (6)
 320  FORMAT (' ***** INITIAL F(X) CANNOT BE COMPUTED *****')
C
      GO TO 390
C
C                                       AIPS mod
 330  WRITE (MSGTXT,340)
      CALL MSGWRT (6)
 340  FORMAT (' ***** BAD PARAMETERS TO ASSESS *****')
      GO TO 999
C
C                                       AIPS mod
 350  WRITE (MSGTXT,360)
      CALL MSGWRT (6)
 360  FORMAT (' ***** GRADIENT COULD NOT BE COMPUTED *****')
      IF (IV(NITER) .GT. 0) GO TO 480
      GO TO 390
C
C                                       AIPS mod
 370  WRITE (MSGTXT,380) IV(1)
      CALL MSGWRT (6)
 380  FORMAT (' ***** IV(1) =',I5,' *****')
      GO TO 999
C
C  ***  INITIAL CALL ON ITSUM  ***
C
C                                       AIPS MOD
C 390  IF (IV(X0PRT) .NE. 0) WRITE(PU,400) (I, X(I), D(I), I = 1, P)
C400 FORMAT (/23H     I     INITIAL X(I),8X,4HD(I)//(1X,I5,D17.6,D14.3))
C 390  WRITE (MSGTXT,391)
 390   CONTINUE
C      CALL MSGWRT (6)
C 391  FORMAT (23H     I     INITIAL X(I),8X,4HD(I) )
C      DO 400 I = 1,P
C         WRITE (MSGTXT,395) I, X(I), D(I)
C         CALL MSGWRT (6)
C 395     FORMAT (1X,I5,1PD17.6,D14.3)
C 400     CONTINUE
C     *** THE FOLLOWING ARE TO AVOID UNDEFINED VARIABLES WHEN THE
C     *** FUNCTION EVALUATION LIMIT IS 1...
      V(DSTNRM) = ZERO
      V(FDIF) = ZERO
      V(NREDUC) = ZERO
      V(PREDUC) = ZERO
      V(RELDX)  = ZERO
      IF (IV1 .GE. 12) GO TO 999
      IV(NEEDHD) = 0
      IV(PRNTIT) = 0
      IF (OL .EQ. 0) GO TO 999
C                                       AIPS mod
C      IF (OL .LT. 0 .AND. ALG .EQ. 1) WRITE(PU,30)
C      IF (OL .LT. 0 .AND. ALG .EQ. 2) WRITE(PU,40)
C      IF (OL .GT. 0 .AND. ALG .EQ. 1) WRITE(PU,70)
C      IF (OL .GT. 0 .AND. ALG .EQ. 2) WRITE(PU,80)
C      IF (ALG .EQ. 1) WRITE(PU,410) V(F)
C      IF (ALG .EQ. 2) WRITE(PU,420) V(F)
      IF (OL .LT. 0 .AND. ALG .EQ. 1) THEN
          WRITE(MSGTXT,30)
          CALL MSGWRT (6)
          END IF
      IF (OL .LT. 0 .AND. ALG .EQ. 2) THEN
          WRITE(MSGTXT,40)
          CALL MSGWRT (6)
          END IF
      IF (OL .GT. 0 .AND. ALG .EQ. 1) THEN
          WRITE(MSGTXT,70)
          CALL MSGWRT (6)
          END IF
      IF (OL .GT. 0 .AND. ALG .EQ. 2) THEN
          WRITE(MSGTXT,80)
          CALL MSGWRT (6)
          END IF
      IF (ALG .EQ. 1) THEN
          WRITE(MSGTXT,410) V(F)
          CALL MSGWRT (6)
          END IF
      IF (ALG .EQ. 2) THEN
          WRITE(MSGTXT,420) V(F)
          CALL MSGWRT (6)
          END IF
 410  FORMAT('     0    1',1PD10.3)
C365  FORMAT(/11H     0    1,E11.3)
 420  FORMAT('     0    1',1PD11.3)
      GO TO 999
C
C  ***  PRINT VARIOUS INFORMATION REQUESTED ON SOLUTION  ***
C
 430  IV(NEEDHD) = 1
      IF (IV(STATPR) .EQ. 0) GO TO 480
         OLDF = MAX(ABS(V(F0)), ABS(V(F)))
         PRELDF = ZERO
         NRELDF = ZERO
         IF (OLDF .LE. ZERO) GO TO 440
              PRELDF = V(PREDUC) / OLDF
              NRELDF = V(NREDUC) / OLDF
 440     NF = IV(NFCALL) - IV(NFCOV)
         NG = IV(NGCALL) - IV(NGCOV)
C                                       AIPS mod
C         WRITE(PU,450) V(F), V(RELDX), NF, NG, PRELDF, NRELDF
C 450  FORMAT(/9H FUNCTION,D17.6,8H   RELDX,D17.3/12H FUNC. EVALS,
C     1   I8,9X,11HGRAD. EVALS,I8/7H PRELDF,D16.3,6X,7HNPRELDF,D15.3)
         WRITE (MSGTXT,450) V(F), V(RELDX)
         CALL MSGWRT (6)
         WRITE (MSGTXT,451) NF, NG
         CALL MSGWRT (6)
         WRITE (MSGTXT,452) PRELDF, NRELDF
         CALL MSGWRT (6)
 450  FORMAT (' FUNCTION',1PD17.6,'   RELDX',D17.3)
 451  FORMAT (' FUNC. EVALS',I8,9X,'GRAD. EVALS',I8)
 452  FORMAT (' PRELDF',1PD16.3,6X,'NPRELDF',D15.3)
C
         IF (IV(NFCOV) .GT. 0) THEN
            WRITE (MSGTXT,460) IV(NFCOV)
            CALL MSGWRT (6)
            END IF
 460     FORMAT(1X,I4,' EXTRA FUNC. EVALS FOR COVARIANCE AND DIAGNOST',
     *      'ICS.')
         IF (IV(NGCOV) .GT. 0) THEN
            WRITE (MSGTXT,470) IV(NGCOV)
            CALL MSGWRT (6)
            END IF
 470     FORMAT(1X,I4,' EXTRA GRAD. EVALS FOR COVARIANCE AND DIAGNOSTI',
     *      'CS.')
C
 480  IF (IV(SOLPRT) .EQ. 0) GO TO 999
         IV(NEEDHD) = 1
         WRITE (MSGTXT,490)
         CALL MSGWRT (6)
 490  FORMAT('     I      FINAL X(I)',8X,'D(I)',10X,'G(I)')
         DO 500 I = 1,P
              WRITE (MSGTXT,510) I, X(I), D(I), G(I)
              CALL MSGWRT(6)
 500          CONTINUE
 510     FORMAT(1X,I5,1PD16.6,2D14.3)
      GO TO 999
C
 520  WRITE (MSGTXT,530)
      CALL MSGWRT (6)
 530  FORMAT(' INCONSISTENT DIMENSIONS')
 999  RETURN
C  ***  LAST CARD OF ITSUM FOLLOWS  ***
      END
      SUBROUTINE DPPDI (AP, N, DET, JOB)
C-----------------------------------------------------------------------
      INTEGER   N, JOB
      DOUBLE PRECISION AP(*), DET(2)
C
C     DPPDI COMPUTES THE DETERMINANT AND INVERSE
C     OF A DOUBLE PRECISION SYMMETRIC POSITIVE DEFINITE MATRIX
C     USING THE FACTORS COMPUTED BY DPPCO OR DPPFA .
C
C     ON ENTRY
C
C        AP      DOUBLE PRECISION (N*(N+1)/2)
C                THE OUTPUT FROM DPPCO OR DPPFA.
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        JOB     INTEGER
C                = 11   BOTH DETERMINANT AND INVERSE.
C                = 01   INVERSE ONLY.
C                = 10   DETERMINANT ONLY.
C
C     ON RETURN
C
C        AP      THE UPPER TRIANGULAR HALF OF THE INVERSE .
C                THE STRICT LOWER TRIANGLE IS UNALTERED.
C
C        DET     DOUBLE PRECISION(2)
C                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.
C                OTHERWISE NOT REFERENCED.
C                DETERMINANT = DET(1) * 10.0**DET(2)
C                WITH  1.0 .LE. DET(1) .LT. 10.0
C                OR  DET(1) .EQ. 0.0 .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
C        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
C        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
C        AND IF DPOCO OR DPOFA HAS SET INFO .EQ. 0 .
C
C     LINPACK.  THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS DAXPY,DSCAL
C     FORTRAN MOD
C
C     INTERNAL VARIABLES
C
      DOUBLE PRECISION T
      DOUBLE PRECISION S
      INTEGER   I, II, J, JJ, JM1, J1, K, KJ, KK, KP1, K1
C-----------------------------------------------------------------------
C
C     COMPUTE DETERMINANT
C
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0D0
         DET(2) = 0.0D0
         S = 10.0D0
         II = 0
         DO 50 I = 1, N
            II = II + I
            DET(1) = AP(II)**2*DET(1)
C        ...EXIT
            IF (DET(1) .EQ. 0.0D0) GO TO 60
   10       IF (DET(1) .GE. 1.0D0) GO TO 20
               DET(1) = S*DET(1)
               DET(2) = DET(2) - 1.0D0
            GO TO 10
   20       CONTINUE
   30       IF (DET(1) .LT. S) GO TO 40
               DET(1) = DET(1)/S
               DET(2) = DET(2) + 1.0D0
            GO TO 30
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     COMPUTE INVERSE(R)
C
      IF (MOD(JOB,10) .EQ. 0) GO TO 140
         KK = 0
         DO 100 K = 1, N
            K1 = KK + 1
            KK = KK + K
            AP(KK) = 1.0D0/AP(KK)
            T = -AP(KK)
            CALL DSCAL(K-1,T,AP(K1),1)
            KP1 = K + 1
            J1 = KK + 1
            KJ = KK + K
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1, N
               T = AP(KJ)
               AP(KJ) = 0.0D0
               CALL DAXPY(K,T,AP(K1),1,AP(J1),1)
               J1 = J1 + J
               KJ = KJ + J
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        FORM  INVERSE(R) * TRANS(INVERSE(R))
C
         JJ = 0
         DO 130 J = 1, N
            J1 = JJ + 1
            JJ = JJ + J
            JM1 = J - 1
            K1 = 1
            KJ = J1
            IF (JM1 .LT. 1) GO TO 120
            DO 110 K = 1, JM1
               T = AP(KJ)
               CALL DAXPY(K,T,AP(J1),1,AP(K1),1)
               K1 = K1 + K
               KJ = KJ + 1
  110       CONTINUE
  120       CONTINUE
            T = AP(JJ)
            CALL DSCAL(J,T,AP(J1),1)
  130    CONTINUE
  140 CONTINUE
      RETURN
      END
      SUBROUTINE DAXPY (N, DA, DX, INCX, DY, INCY)
C-----------------------------------------------------------------------
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C-----------------------------------------------------------------------
      DOUBLE PRECISION    DX(*), DY(*), DA
      INTEGER     I, INCX, INCY, M, MP1, N, IX, IY
C-----------------------------------------------------------------------
      IF(N.LE.0) GO TO 999
      IF (DA .EQ. 0.0D0) GO TO 999
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
      GO TO 999
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C        CLEAN-UP LOOP
C
   20 M = MOD (N, 4)
      IF(M .EQ. 0) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF(N .LT. 4) GO TO 999
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
C
 999  RETURN
      END
      SUBROUTINE DSCAL (N, DA, DX, INCX)
C-----------------------------------------------------------------------
C     SCALES A VECTOR BY A CONSTANT.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C-----------------------------------------------------------------------
      DOUBLE PRECISION    DA, DX(*)
      INTEGER   I, INCX, M, MP1, N, NINCX
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      IF (INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DX(I) = DA*DX(I)
   10 CONTINUE
      GO TO 999
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD (N, 5)
      IF(M .EQ. 0) GO TO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF(N .LT. 5) GO TO 999
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
C
 999  RETURN
      END
