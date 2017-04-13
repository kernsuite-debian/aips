LOCAL INCLUDE 'RLCAL.INC'
C                                                         Include RLCAL
C                                       Local include for RLCAL
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       MAXDAT = Number of vis in
C                                       arrays.
C                                       MAXSCN max # time intervals
      INTEGER   MAXDAT, MAXSCN
      PARAMETER (MAXDAT=20000)
      PARAMETER (MAXSCN=5000)
C
      INTEGER   CATIN(256), SEQIN, SEQ2, DISKIN, DISK2, CNOIN, CNOIN2,
     *   CCTVER, VISDSK, VISCNO, VMDSK, VMCNO, JBUFSZ, ANTS(2,MAXDAT),
     *   VSOU(MAXDAT), VCALID, NCOMP(MAXFLD), NUMDAT,
     *   IONSCR, FREQID, CHNSEL(3,20,MAXIF), PSMRAD, CPVER, CSIDNO,
     *   NSCANS
      LOGICAL   SINGLE, CLNMOD, ISXY, AVGIF, IMODEL, DOLINE, DOINTP,
     *   CMODEL
      REAL   XSI, XDI, XQUAL, XTIME(8), XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XANTS(50), XUVRA(2), XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XCHNS(4,20), XS2,
     *   XD2, XVER, XNCOMP(MAXAFL), XFLUX, XNMAP, XSMOD(7),
     *   XSPECT(4,20), XSOLIN, XPRTLV, XBADD(10),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), TIME(MAXDAT), PARAN(2,MAXDAT),
     *   VMOD(2,4,MAXDAT), VWT(4,MAXDAT), VOBS(2,4,MAXDAT),
     *   VFLUX(4,MAXIF), VFINC(MAXIF), REFPIX, PSMTAB(1024),
     *   PHDIFF(MAXCIF), TSCAN(2,MAXSCN), PCORR(MAXIF,MAXSCN)
      HOLLERITH XNAMEI(3), XCLAIN(2), XXCALC, XXSOUR(4,30), XNAME2(3),
     *   XCLAS2(2), XCMETH, XCMOD
      DOUBLE PRECISION FREQSO(MAXIF)
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, NAME2*12, CLAS2*6,
     *   CMETH*4, CMOD*4, CSNAME*16, XCALCO*4
      COMMON /CINFO/ CATIN, FREQSO, NCOMP, SINGLE, CLNMOD, CNOIN,
     *   CNOIN2, CCTVER, VISDSK, VISCNO, VMDSK, VMCNO, IONSCR,
     *   DISKIN, DISK2, SEQIN, SEQ2, FREQID, IMODEL, CHNSEL, DOLINE,
     *   PSMRAD, PSMTAB, DOINTP, CPVER, CSIDNO, CMODEL
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /XINPUT/ XNAMEI, XCLAIN, XSI, XDI, XXSOUR, XQUAL, XXCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XBIF, XEIF, XANTS, XUVRA, XSUBA,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH, XCHNS, XNAME2, XCLAS2, XS2, XD2, XVER, XNCOMP, XFLUX,
     *   XNMAP, XCMETH, XCMOD, XSMOD, XSPECT, XSOLIN, XPRTLV, XBADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, NAME2, CLAS2, CMETH,
     *   CMOD, CSNAME, XCALCO
      COMMON /VDATA/ TIME, PARAN, VMOD, VWT, VOBS, VFLUX, ISXY,
     *   AVGIF, NUMDAT, ANTS, VSOU, VCALID, VFINC,
     *   REFPIX, PHDIFF, NSCANS, TSCAN, PCORR
C                                                          End RLCAL
LOCAL END
      PROGRAM RLCAL
C-----------------------------------------------------------------------
C! Determines Antenna polarization characteristics
C# UV Calibration AP-appl EXT-appl polarization
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2012, 2014-2016
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
C   RLCAL computes polarization parameters and enters them into the AN
C   table  also determines calibrator polarizations and enters them in
C   the SU table.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NC, NI, NA
      INCLUDE 'RLCAL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      DATA PRGM /'RLCAL'/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL RLCIN (PRGM, NC, NI, NA, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Determine poln. parameters.
      CALL RLCUV (NC, NI, NA, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL RLCHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE RLCIN (PRGN, NC, NI, NA, IRET)
C-----------------------------------------------------------------------
C   RLCIN gets input parameters for RLCAL.
C   Inputs:
C      PRGN    C*6     Program name
C      NC      I     Number spectral channels
C      NI      I     Number IFs
C      NA      I     Max antenna number
C   Output:
C      IRET    I         Error code: 0 => ok
C                          1 => too few frequency channels.
C                          5 => catalog troubles
C                          8 => can't start
C   Commons: /XINPUT/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in RLCAL for more details.
C-----------------------------------------------------------------------
      INTEGER   NC, NI, NA, IRET
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   IERR, NPARM, I, MXFLD, NUMSUB, IROUND, LUN1, NUMAN(513),
     *   J, K, K1, K2, LUNTMP, BUFFER(512)
      LOGICAL   T, TABLE, EXIST, FITASC, MATCH
      REAL      CATR(256), CATINR(256), DUM
      DOUBLE PRECISION CATD(128)
      INCLUDE 'RLCAL.INC'
      INTEGER   NW(MAXIF), ISBAND(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      DOUBLE PRECISION REFFRQ, FOFF(MAXIF), FRATIO
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
      DATA LUN1 /28/
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
      NPARM = 397 + MAXAFL
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
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (4, 1, XCMETH, CMETH)
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
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      DO 30 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 30      CONTINUE
C                                       Linearly polarized feeds?
      ISXY = CATD(KDCRV+JLOCS) .LT. -4.0D0
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
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      STOKES = 'RLLR'
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
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
      CALL FNDEXT ('NX', CATBLK, I)
      IF (XSOLIN.LE.0.0) THEN
         IF (I.GT.0) THEN
            XSOLIN = 1.E6
         ELSE
            XSOLIN = 10.0
            END IF
         END IF
      XSOLIN = XSOLIN / (24.0 * 60.0)
      CMODEL = ((NAME2.NE.' ') .OR. (CLAS2.NE.' ')) .AND. (XNMAP.GT.0.5)
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
C                                       Get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      NA = NSTNS
C                                       check UV data set exists,
C                                       set variables for RLCSOU
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVGET ('CLOS', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Init. cal. fluxes etc.
      VCALID = 0
      K = 4 *NUMIF
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
         VCALID = 1
         CALL H2CHR (8, 1, CATUV(KHOBJ), CSNAME)
         CSIDNO = 1
         CALL DFILL (NUMIF, 0.0D0, FREQSO)
         END IF
C                                       multi-source
      CALL RLCSOU (IRET)
      IF (IRET.GT.0) GO TO 999
C                                       get full frequencies
      K1 = 1
      LUN1 = LUNTMP (1)
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, K1, CATIN, LUN1,
     *   NUMIF, FOFF, ISBAND, VFINC, BNDCOD, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
      REFFRQ = CATD(KDCRV+JLOCF)
      REFPIX = CATR(KRCRP+JLOCF)
      DO 100 J = 1,NUMIF
         FREQSO(J) = FREQSO(J) + REFFRQ + FOFF(J)
         IF (IMODEL) THEN
            FRATIO = FREQSO(J) / FREQSO(1)
            DO 95 K = 1,4
               VFLUX(K,J) = XSMOD(K) * (FRATIO**XSPECT(K,1))
 95            CONTINUE
            END IF
 100     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RLCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1050 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1070 FORMAT ('SORT ORDER IS ',A2,' NOT ',A2,' AS REQUIRED')
 1085 FORMAT ('SPECIFIED SUBARRAY ',I4,' > MAX. OF ',I4)
 1100 FORMAT ('RLCIN: ERROR',I4,' GETTING FREQUENCY PARAMETERS')
      END
      SUBROUTINE RLCSOU (IRET)
C-----------------------------------------------------------------------
C   RLCSOU collects calibrator polarized flux density information.
C   Input from common:
C      NSOUWD   I       Number of sources included or excluded; if 0
C                       all sources are included.
C      DOSWNT   L       If .TRUE. then sources in SOUWAN are included
C                       If .FALSE. then excluded.
C      SOUWAN   I(30)   The source numbers of sources included or
C                       excluded.
C      DISKIN   I       Disk number of the input multisource data file
C                       whose SU table is to be used.
C      CNOIN    I       Catalog slot number for SU file.
C   Output to Common:
C      VCALID   I       Calibrator ID numbers
C      VFLUX    R(4,if)   Polarized flux densities (I,Q,U,V) (Jy)
C   Output:
C      IRET     I       Return code, 0 => OK, otherwise abort.
C   Note: also uses buffer NXBUFF
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'RLCAL.INC'
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   I, SUKOLS(MAXSUC), SUNUMV(MAXSUC), IDSOU, QUAL, SULUN,
     *   ISURNO, NUMSOU, LOOP, SUFQID, II, ISUB, VS, VE, VERS
      DOUBLE PRECISION BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF), RAOBS,
     *   DECOBS
      REAL      FLUX(4,MAXIF), EPS, TBEG, CTIME, DTIME
      LOGICAL   TABLE, EXIST, FITASC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA SULUN /27/
C-----------------------------------------------------------------------
      EPS = 1.0 / (24.0 * 3600.0 * 10.0)
      IF (.NOT.SINGLE) THEN
C                                       Open source (SU) table
         CALL SOUINI ('READ', NXBUFF, DISKIN, CNOIN, 1, CATUV,
     *      SULUN, NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS,
     *      SUNUMV, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING SOURCE TABLE'
            GO TO 990
            END IF
C                                       Check FREQID compatibility.
         IF ((SUFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *      (SUFQID.NE.FREQID)) THEN
            MSGTXT = 'WARNING - POTENTIALLY FATAL ERROR'
            CALL MSGWRT (6)
            WRITE (MSGTXT,1040) SUFQID
            CALL MSGWRT (6)
            WRITE (MSGTXT,1050) FREQID
            CALL MSGWRT (6)
            MSGTXT = '   Rerun SETJY with the correct FREQID'
            CALL MSGWRT (6)
            IRET = 5
            GO TO 999
            END IF
C                                       Get number of sources
         NUMSOU = NXBUFF(5)
C                                       Read flux array
         VCALID = 0
         DO 50 LOOP = 1,NUMSOU
            ISURNO = LOOP
            CALL TABSOU ('READ', NXBUFF, ISURNO, SUKOLS, SUNUMV,
     *         IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *         DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL,
     *         RESTFQ, PMRA, PMDEC, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING SOURCE TABLE'
               GO TO 990
               END IF
C                                       Save calibrator fluxes
C                                       See if wanted
            IF (NSOUWD.GT.0) THEN
               DO 20 I = 1,NSOUWD
                  IF (IDSOU.EQ.SOUWAN(I)) GO TO 30
 20               CONTINUE
C                                       Not wanted
               GO TO 50
               END IF
C                                       Wanted
 30         IF (VCALID.GT.0) THEN
               MSGTXT = 'MORE THAN ONE SOURCE MATCHES SOURCE PARAMETERS'
               IRET = 10
               GO TO 990
            ELSE
               VCALID = IDSOU
               DO 35 I = 1,NUMIF
                  VFLUX(1,I) = FLUX(1,I)
                  VFLUX(2,I) = FLUX(2,I)
                  VFLUX(3,I) = FLUX(3,I)
                  VFLUX(4,I) = FLUX(4,I)
                  FREQSO(I) = FREQO(I)
 35               CONTINUE
               CSNAME = SOUNAM
               CSIDNO = IDSOU
               END IF
 50         CONTINUE
C                                       Close table
         CALL TABIO ('CLOS', 0, ISURNO, BUFF1, NXBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSING SOURCE TABLE'
            GO TO 990
            END IF
         END IF
C                                       Index table
      VERS = 1
      CALL ISTAB ('NX', DISKIN, CNOIN, VERS, SULUN, NXBUFF, TABLE,
     *   EXIST, FITASC, IRET)
      IF ((EXIST) .AND. (IRET.EQ.0)) THEN
         CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, VERS, CATUV, SULUN,
     *      ISURNO, SUKOLS, SUNUMV, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INDEX TABLE'
            GO TO 990
            END IF
         NUMSOU = NXBUFF(5)
C                                       Read flux array
         DO 110 LOOP = 1,NUMSOU
            ISURNO = LOOP
            CALL TABNDX ('READ', NXBUFF, ISURNO, SUKOLS, SUNUMV, CTIME,
     *         DTIME, IDSOU, ISUB, VS, VE, FREQID, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING INDEX TABLE'
               GO TO 990
               END IF
            IF ((ISUB.NE.SUBARR) .AND. (SUBARR.GT.0)) GO TO 110
            IF ((FREQID.GT.0) .AND. (FRQSEL.NE.FREQID) .AND.
     *         (FRQSEL.GT.0)) GO TO 110
            IF (IDSOU.EQ.CSIDNO) THEN
               IF (XSOLIN.GE.DTIME) THEN
                  NSCANS = NSCANS + 1
                  TSCAN(1,NSCANS) = CTIME - DTIME/2 - EPS
                  TSCAN(2,NSCANS) = CTIME + DTIME/2 + EPS
               ELSE
                  II = DTIME / XSOLIN + 0.5
                  TBEG = CTIME - DTIME/2
                  DTIME = DTIME / II
                  DO 105 I = 1,II
                     NSCANS = NSCANS + 1
                     TSCAN(1,NSCANS) = TBEG + (I-1)*DTIME - EPS/3.
                     TSCAN(2,NSCANS) = TBEG + I*DTIME + EPS/3.
 105                 CONTINUE
                  END IF
               END IF
 110        CONTINUE
C                                       Close table
         CALL TABIO ('CLOS', 0, ISURNO, BUFF1, NXBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSING INDEX TABLE'
            GO TO 990
            END IF
C                                       no NX
      ELSE IF ((IRET.GT.0) .OR. (.NOT.SINGLE)) THEN
         MSGTXT = 'UNABLE TO OPEN INDEX TABLE'
         IF (IRET.LE.0) IRET = 10
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RLCSOU: ERROR',I3,' ON ',A)
 1040 FORMAT ('   Your calibrators have their fluxes set for FQID ',I3)
 1050 FORMAT ('   You are using them to calibrate FQID ',I3)
      END
      SUBROUTINE RLCUV (NC, NI, NA, IRET)
C-----------------------------------------------------------------------
C   RLCUV cause the work to be done
C   Inputs:
C      NC       I      Number spectral channels
C      NI       I      Number IFs
C      NA       I      Max antenna number
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NC, NI, NA, IRET
C
      INCLUDE 'RLCAL.INC'
      HOLLERITH CATINH(256)
      INTEGER   I, IFNO, ISNRNO, NODENO, SAVEIF, SAVBIF, J, SNBUFF(512),
     *   SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMNOD, VER, LUN, LOOPC,
     *   REFA(2,MAXIF), INNSCR, LOOPIF
      LOGICAL   ISAPPL
      REAL      RANOD, DECNOD, LFR, MBDELY(2), TIMEI, CREAL(2,MAXIF),
     *   CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF),
     *   DISP(2), DDISP(2)
      DOUBLE PRECISION CTIME
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATIN, CATINH)
C-----------------------------------------------------------------------
      I = MAXIF * MAXSCN
      CALL RFILL (I, 0.0, PCORR)
      SAVBIF = BIF
      SAVEIF = EIF
C                                       an IF at a time
      DO 20 IFNO = SAVBIF,SAVEIF
         INNSCR = NSCR
         VISDSK = 0
         VISCNO = 0
         CALL RLCSEL (IFNO, NC, CHNSEL(1,1,IFNO), IRET)
         IF (IRET.NE.0) GO TO 999
         DO 10 I = INNSCR+1,NSCR
            LOOPIF = 1
            LOOPC = 2
            CALL MAPCLR (LOOPIF, SCRVOL(I), SCRCNO(I), LOOPC, BUFF2)
 10         CONTINUE
         NSCR = INNSCR
 20      CONTINUE
      BIF = SAVBIF
      EIF = SAVEIF
C                                       write a new SN table
      VER = 0
      NUMANT = NA
      NUMPOL = 2
      NUMIF = NI
      NUMNOD = 0
      GMMOD = 1.0
      RANOD = 0.0
      DECNOD = 0.0
      ISAPPL = .FALSE.
      LUN = 27
      CALL SNINI ('WRIT', SNBUFF, DISKIN, CNOIN, VER, CATIN, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING NEW SN TABLE'
         GO TO 980
         END IF
      LFR = 0.0
      MBDELY(1) = 0.0
      MBDELY(2) = 0.0
      DISP(1) = 0.0
      DISP(2) = 0.0
      DDISP(1) = 0.0
      DDISP(2) = 0.0
      NODENO = 0
      I = 2 * NI
      CALL RFILL (I, 1.0, CREAL)
      CALL RFILL (I, 0.0, CIMAG)
      CALL RFILL (I, 0.0, DELAY)
      CALL RFILL (I, 0.0, RATE)
      CALL RFILL (I, 1.0, WEIGHT)
      CALL FILL (I, 1, REFA)
      DO 100 I = 1,NSCANS
         DO 80 IFNO = BIF,EIF
            CREAL(2,IFNO) = COS (DG2RAD * PCORR(IFNO,I))
            CIMAG(2,IFNO) = -SIN (DG2RAD * PCORR(IFNO,I))
 80         CONTINUE
         CTIME = TSCAN(1,I)/2.0D0  + TSCAN(2,I)/2.0D0
         TIMEI = TSCAN(2,I) - TSCAN(1,I)
         DO 90 J = 1,NA
            CALL TABSN ('WRIT', SNBUFF, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *         CTIME, TIMEI, CSIDNO, J, SUBARR, FRQSEL, LFR, NODENO,
     *         MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *         REFA, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING SN TABLE'
               GO TO 980
               END IF
 90         CONTINUE
 100     CONTINUE
      CALL TABSN ('CLOS', SNBUFF, ISNRNO, SNKOLS, SNNUMV, NUMPOL, CTIME,
     *   TIMEI, CSIDNO, J, SUBARR, FRQSEL, IFR, NODENO, MBDELY, DISP,
     *   DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITING SN TABLE'
         GO TO 980
         END IF
      WRITE (MSGTXT,1100) VER
      CALL MSGWRT (2)
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RLCUV ERROR',I4,' ON ',A)
 1100 FORMAT ('Wrote SN table version',I5)
      END
      SUBROUTINE RLCSEL (IFNO, NC, CHNS, IRET)
C-----------------------------------------------------------------------
C   RLCSEL will read a multi source data set into a scratch file.
C   Editing and calibration may be applied.  It then adds a model to
C   that file in adjacent channels.  It rereads the data file getting
C   the phase correction averaged over time and spectral channel.  If
C   there was no NX table, it prepares an integration list on the first
C   pass.  That is used for the time averaging thereafter.
C   Input:
C      IFNO         I    Do this IF now
C      NC           I    Number spectral channels
C      CHNS         I(3,20)   channel selection
C   Input via common:
C      XSOLIN       R    Solution interval (min)
C   Inputs via common /SELCAL/  (Include DSEL.INC)
C      UNAME        C    AIPS name of input file.
C      UCLAS        C    AIPS class of input file.
C      UDISK        R    AIPS disk of input file.
C      USEQ         R    AIPS sequence of input file.
C      SOURCS(30)   C    Name (16 char) of desired cal source
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
C      VCALID       I    Calibrator ID numbers
C   Output:
C      IRET         I    Error code: 0 => OK,
C                        -1 => end of data
C                        >0 => failed, abort process.
C-----------------------------------------------------------------------
      INTEGER   IFNO, NC, CHNS(3,20), IRET
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER IFILE*48
      INTEGER   LUN1, LUN2, LSOU, SAVNRP, SAVLRC, IND, ILENBU, KBIND,
     *   IPTRI, INIO, SAVCR0, NT,IP, JP, IC, PRTLEV, LOOP, VO, BO, NW,
     *   LOPCNT, IIVER, OOVER, PTIME(4), ISCAN
      LOGICAL   T, F
      REAL      DUM, LTIMEP, CTIME, TSCEND, SAVSTK, CATR(256), CHWT
      DOUBLE PRECISION CATD(128), PRR, PII, PRS, PIS, PW, PT, D
      COMPLEX   RLMOD, LRMODS, RLDAT, LRDATS
      INCLUDE 'RLCAL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSCD.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA T, F /.TRUE.,.FALSE./
      DATA VO, BO /0,1/
      DATA LUN1,LUN2 /27,28/
C-----------------------------------------------------------------------
C                                       Setup
      WRITE (MSGTXT,1002) IFNO
      CALL MSGWRT (2)
      PRTLEV = XPRTLV + 0.5
C                                       Get data
      BIF = IFNO
      EIF = IFNO
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy to scratch file.
      IF (NVIS.LE.0) THEN
         CALL UVGET ('CLOS', DUM, DUM, IRET)
         VISDSK = 0
         VISCNO = 0
         IRET = -1
         WRITE (MSGTXT,1100) IFNO
         GO TO 990
         END IF
C                                       do copy
      CALL RLCCOP (VISDSK, VISCNO, BUFF1, JBUFSZ, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Model polarizations
C                                       Create new file with room for
C                                       model.
      CALL COPY (256, CATBLK, SCRCAT)
      SCLREC = LREC
      SCRPRM = NRPARM
      COMPDT = CATBLK(KINAX).EQ.1
      DATDIV = .TRUE.
      IF (COMPDT) THEN
         CALL AXEFND (8, 'WEIGHT  ', SCRCAT(KIPCN), SCRHOL(KHPTP),
     *      WTLOC, IRET)
C                                       Must have this one
         IF ((IRET.NE.0) .OR. (WTLOC.LT.0)) THEN
            IRET = 5
            MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED
     *         DATA'
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
     *   CNOIN, SCRCNO(VMCNO), CATUV, CATBLK, BIF, EIF, FRQSEL,
     *   SFREQS, BUFF1, BUFF2, UBUFF, UBUFF(2049), IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save header values.
      SAVNRP = NRPARM
      SAVLRC = LREC
      SAVCR0 = ICOR0
      SAVSTK = CATR(KRCIC+JLOCS)
      NRPARM = LREC
      LREC = 2 * SAVLRC - SAVNRP
      ICOR0 = 2
      CATR(KRCIC+JLOCS) = ABS (CATR(KRCIC+JLOCS))
      CATD(KDCRV+JLOCS) = 2.0D0
      CALL CATIO ('UPDT', SCRVOL(VMCNO), SCRCNO(VMCNO), CATBLK,
     *   'REST', BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1102) IRET
         GO TO 990
         END IF
C                                       Model computation
      CALL RLCFLX (IFNO, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       find corrections
      LTIMEP = -1.0E10
      LSOU = -10
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
C                                       Read data
      TSCEND = -1.0E10
      LOPCNT = 0
      ISCAN = 0
      DO 200 LOOP = 1,NVIS+1
C                                       Read vis. record.
         CALL UVDISK ('READ', LUN1, IND, BUFF2, INIO, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1124) IRET
            GO TO 990
            END IF
         IPTRI = KBIND
         CTIME = BUFF2(IPTRI+ILOCT)
C                                       See if average finished
         IF ((CTIME.GT.TSCEND) .OR. (INIO.LE.0)) THEN
C                                       Average
            IF (TSCEND.GT.0.0) THEN
               IF (PW.GT.0.0) THEN
                  PRR = PRR / PW
                  PII = PII / PW
                  PRS = PRS / PW - PRR * PRR
                  PIS = PIS / PW - PII * PII
                  PRS = SQRT (MAX (0.0D0, PRS))
                  PIS = SQRT (MAX (0.0D0, PIS))
                  PCORR(IFNO,ISCAN) = ATAN2 (PII, PRR) * RAD2DG
                  IF (PRTLEV.GT.0) THEN
                     CTIME = PT / NT
                     CALL TODHMS (CTIME, PTIME)
                     PT = RAD2DG * SQRT ((PII*PRS)**2 +
     *                  (PRR*PIS)**2) / (PRR**2+PII**2)
                     WRITE (MSGTXT,1015) IFNO, PTIME, PCORR(IFNO,ISCAN),
     *                  PT, NW
                     CALL MSGWRT (3)
                     END IF
                  END IF
               END IF
            IF (INIO.LE.0) GO TO 210
C                                       new sub-scan
            ISCAN = ISCAN + 1
            NT = 0
            NW = 0
            PW = 0.0D0
            PRR = 0.0D0
            PII = 0.0D0
            PRS = 0.0D0
            PIS = 0.0D0
            PT = 0.0D0
            TSCEND = TSCAN(2,ISCAN)
            END IF
C                                       add in
         PT = PT + CTIME
         NT = NT + 1
         IP = IPTRI + SAVNRP - 7
         JP = IPTRI + NRPARM - 7
         DO 50 IC = 1,NC
            IP = IP + 6
            JP = JP + 6
            CHWT = MIN (BUFF2(IP+3), BUFF2(IP+6))
            CALL WANTCH (CHNS, IC, CHWT)
            IF (CHWT.GT.0.0) THEN
               RLMOD = CMPLX (BUFF2(JP+1)-BUFF2(JP+5),
     *            BUFF2(JP+2)+BUFF2(JP+4))
               LRMODS = CMPLX (BUFF2(JP+1)+BUFF2(JP+5),
     *            -BUFF2(JP+2)+BUFF2(JP+4))
               RLMOD = RLMOD / CABS (RLMOD)
               LRMODS = LRMODS / CABS (LRMODS)
               RLDAT = CMPLX (BUFF2(IP+1), BUFF2(IP+2))
               LRDATS = CMPLX (BUFF2(IP+4), -BUFF2(IP+5))
               RLDAT = RLDAT / CABS (RLDAT)
               LRDATS = LRDATS / CABS (LRDATS)
               RLDAT = RLDAT / RLMOD
               LRDATS = LRDATS / LRMODS
               PW = PW + CHWT
               NW = NW + 1
               D = REAL (RLDAT)
               PRR = PRR + D * CHWT
               PRS = PRS + D * D * CHWT
               D = AIMAG (RLDAT)
               PII = PII + D * CHWT
               PIS = PIS + D * D * CHWT
               PW = PW + CHWT
               NW = NW + 1
               D = REAL (LRDATS)
               PRR = PRR + D * CHWT
               PRS = PRS + D * D * CHWT
               D = AIMAG (LRDATS)
               PII = PII + D * CHWT
               PIS = PIS + D * D * CHWT
               END IF
 50         CONTINUE
 200     CONTINUE
C                                       Force new scratch files next
C                                       pass.
 210  VISDSK = 0
      VISCNO = 0
      VMCNO = 0
C                                       Close scratch files
      CALL ZCLOSE (LUN1, IND, IRET)
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
 1002 FORMAT ('Processing IF number',I3)
 1015 FORMAT ('IF',I3,' at',I3,'/',2(I2.2,':'),I2.2,F8.2,' +-',F6.2,
     *   '   Nsamp',I11)
 1100 FORMAT ('RLCSEL: NO DATA SELECTED FOR IF',I3)
 1102 FORMAT ('RLCSEL: ERROR ',I5,' UPDATING SCRATCH FILE CATBLK')
 1120 FORMAT ('RLCSEL: ZOPEN ERROR ',I3,' OPENING SCRATCH FILE')
 1121 FORMAT ('RLCSEL: UVINIT ERROR ',I3,' INITIALIZING SCRATCH FILE')
 1124 FORMAT ('RLCSEL: UVDISK ERROR ',I3,' READING SCRATCH FILE')
 1300 FORMAT ('RLCSEL: ZCLOSE ERROR ',I3,' CLOSING SCRATCH FILE')
      END
      SUBROUTINE RLCFLX (IFNO, IRET)
C-----------------------------------------------------------------------
C   RLCFLX includes the CLEAN model visibilities into the data.
C   The model visibilities computed are in the form of I,Q,U and V,
C   one IF at a time in and uv data file created by RLCSEL with space
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
C     VCALID    I    Calibrator ID numbers
C   Output:
C     CNOIN2    I    CLEAN file catalog number.
C     IRET      I    Return code, 0 => ok, otherwise not.
C-----------------------------------------------------------------------
      INTEGER   IFNO, IRET
C
      CHARACTER CLASS*6, STAT*4, CHSTOK(4)*1, UTYPE*2
      INTEGER   MODEL, METHOD, ISTOKE, DISKO, ISCR, CHAN, NCHAN, I,
     *   IROUND
      LOGICAL   DOMSG, DOSUM, F, WASOME
      INCLUDE 'RLCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INTEGER   BITER(MAXFLD)
      INCLUDE 'INCS:DSEL.INC'
      DATA DOMSG, DOSUM, F /.FALSE.,.FALSE.,.FALSE./
      DATA CHSTOK /'I','Q','U','V'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Set model and method
C                                       (Options limited by sort order.)
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
      IF (CMOD.EQ.'SUBI') MODEL = 3
      METHOD = 0
      IF (CMETH.EQ.'DFT ') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
      DOPTMD = (NAME2.EQ.' ') .AND. (CLAS2.EQ.' ')
C                                       ADD the models.
      FACGRD(1) = -1.0
      FACGRD(2) = 1.0
C                                       Can have only 1 source here with
C                                       known polarization.
      PTRAOF = XSMOD(5)
      PTDCOF = XSMOD(6)
C                                       Loop over four Stokes.
      DO 200 ISTOKE = 2,3
C                                       pointers for model routines
         NSTOK = 1
         KSTOK = ISTOKE
         VOFF = (ISTOKE-2) * 3
C                                       Point model only
         CLNMOD = .NOT.DOPTMD
         IF (DOPTMD) THEN
            PTFLX = VFLUX(ISTOKE,IFNO)
         ELSE
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
C                                       Get correct class
            CLASS = CLAS2
            CLASS(1:1) = CHSTOK(ISTOKE)(1:1)
            CNOIN2 = 1
            UTYPE = 'MA'
            CALL CATDIR ('SRCH', DISK2, CNOIN2, NAME2, CLASS, SEQ2,
     *         UTYPE, NLUSER, STAT, BUFF1, IRET)
C                                       can't find - not allowed
            IF (IRET.NE.0) THEN
               MSGTXT = 'CANNOT FIND MODEL IMAGE'
               CALL MSGWRT (8)
               GO TO 999
               END IF
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
C              CALL FACSET (DISKIN, CNOIN, IFNO, SOUWAN(1), MODEL,
C    *            FACGRD(1), IRET)
C              IF (IRET.NE.0) GO TO 999
               END IF
            CNOIN2 = CCCNO(1)
            END IF
C                                       Compute model.
         DISKO = VMDSK
         ISCR = VMCNO
         CHAN = 1
         NUMIF = 1
         IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
         NCHAN = CATBLK(KINAX+JLOCF)
         CALL UVMSUB (DISKO, ISCR, DISKO, ISCR, 0, MODEL, METHOD,
     *      CHAN, NCHAN, DOSUM, DOMSG, CATBLK, JBUFSZ, FRQSEL, BUFF1,
     *      BUFF2, UBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL UNSETG (BUFF2)
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RLCHIS
C-----------------------------------------------------------------------
C   RLCHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12, HILINE*72
      INTEGER   LUN, IERR, I, ITIME(3), DATE(3), K, J
      LOGICAL   T
      INCLUDE 'RLCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /28/
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
C                                       Add selection/calibration
C                                       criteria:
C                                       Sources
      WRITE (HILINE,3000) TSKNAM, I, CSNAME, VCALID
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Timerange
      CALL HITIME (TSTART, TEND, LUN, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Subarray
      WRITE (HILINE,3013) TSKNAM, SUBARR
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Flag table
      WRITE (HILINE,3014) TSKNAM, FGVER
      IF (DOFLAG) CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Calibration info
C                                       Gain tables
      IF (DOCAL) THEN
         WRITE (HILINE,3019) TSKNAM, CLUSE
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
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
C                                       Point source model
      IF (XSMOD(1).GE.1.0E-5) THEN
         WRITE (HILINE,2020) TSKNAM, XSMOD(2), XSMOD(3)
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2021) TSKNAM, XSMOD(5), XSMOD(6)
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       spectral index
      IF (XSPECT(1,1).NE.0.0) THEN
         WRITE (HILINE,2030) TSKNAM, (XSPECT(J,1), J = 2,3),
     *      VCALID
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Solution interval
      XSOLIN = XSOLIN * (24.0 * 60.0)
      WRITE (HILINE,2022) TSKNAM, XSOLIN
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       IFs.
      BIF = XBIF + 0.01
      EIF = XEIF + 0.01
      WRITE (HILINE,2025) TSKNAM, BIF, EIF
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
 2020 FORMAT (A6,'PMODEL =',F9.4,',',F9.4,' / Q, U fluxes')
 2021 FORMAT (A6,'        ',2(F9.3,','),F9.3,' /Poln. model')
 2022 FORMAT (A6,'SOLINT =',F10.3,' /Solution interval (min)')
 2025 FORMAT (A6,'BIF =',I3,', EIF =',I4,' /IFs processed')
 2030 FORMAT (A6,'XSPECT=',2F7.3,'  / Q, U spectral index source',I3)
 3000 FORMAT (A6,'CALSOUR(',I2,') = ''',A,''' ID=',I4,'  / cal source')
 3013 FORMAT (A6,'SUBARRAY =',I4)
 3014 FORMAT (A6,'FLAGVER =',I3,' /Flagging table used')
 3019 FORMAT (A6,'GAINUSE = ',I3,' /CL table applied')
 3050 FORMAT (A6,'/ Chns used: Start, Stop, Inc ',2I5,I4,'  IF=',I3)
      END
      SUBROUTINE RLCCOP (DISK, CNOSCR, BUFFER, BUFSZ, IRET)
C-----------------------------------------------------------------------
C   Routine to copy selected data from one data file to another
C   optionally applying calibration and editing information.
C   The input file should have been opened with UVGET.
C     Note: UVGET returns the information necessary to catalog the
C   output file.  The output file will be compressed if necessary at
C   completion of RLCCOP.
C   Inputs:
C      DISK     I       Disk number for catalogd output file.
C                       If .LE. 0 then the output file is a /CFILES/
C                       scratch file.
C      BUFFER   R(*)    Work buffer for writing.
C      BUFSZ    I       Size of BUFFER in bytes.
C
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
C   Output:
C      IRET     I       Error code: 0 => OK,
C                          > 0 => failed, abort process.
C   Usage notes:
C   (1) UVGET with OPCODE='INIT' MUST be called before RLCCOP to setup
C       for calibration, editing and data translation.  If an output
C       cataloged file is to be created this should be done after the
C       call to UVGET.
C   (2) Uses AIPS LUNs 24 and 27
C-----------------------------------------------------------------------
      INTEGER   DISK, CNOSCR, BUFSZ, IRET
      REAL      BUFFER(*)
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER NAME*48
      INTEGER   VOL, LUN, FIND, BIND, LENBU, NIO, CNO, BO, VO, I,
     *   XCOUNT, ISIZE, JERR
      LOGICAL   DONX
      REAL      STIME, UTIME, ETIME
      PARAMETER (LUN = 24)
      INCLUDE 'RLCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA BO, VO /1,0/
C-----------------------------------------------------------------------
      IRET = 0
      LENBU = 1
      DONX = NSCANS.LE.0
      IF ((DONX) .AND. (XSOLIN.LE.0.0)) XSOLIN = 10.0 / (24.0 * 60.0)
      STIME = -100.
      ETIME = -101.
C                                       Create output file if necessary
      IF ((DISK.EQ.0) .AND. (CNOSCR.EQ.0)) THEN
C                                       Determine size.
         CALL UVSIZE (LREC, NVIS, ISIZE)
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
      CALL UVINIT ('WRIT', LUN, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *    BUFFER, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         GO TO 990
         END IF
      DO 100 I = 1,NVIS
         XCOUNT = I
C                                       Read old.
         CALL UVGET ('READ', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
         IF (IRET.LT.0) GO TO 110
         IF (IRET.NE.0) GO TO 999
         IF (DONX) THEN
            UTIME = BUFFER(BIND+ILOCT)
            IF (UTIME.GT.ETIME) THEN
               NSCANS = NSCANS + 1
               ETIME = UTIME + XSOLIN
               TSCAN(1,NSCANS) = UTIME
               END IF
            TSCAN(2,NSCANS) = MAX (UTIME, TSCAN(2,NSCANS))
            END IF
C                                       Write new
         NIO = 1
         CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1008) IRET
            GO TO 990
            END IF
 100     CONTINUE
C                                       Check if last call to UVGET
C                                       returned valid data.
 110  IF (IRET.LT.0) XCOUNT = XCOUNT - 1
      NIO = 0
      CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1008) IRET
         GO TO 990
         END IF
C                                       Close input
      CALL UVGET ('CLOS', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
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
      CALL ZCLOSE (LUN, FIND, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RLCCOP: TOO LITTLE DISK SPACE FOR SCRATCH FILE')
 1001 FORMAT ('RLCCOP: ERROR ',I5,' CREATING SCRATCH FILE')
 1002 FORMAT ('RLCCOP: ERROR ',I5,' UPDATING SCRATCH FILE CATBLK')
 1003 FORMAT ('RLCCOP: ERROR ',I5,' OPENING OUTPUT FILE')
 1005 FORMAT ('RLCCOP: ERROR ',I5,' INIT. OUTPUT FILE')
 1008 FORMAT ('RLCCOP: ERROR ',I5,' WRITING OUTPUT FILE')
      END
